(** Entry point for the whatwhat executable.
    Use [whatwhat --help] for usage instructions.
    *)

open Whatwhat
open Cmdliner

(* ------------------------------- *)
(* ------- whatwhat open --------- *)

let ww_open (issue_num : int) : unit =
  let uri =
    String.concat
      "/"
      [ "https://github.com"
      ; Config.get_github_repo_owner ()
      ; Config.get_github_repo_name ()
      ; "issues"
      ; string_of_int issue_num
      ]
  in
  Filename.quote_command "open" [ uri ] |> Sys.command |> exit
;;

let issue_num_arg : int Term.t =
  Arg.(
    required & pos 0 (some int) None & info ~docv:"NUMBER" ~doc:"Issue number to open" [])
;;

let ww_open_cmd : unit Cmd.t =
  Cmd.v
    (Cmd.info "open" ~doc:"Open a GitHub issue in a browser")
    Term.(const ww_open $ issue_num_arg)
;;

(* -------------------------------------- *)
(* --- whatwhat export-{team,project} --- *)

type fc_export =
  | Project
  | Team

let show_fc_export = function
  | Project -> "project"
  | Team -> "team"
;;

let ww_export
  (export : fc_export)
  (start_date_in : CalendarLib.Date.t)
  (end_date_in : CalendarLib.Date.t)
  (output_file : string option)
  : unit
  =
  (* Determine start and end points. We export full weeks only, so the start
     must be a Monday and the end a Sunday. This mimics the interface provided
     on the Forecast website. *)
  let open CalendarLib.Date in
  let start_date = Utils.rollback_week start_date_in in
  let end_date = Utils.rollforward_week ~with_weekend:true end_date_in in
  if start_date >= end_date then failwith "Start date cannot be before end date!";

  (* Calculate the schedule *)
  let csv =
    match export with
    | Project -> ForecastExport.export_project_schedule ~start_date ~end_date
    | Team -> ForecastExport.export_team_schedule ~start_date ~end_date
  in

  (* Determine output file *)
  let output =
    match output_file with
    | Some "" ->
      let start_str = CalendarLib.Printer.Date.sprint "%Y-%m-%d" start_date in
      let end_str = CalendarLib.Printer.Date.sprint "%Y-%m-%d" end_date in
      Some
        (Printf.sprintf
           "forecast-%s-export-from-%s-to-%s.csv"
           (show_fc_export export)
           start_str
           end_str)
    | Some f -> Some f
    | None -> None
  in

  (* Print some useful info *)
  let module ANSI = ANSITerminal in
  ANSI.eprintf [ Bold ] "Forecast %s export\n" (show_fc_export export);
  ANSI.eprintf [ Bold ] "-----------------------\n";
  ANSI.eprintf [ Bold ] "Start date      ";
  ANSI.eprintf [] "%s\n" (CalendarLib.Printer.Date.to_string start_date);
  ANSI.eprintf [ Bold ] "End date        ";
  ANSI.eprintf [] "%s\n" (CalendarLib.Printer.Date.to_string end_date);
  ANSI.eprintf [ Bold ] "Output location ";
  ANSI.eprintf [] "%s\n" (Option.value output ~default:"standard output");

  (match output with
   | Some fname -> Csv.save fname csv
   | None -> Csv.print csv);

  Printf.eprintf "Done.\n"
;;

let start_date_arg : CalendarLib.Date.t Term.t =
  let default_start_date = Utils.default_start_date () in
  let parser s =
    match s with
    | "" -> Ok default_start_date
    | "_" -> Ok default_start_date
    | s -> Utils.parse_date s
  in
  let printer f d = Format.pp_print_string f (CalendarLib.Printer.Date.to_string d) in
  Arg.(
    value
    & pos 0 (conv (parser, printer)) default_start_date
    & info
        ~docv:"START"
        ~doc:
          "Start date. Defaults to the 1st of the previous month. If you want to specify \
           a custom end date but leave the start date as the default, pass a single \
           underscore."
        [])
;;

let end_date_arg : CalendarLib.Date.t Term.t =
  let default_end_date = Utils.default_end_date () in
  let parser s =
    match s with
    | "" -> Ok default_end_date
    | "_" -> Ok default_end_date
    | s -> Utils.parse_date s
  in
  let printer f d = Format.pp_print_string f (CalendarLib.Printer.Date.to_string d) in
  Arg.(
    value
    & pos 1 (conv (parser, printer)) default_end_date
    & info ~docv:"END" ~doc:"End date. Defaults to the last day of the next month." [])
;;

(** Returns [None] if the [-o] option was completely absent, [Some ""] if the
    [-o] option was provided without an argument, or [Some s] if the [-o] option
    was provided with the argument [s]. *)
let output_file_arg : string option Term.t =
  Arg.(
    value
    & opt ~vopt:(Some "") (some string) None
    & info
        ~docv:"OUTPUT"
        ~doc:"File to output CSV to. By default, the CSV is printed to standard output."
        [ "o"; "output" ])
;;

let ww_export_project_cmd : unit Cmd.t =
  Cmd.v
    (Cmd.info "export-project" ~doc:"Export Forecast project CSVs")
    Term.(const (ww_export Project) $ start_date_arg $ end_date_arg $ output_file_arg)
;;

let ww_export_team_cmd : unit Cmd.t =
  Cmd.v
    (Cmd.info "export-team" ~doc:"Export Forecast team CSVs")
    Term.(const (ww_export Team) $ start_date_arg $ end_date_arg $ output_file_arg)
;;

(* ------------------------------- *)
(* ---------- whatwhat ----------- *)

type project_subset =
  | AllProjects
  | SelectedColumnsOnly
  | Specific of int list

let ww_main notify no_color quiet verbose codes_without codes_only project_subset =
  (* Use color if output is to a terminal and --no-color flag was absent. *)
  let use_color = Unix.isatty Unix.stdout && not no_color in

  (* Decide which codes to show *)
  let restrict_codes =
    match codes_only, codes_without with
    | _ :: _, _ -> Log.Only codes_only
    | _, _ :: _ -> Log.Without codes_without
    | _ -> Log.All
  in

  try
    let open CalendarLib.Date in
    let start_date = make 2016 1 1 in
    let end_date = add (today ()) (Period.year 1) in
    let people, projects, assignments, github_project_numbers =
      Schedule.get_the_schedule ~start_date ~end_date
    in
    print_endline "Whatwhat downloaded:";
    Printf.printf "%d people; " (List.length people);
    Printf.printf "%d projects; and " (Domain.IntMap.cardinal projects);
    Printf.printf "%d assignments\n\n" (List.length assignments);

    let restrict_issues =
      match project_subset with
      | AllProjects -> None
      | SelectedColumnsOnly -> Some github_project_numbers
      | Specific ps -> Some ps
    in

    (* Print output *)
    if not quiet
    then Log.pretty_print ~use_color ~verbose ~restrict_codes ~restrict_issues;

    (* Send notifications if requested *)
    match notify with
    | Notify.NoTarget -> print_endline "No notifications requested."
    | Notify.Github -> Notify.post_github ~verbose ~restrict_codes ~restrict_issues
    | Notify.Slack -> print_endline "CATCH: this would post reports to Slack."
    | Notify.All -> print_endline "CATCH: this would post reports to everywhere!"
  with
  | Failure msg ->
    let open ANSITerminal in
    Log.pretty_print ~use_color ~verbose ~restrict_codes ~restrict_issues:None;
    Pretty.prerr ~use_color [ Bold; Foreground Red ] "Fatal error: ";
    Printf.eprintf "%s\n" msg;
    exit Cmd.Exit.internal_error (* Defined as 125. *)
;;

let notify_arg =
  let tgs =
    Arg.enum
      [ "github", Notify.Github
      ; "slack", Notify.Slack
      ; "all", Notify.All
      ; "none", Notify.NoTarget
      ]
  in
  let doc =
    "Where to send notifications. $(docv) may be $(b,github), $(b,slack), $(b,all), or \
     $(b,none). Note that terminal output is always enabled. This means that you can \
     (for example) run `whatwhat OPTS` first as a dry run to see which errors are being \
     logged, and then run `whatwhat OPTS --notify github` to post GitHub comments about \
     those errors."
  in
  Arg.(value & opt tgs Notify.NoTarget & info [ "n"; "notify" ] ~docv:"NOTIFY" ~doc)
;;

let no_color_arg =
  Arg.(
    value
    & flag
    & info
        [ "C"; "no-color" ]
        ~doc:
          "Disable colored output. This is automatically disabled if the output is not \
           to a terminal.")
;;

let quiet_arg =
  Arg.(value & flag & info [ "q"; "quiet" ] ~doc:"Turn off all notifications.")
;;

let verbose_arg =
  Term.app
    (Term.const List.length)
    Arg.(
      value
      & flag_all
      & info
          [ "v"; "verbose" ]
          ~doc:
            "Increase verbosity. -v shows INFO messages, and -vv additionally shows \
             DEBUG messages.")
;;

let parse_code c =
  let e = Tyre.((char 'e' *> int) --> fun i -> Log.Error' i) in
  let w = Tyre.((char 'w' *> int) --> fun i -> Log.Warning i) in
  let code_regex = Tyre.route [ e; w ] in
  match Tyre.exec code_regex (String.lowercase_ascii c) with
  | Ok code -> Some code
  | Error _ -> None
;;

let codes_without_arg =
  Term.app
    (Term.const (List.filter_map parse_code))
    Arg.(
      value
      & opt (list string) []
      & info
          [ "codes-without" ]
          ~doc:"Suppress specific error codes. See --only for more explanation."
          ~docv:"CODES")
;;

let codes_only_arg =
  Term.app
    (Term.const (List.filter_map parse_code))
    Arg.(
      value
      & opt (list string) []
      & info
          [ "codes-only" ]
          ~doc:
            "Only show specific error codes. $(docv) should be passed as a \
             comma-separated list, so for example, use $(opt)$(b,=E3001,W3001) to only \
             show those two types of messages. The argument is case-insensitive. Note \
             that $(opt) takes precedence over --codes-without."
          ~docv:"CODES")
;;

let projects_arg =
  let parser s =
    match String.lowercase_ascii s with
    | "all" -> Ok AllProjects
    | "github" -> Ok SelectedColumnsOnly
    | s ->
      let reg = Tyre.(compile (int <&> rep (char ',' *> int))) in
      (match Tyre.exec reg s with
       | Ok (first, last) -> Ok (Specific (first :: List.of_seq last))
       | Error _ -> Error (`Msg "Invalid value"))
  in
  let show_selected_project_arg = function
    | AllProjects -> "all"
    | SelectedColumnsOnly -> "github"
    | Specific issues -> String.concat "," (List.map string_of_int issues)
  in
  let printer f i = Format.pp_print_string f (show_selected_project_arg i) in
  Arg.(
    value
    & opt (conv (parser, printer)) SelectedColumnsOnly
    & info
        [ "projects" ]
        ~doc:
          "Decide which projects to display errors for in the output, and notify for (if \
           requested). Permitted values are: $(b,all) (runs over all projects in \
           Forecast), $(b,github) (default; runs over all issues in the specified GitHub \
           columns), or a comma-separated list of issue numbers."
        ~docv:"PROJECTS")
;;

let ww_main_term : unit Term.t =
  Term.(
    const ww_main
    $ notify_arg
    $ no_color_arg
    $ quiet_arg
    $ verbose_arg
    $ codes_without_arg
    $ codes_only_arg
    $ projects_arg)
;;

(* ------------------------------- *)
(* ------ whatwhat project ------- *)

let ww_project project_name_or_number no_color =
  (* Use color if output is to a terminal and --no-color flag was absent. *)
  let use_color = Unix.isatty Unix.stdout && not no_color in
  ignore use_color;

  let open CalendarLib.Date in
  let start_date = make 2016 1 1 in
  let end_date = add (today ()) (Period.year 1) in
  let _, projects, assignments, _ = Schedule.get_the_schedule ~start_date ~end_date in

  match project_name_or_number with
  (* Searched for project number *)
  | Either.Left n ->
    (match Domain.IntMap.find_opt n projects with
     | Some prj -> Project.print ~use_color prj assignments
     | None -> Printf.printf "No project with number %d was found.\n" n)
  (* Searched for project name *)
  | Either.Right s ->
    let matched_projects =
      projects
      |> Domain.IntMap.to_seq
      |> List.of_seq
      |> List.filter (fun (_, (p : Domain.project)) ->
           Utils.contains ~case_sensitive:false p.name s)
      |> List.map snd
    in
    (match matched_projects with
     | [ prj ] -> Project.print ~use_color prj assignments
     | [] -> Printf.printf "No project with '%s' in its name was found.\n" s
     | _ ->
       Printf.printf "Multiple projects were found matching the string '%s':\n" s;
       List.iter
         (fun (p : Domain.project) -> Printf.printf "#%-5d %s\n" p.number p.name)
         matched_projects)
;;

let parse_int_or_string s =
  match int_of_string_opt s with
  | Some i -> Either.Left i
  | None -> Either.Right s
;;

let project_arg =
  let doc = "Identifier of a project. Can either be an issue number or issue title." in
  Term.app
    (Term.const parse_int_or_string)
    Arg.(required & pos 0 (some string) None & info ~docv:"PROJECT" ~doc [])
;;

let ww_project_cmd : unit Cmd.t =
  Cmd.v
    (Cmd.info "project" ~doc:"Provide an overview of a project.")
    Term.(const ww_project $ project_arg $ no_color_arg)
;;

(* ------------------------------- *)
(* ------ whatwhat person -------- *)

let ww_person person no_color =
  (* Use color if output is to a terminal and --no-color flag was absent. *)
  let use_color = Unix.isatty Unix.stdout && not no_color in
  ignore use_color;

  let open CalendarLib.Date in
  let start_date = make 2016 1 1 in
  let end_date = add (today ()) (Period.year 1) in
  let people, _, assignments, _ = Schedule.get_the_schedule ~start_date ~end_date in

  let matched_people =
    List.filter
      (fun (p : Domain.person) ->
        Utils.contains ~case_sensitive:false p.full_name person
        || Utils.contains ~case_sensitive:false p.email person
        ||
        match p.github_handle with
        | None -> false
        | Some s -> Utils.contains ~case_sensitive:false s person)
      people
  in
  match matched_people with
  | [] -> Printf.printf "No person with the name '%s' name was found.\n" person
  | [ p ] -> Person.print ~use_color p assignments
  | ps ->
    Printf.printf "Multiple people were found matching the string '%s':\n" person;
    List.iter
      (fun (p : Domain.person) ->
        match p.github_handle with
        | None -> print_endline p.full_name
        | Some s -> Printf.printf "%s (@%s)\n" p.full_name s)
      ps
;;

let person_arg =
  let doc = "Full name, Turing username, or GitHub username of a person." in
  Arg.(required & pos 0 (some string) None & info ~docv:"PERSON" ~doc [])
;;

let ww_person_cmd : unit Cmd.t =
  Cmd.v
    (Cmd.info "person" ~doc:"Provide an overview of a person.")
    Term.(const ww_person $ person_arg $ no_color_arg)
;;

(* ------------------------------- *)
(* ------- whatwhat test --------- *)
(* - Use this for experimenting! - *)

let post_github_comment issue user repo post_body =
  let uri =
    String.concat
      "/"
      [ Config.github_url
      ; "repos"
      ; user
      ; repo
      ; "issues"
      ; string_of_int issue
      ; "comments"
      ]
  in
  let body = `Assoc [ "body", `String post_body ] |> Yojson.Basic.to_string in
  ignore @@ GithubRaw.run_github_query ~as_bot:true ~http_method:POST ~body uri
;;

let ww_test () =
  let open Yojson.Basic in
  let params = [ "participating", [ "true" ] ] in
  let resp =
    GithubRaw.run_github_query ~as_bot:true ~params "https://api.github.com/notifications"
  in
  let resp_list = resp |> Util.to_list in
  match resp_list with
  | [] -> print_endline "No new notifications."
  | _ ->
    let to_reply_to =
      List.filter_map
        (fun json ->
          if json
             |> Util.member "subject"
             |> Util.member "type"
             |> Util.to_string
             = "Issue"
          then (
            let url =
              json |> Util.member "subject" |> Util.member "url" |> Util.to_string
            in
            let issue_number =
              url |> String.split_on_char '/' |> List.rev |> List.hd |> int_of_string
            in
            let user =
              json
              |> Util.member "repository"
              |> Util.member "owner"
              |> Util.member "login"
              |> Util.to_string
            in
            let name =
              json |> Util.member "repository" |> Util.member "name" |> Util.to_string
            in
            let subscription_url =
              json |> Util.member "subscription_url" |> Util.to_string
            in
            Some (user, name, issue_number, subscription_url))
          else None)
        resp_list
    in
    List.iter
      (fun (user, repo, n, _) ->
        Printf.printf
          "Hello, you summoned me in issue #%d of repository %s/%s!\n"
          n
          user
          repo;
        post_github_comment
          n
          user
          repo
          (Printf.sprintf
             "Hello, you summoned me in issue #%d of repository %s/%s!\n"
             n
             user
             repo))
      to_reply_to;
    (* Mark all notifications as read *)
    ignore
    @@ GithubRaw.run_github_query
         ~as_bot:true
         ~http_method:PUT
         ~body:"{\"read\": true}"
         "https://api.github.com/notifications";
    (* Delete subscription to stop other comments from triggering it *)
    List.iter
      (fun (_, _, _, subsc_url) ->
        ignore @@ GithubRaw.run_github_query ~as_bot:true ~http_method:DELETE subsc_url)
      to_reply_to
;;

let ww_test_cmd : unit Cmd.t =
  Cmd.v
    (Cmd.info "test" ~doc:"Command to be freely used for internal testing purposes.")
    (* The homomorphism law for applicative functors suggests that
          [const ww_test $ const ()]
       should be equivalent to 
          [const (ww_test ())],
        but because of side effects (and eager evaluation) this isn't true: the
        latter always evaluates [ww_test ()] whereas the former doesn't. A case
        where lack of purity makes it harder to reason about the behaviour of a
        programme! *)
    Term.(const ww_test $ const ())
;;

(* ------------------------------- *)
(* --- putting it all together --- *)

let cmd : unit Cmd.t =
  Cmd.group
    ~default:ww_main_term
    (Cmd.info "whatwhat" ~doc:"Report current project status")
    [ ww_export_project_cmd
    ; ww_export_team_cmd
    ; ww_open_cmd
    ; ww_project_cmd
    ; ww_person_cmd
    ; ww_test_cmd
    ]
;;

let () = exit (Cmd.eval cmd)
