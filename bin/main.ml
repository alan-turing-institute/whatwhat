(** Entry point for the whatwhat executable.
    Use [whatwhat --help] for usage instructions. *)

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
  let default_start_date = Utils.default_export_start_date () in
  let parser s =
    match s with
    | "" -> Ok default_start_date
    | "_" -> Ok default_start_date
    | s -> Utils.date_of_string ~lax:true s
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
  let default_end_date = Utils.default_export_end_date () in
  let parser s =
    match s with
    | "" -> Ok default_end_date
    | "_" -> Ok default_end_date
    | s -> Utils.date_of_string ~lax:true s
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

let get_start_and_end_dates (n_months : int) =
  let open CalendarLib.Date in
  let start_date = rem (today ()) (Period.month n_months) in
  let end_date = add start_date (Period.month n_months) in
  start_date, end_date
;;

type project_subset =
  | AllProjects
  | Specific of int list

let ww_main
  notify
  no_color
  quiet
  verbose
  codes_without
  codes_only
  project_subset
  months_around
  =
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
    let start_date, end_date = get_start_and_end_dates months_around in
    let people, projects, assignments = Schedule.get_the_schedule ~start_date ~end_date in
    print_endline "Whatwhat downloaded:";
    Printf.printf "%d people; " (List.length people);
    Printf.printf "%d projects; and " (Domain.IntMap.cardinal projects);
    Printf.printf "%d assignments\n\n" (List.length assignments);

    let restrict_issues =
      match project_subset with
      | AllProjects -> None
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
    flush stdout;
    Pretty.prerr ~use_color [ Bold; Foreground Red ] "Fatal error: ";
    Printf.eprintf "%s\n" msg;
    exit Cmd.Exit.internal_error
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
    | s ->
      let reg = Tyre.(compile (int <&> rep (char ',' *> int))) in
      (match Tyre.exec reg s with
       | Ok (first, last) -> Ok (Specific (first :: List.of_seq last))
       | Error _ -> Error (`Msg "Invalid value"))
  in
  let show_selected_project_arg = function
    | AllProjects -> "all"
    | Specific issues -> String.concat "," (List.map string_of_int issues)
  in
  let printer f i = Format.pp_print_string f (show_selected_project_arg i) in
  Arg.(
    value
    & opt (conv (parser, printer)) AllProjects
    & info
        [ "projects" ]
        ~doc:
          "Decide which projects to display errors for in the output, and notify for (if \
           requested). Permitted values are: $(b,all) (runs over all projects in \
           Forecast with assignments within the given time period), or a\n\
          \           comma-separated list of issue numbers."
        ~docv:"PROJECTS")
;;

let months_lookahead_behind_arg : int Term.t =
  let doc =
    "Number of months to look ahead and behind when fetching the schedule. Only projects \
     with assignments within this time period will be shown. Increase this if you are \
     particularly interested in projects from a long time ago."
  in
  Arg.(value & opt int 6 & info [ "m"; "months" ] ~docv:"MONTHS" ~doc)
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
    $ projects_arg
    $ months_lookahead_behind_arg)
;;

(* ------------------------------- *)
(* ------ whatwhat project ------- *)

let ww_project project_name_or_number no_color months_around =
  (* Use color if output is to a terminal and --no-color flag was absent. *)
  let use_color = Unix.isatty Unix.stdout && not no_color in
  ignore use_color;

  let start_date, end_date = get_start_and_end_dates months_around in
  let people, projects, assignments = Schedule.get_the_schedule ~start_date ~end_date in

  match project_name_or_number with
  (* Searched for project number *)
  | Either.Left n ->
    (match
       Domain.IntMap.filter (fun _ p -> p.Domain.number = n) projects
       |> Domain.IntMap.bindings
       |> List.map snd
     with
     | [ prj ] -> Project.print ~use_color prj people assignments
     | [] -> Printf.printf "No project with number %d was found.\n" n
     | prjs ->
       Pretty.prout ~use_color [ Bold; Foreground Yellow ] "[WARNING] ";
       Printf.printf
         "Multiple Forecast projects were found with number %d; displaying only the first.\n"
         n;
       Project.print ~use_color (List.hd prjs) people assignments)
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
     | [ prj ] -> Project.print ~use_color prj people assignments
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
    Term.(const ww_project $ project_arg $ no_color_arg $ months_lookahead_behind_arg)
;;

(* ------------------------------- *)
(* ------ whatwhat person -------- *)

let ww_person person no_color months_around =
  (* Use color if output is to a terminal and --no-color flag was absent. *)
  let use_color = Unix.isatty Unix.stdout && not no_color in
  ignore use_color;

  let start_date, end_date = get_start_and_end_dates months_around in
  let people, projects, assignments = Schedule.get_the_schedule ~start_date ~end_date in

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
  | [ p ] -> Person.print ~use_color p projects assignments
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
    Term.(const ww_person $ person_arg $ no_color_arg $ months_lookahead_behind_arg)
;;

(* ------------------------------- *)
(* ----- whatwhat slack-bot ------- *)

let ww_slack_bot () = Slack.run_bot ()

let ww_slack_bot_cmd : unit Cmd.t =
  Cmd.v
    (Cmd.info "slack-bot" ~doc:"Run the Slack bot.")
    Term.(const ww_slack_bot $ const ())
;;

(*----------------------------------*)
(*----- whatwhat dump-users ------- *)

let ww_dump_users () =
  (* Configuring the number of months seems overkill for this *)
  let start_date, end_date = get_start_and_end_dates 6 in
  let people, _, _ = Schedule.get_the_schedule ~start_date ~end_date in
  let print_person (p : Domain.person) =
    match p.github_handle with
    | Some gh -> Printf.printf "%s:%s\n" p.full_name gh
    | None -> Printf.printf "%s\n" p.full_name
  in
  List.iter print_person people
;;

let ww_dump_users_cmd : unit Cmd.t =
  Cmd.v
    (Cmd.info "dump-users" ~doc:"Print a list of all known users with GitHub handles")
    Term.(const ww_dump_users $ const ())
;;

(* ------------------------------- *)
(* ----- whatwhat overview ------- *)
let ww_overview no_color =
  let use_color = Unix.isatty Unix.stdout && not no_color in

  (* since whatwhat overview only prints current assignments, we don't need to
     fetch anything more than 1 month *)
  let start_date, end_date = get_start_and_end_dates 1 in
  let _, projects, assignments = Schedule.get_the_schedule ~start_date ~end_date in
  Domain.IntMap.iter
    (fun _ (p : Domain.project) -> Project.print_current_people ~use_color p assignments)
    projects
;;

let ww_overview_cmd : unit Cmd.t =
  Cmd.v
    (Cmd.info "overview" ~doc:"Provide an overview of what the team is working on.")
    Term.(const ww_overview $ no_color_arg)
;;

(* ------------------------------- *)
(* ------- whatwhat test --------- *)
(* - Use this for experimenting! - *)

let ww_test () = print_endline "Testing."

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

(*----------------------------------*)
(*------- whatwhat config --------- *)

let ww_init (force : bool) =
  let config_dir = XDGBaseDir.default.config_home ^ "/whatwhat/" in

  (* First check for the secrets file *)
  let secrets_path = config_dir ^ "secrets.json" in
  Config.create_file ~force secrets_path Config.default_secrets_contents true;

  (* Now check for the config file *)
  let config_path = config_dir ^ "config.json" in
  Config.create_file ~force config_path Config.default_config_contents false
;;

let force_arg : bool Term.t =
  let doc = "Force-overwrite existing config and secrets files" in
  Arg.(value & flag & info [ "f"; "force" ] ~doc)
;;

let ww_init_cmd : unit Cmd.t =
  Cmd.v
    (Cmd.info "init" ~doc:"Update your config files. ")
    Term.(const ww_init $ force_arg)
;;

(* ------------------------------- *)
(* --- putting it all together --- *)

let cmd : unit Cmd.t =
  Cmd.group
    ~default:ww_main_term
    (Cmd.info
       "whatwhat"
       ~doc:"inform about REG projects and people"
       ~version:
         (match Build_info.V1.version () with
          | None -> "unknown version"
          | Some v -> "whatwhat " ^ Build_info.V1.Version.to_string v))
    [ ww_export_project_cmd
    ; ww_export_team_cmd
    ; ww_open_cmd
    ; ww_project_cmd
    ; ww_person_cmd
    ; ww_overview_cmd
    ; ww_test_cmd
    ; ww_init_cmd
    ; ww_slack_bot_cmd
    ; ww_dump_users_cmd
    ]
;;

let () =
  try exit (Cmd.eval ~catch:false cmd) with
  | Sys_error e when e = "Broken pipe" ->
    (* When piped into `head`, for example. Note the awkward construction
       used here instead of directly pattern matching against
       Sys_error "Broken pipe"
       This is needed to get around a "fragile match" warning, explained
       in https://v2.ocaml.org/releases/5.1/htmlman/comp.html#ss:warn52 *)
    (try Sys.set_signal Sys.sigpipe Sys.Signal_default with
     | Invalid_argument _ -> ());
    (try flush stderr with
     | _ -> ());
    (try flush stdout with
     | _ -> ());
    exit 141 (* Choice of exit code is arbitrary, but mimics opam. *)
  | e ->
    (* Any other errors. *)
    let use_color = Unix.isatty Unix.stdout in
    Pretty.prerr ~use_color [ Bold; Foreground Red ] "Fatal error: ";
    Printf.eprintf "%s\n" (Printexc.to_string e);
    exit Cmd.Exit.internal_error (* Defined as 125. *)
;;
