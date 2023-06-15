(** Pings people about problems on GitHub (and perhaps Slack, eventually). *)

open GithubRaw
module IntMap = Map.Make (Int)

let post_github_comment_async issue post_body =
  let open Lwt.Syntax in
  let uri =
    String.concat
      "/"
      [ Config.github_url
      ; "repos"
      ; Config.get_github_repo_owner ()
      ; Config.get_github_repo_name ()
      ; "issues"
      ; string_of_int issue
      ; "comments"
      ]
  in
  let body = `Assoc [ "body", `String post_body ] |> Yojson.Basic.to_string in
  (* We don't need the return value. *)
  let* _ = run_github_query_async ~as_bot:true ~http_method:POST ~body uri in
  Lwt.return ()
;;

type notify_target =
  | NoTarget
  | Github
  | Slack
  | All

let make_github_message events =
  let error_url =
    "https://alan-turing-institute.github.io/whatwhat/whatwhat/reporting.html"
  in
  let buf = Buffer.create 128 in
  List.iter
    (Buffer.add_string buf)
    [ "Beep boop! I'm a bot.\n"
    ; "\n"
    ; "I encountered the following errors when parsing data about this project from \
       GitHub and Forecast:\n"
    ; "\n"
    ];
  events
  |> List.map (fun (e : Log.event) ->
       Printf.sprintf
         " - **%s**: %s\n"
         (Log.show_level e.level)
         (Utils.gfm_escape e.message))
  |> List.iter (Buffer.add_string buf);

  List.iter
    (Buffer.add_string buf)
    [ "\n"
    ; "You can get more info about how to fix these issues at: "
    ; error_url
    ; "\n"
    ; "Alternatively, [contact the people in charge of project scheduling](https://github.com/alan-turing-institute/research-engineering-group/wiki/Service-areas)."
    ];
  Buffer.contents buf
;;

let post_github ~verbose ~restrict_codes ~restrict_issues =
  let pairs = Log.gather_events' ~verbose ~restrict_codes ~restrict_issues in
  let requests =
    List.map
      (fun (issue_num_opt, events) ->
        match issue_num_opt with
        | None ->
          Lwt.return ()
          (* TODO: decide what to do with events that don't have an associated issue number *)
        | Some num -> post_github_comment_async num (make_github_message events))
      pairs
  in
  ignore (requests |> Utils.all_throttled |> Lwt_main.run)
;;

(* ---------------------------------------------------------------------- *)

(** --- Notifications to individuals via Slack *)


(* Find a person to notify *)

(* Return the person in the assigment if
   (a) They are a person, not a placeholder; and
   (b) Their allocation is not in the past.
 *)
let assigned_person (assn : Domain.assignment) : Domain.person option =
  match (Domain.Assignment.get_time_status assn) with
  | Past ->
     (match assn.entity with
      | Person p -> Some p
      | _        -> None)
  | _ ->
     None
  
(* Look up those allocated to a project from now *)
let get_project_team (_, _, assignments) project_num : Domain.person list =
  assignments
  |> List.filter (fun (a : Domain.assignment) -> a.project.number == project_num)
  |> List.filter_map assigned_person

(* Follow the "notification chain" until we find someone
   Returns a list of people, which may be empty *)

let find_notifiable_individuals the_schedule proj_num =
    get_project_team the_schedule proj_num

let show_event_project
      (_, (projects : Domain.project IntMap.t), _)
      (evt : (int option * Log.event))
    : string =
  let (maybe_num, _) = evt in
  match maybe_num with
    | None -> "(Could not identify project)"
    | Some num ->
       "Project #"
       ^ (string_of_int num)
       ^ " \"" ^ (IntMap.find num projects).name ^ "\""
           

let post_event_to_slack
      the_schedule
      (evt : (int option * Log.event)) =
  let (maybe_num, ev) = evt in
  match maybe_num with
  | None     -> ()
  | Some num ->
     match find_notifiable_individuals the_schedule num with
     | [] -> ()
     | _ as people ->
        begin
          print_string "\nPlease notify ";
          print_endline @@
            String.concat ", "
              (List.map (fun (p : Domain.person) -> p.full_name) people);
          print_endline "about the message:";
          print_endline @@ show_event_project the_schedule evt;
          print_endline @@ (Log.show_level ev.level) ^ ": " ^ ev.message
        end

let post_slack
      the_schedule
      ~verbose
      ~restrict_codes
      ~restrict_issues =
  begin
    print_endline "TODO. The following should be posted to Slack.";
    Log.gather_events ~verbose ~restrict_codes ~restrict_issues
    |> List.iter (post_event_to_slack the_schedule)
  end

