(** Notify is used to tell certain people about certain problems
    
   

    The plan is to emit a GitHub comment of the following form:
    {v
    Hi there, this is WhatWhat Bot. I'm a bot. Beep boop!

    I am unable to read the metadata block for this project. Please see [url]
    for details of how this block should be formatted. Among the problems I
    encountered were:

    I couldn't find the block at all. It should be in the body of the issue. 
    I couldn't find these fields:
       - etc
    I couldn't understand the value of these fields:
       - so forth    
    v}
    
 *)

module IntMap = Map.Make (Int)

type notify_target =
  | NoTarget
  | Print
  | Github
  | Slack
  | All

(* From the log of events, produce a map Project Number => events for
   those events that are related to the metadata of a project *)
let extract_metadata_events (event_log : Log.event Seq.t) =
  let extract_project_event (ev : Log.event) =
    match ev.source with
    | Log.GithubMetadata ->
      (match ev.entity with
       | Log.Project nmbr -> Some (nmbr, ev)
       | _ -> None)
    | _ -> None
  in
  (* Cons the event onto a list of events for this project, if this project
     currently exists *)
  let add_event_to_map m (nmbr, ev) =
    let updater = function
      | Some curr -> Some (ev :: curr)
      | None -> Some (ev :: [])
    in
    IntMap.update nmbr updater m
  in
  event_log
  |> Seq.filter_map extract_project_event
  |> Seq.fold_left add_event_to_map IntMap.empty
;;

(* ------------------------------------------------------------ 
   Debugging
 *)

let dump_event (e : Log.event) =
  Printf.printf
    "%s: Module %s reports: %s\n"
    (Log.show_level e.level)
    (Log.show_source e.source)
    e.message
;;

(* Dump all logged events to standard out *)
let dump_the_log () = Seq.iter dump_event @@ Log.get_the_log ()

let dump_metadata_events () =
  extract_metadata_events @@ Log.get_the_log ()
  |> IntMap.iter (fun nmbr evs ->
       print_endline ("Metadata events for project number " ^ string_of_int nmbr);
       List.iter dump_event evs;
       print_endline "")
;;

(* ------------------------------------------------------------
   Reporting
 *)

(* Produce reports for project with issue number nmbr and a list of events
   (for that project!)
 *)
let format_metadata_report (events : Log.event list) : string =
  let errors, warnings = List.partition (fun ev -> ev.Log.level = Log.Error) events in
  let error_msgs = List.map (fun ev -> ev.Log.message) errors in
  let warning_msgs = List.map (fun ev -> ev.Log.message) warnings in
  let n_errors = List.length errors in
  let n_warnings = List.length warnings in
  let buf = Buffer.create 128 in
  Buffer.add_string buf "Beep boop! I'm a bot.\n\n";
  if n_errors > 0
  then (
    Buffer.add_string
      buf
      "I was unable to read the [YAML metadata \
       block](https://github.com/alan-turing-institute/Hut23/blob/master/.github/ISSUE_TEMPLATE/project.md) \
       at the top of this issue because of the following **error(s)**:\n\n\
       - ";
    Buffer.add_string buf (String.concat "\n- " error_msgs))
  else
    Buffer.add_string
      buf
      "I had trouble reading the [YAML metadata \
       block](https://github.com/alan-turing-institute/Hut23/blob/master/.github/ISSUE_TEMPLATE/project.md) \
       at the top of this issue because of the following **problem(s)**:\n\n\
       - ";
  if n_errors > 0 && n_warnings > 0
  then Buffer.add_string buf "\n\nIn addition, I had the following **problem**(s):\n\n- ";
  if n_warnings > 0 then Buffer.add_string buf (String.concat "\n- " warning_msgs);
  Buffer.contents buf
;;

(* Create a markdown report on the problems found when parsing project metadata from GitHub *)
let make_metadata_reports log =
  log |> extract_metadata_events |> IntMap.map format_metadata_report |> IntMap.to_seq
;;

let print_metadata_reports () =
  let the_log = Log.get_the_log () in
  let metadata_reports = make_metadata_reports the_log in
  print_endline "The following problems occured when parsing project metadata:\n";
  Seq.iter
    (fun (nmbr, report) ->
      Printf.printf "Project hut23-%d\n" nmbr;
      print_endline report)
    metadata_reports
;;

let post_metadata_reports () =
  let the_log = Log.get_the_log () in
  let metadata_reports = make_metadata_reports the_log in
  Printf.printf
    "Posting metadata reports to the following %d projects:\n"
    (Seq.length metadata_reports);
  Seq.iter
    (fun (nmbr, report) ->
      Printf.printf "hut23-%d; " nmbr;
      flush stdout;
      ignore @@ GithubBot.github_post "Hut23" nmbr report;
      Unix.sleep 2)
    metadata_reports
;;
