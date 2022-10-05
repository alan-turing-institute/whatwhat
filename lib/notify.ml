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

module IntMap = Map.Make(Int)

type target =
  | NoTarget
  | GitHub
  | Slack
  | All


(* From the log of events, produce a map Project Number => event for
   those events that are related to the metadata of a project *)
let extract_metadata_events (event_log : Log.event Seq.t) =
  let extract_project_event (ev : Log.event) =
    match ev.source with
    | Log.GitHubMetadata ->
       begin
         match ev.entity with
         | Log.Project nmbr -> Some (nmbr, ev)
         | _ -> None
       end
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





(* ------------------------------------------------------------ 
   Debugging
 *)

let dump_event (e : Log.event) =
  Printf.printf "%s: Module %s reports: %s\n"
    (Log.show_level e.level)
    (Log.show_source e.source)
    e.message

(* Dump all logged events to standard out *)
let dump_the_log () =
  Seq.iter dump_event @@ Log.get_the_log ()

let dump_metadata_events () =
  extract_metadata_events @@ Log.get_the_log ()
  |> IntMap.iter (fun nmbr evs ->
         begin
           print_endline ("Metadata events for project number " ^ (string_of_int nmbr));
           List.iter dump_event evs;
           print_endline ""
         end
       )
                  


