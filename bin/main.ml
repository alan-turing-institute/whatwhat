(** Placeholder: attempts to download the schedule from Forecast *)

(* Note: to run under dune and pass arguments use
   dune exec -- whatwhat <args>
 *)

open Whatwhat




let whatwhat notify person issue=

  (* notification reports*)
  if notify <> Notify.NoTarget then
    let people, projects, assignments = Schedule.get_the_schedule () in
    print_endline "Whatwhat downloaded:";
    Printf.printf " %d people; " (List.length people);
    Printf.printf "%d projects; and " (List.length projects);
    Printf.printf "%d assignments\n\n" (List.length assignments);

    (* Emit errors and warnings *)
    if notify = Notify.All || notify = Notify.Github
    then print_endline "Would notify on Github (maybe don't do this for now)." (*Notify.post_metadata_reports ()*)
    else print_endline "Would print robot messages to the terminal." (*Notify.print_metadata_reports ()*)  
  else
    print_endline "No notifications requested." ;

  if person <> "none" then
    Query_reports.individuals_reactions person
  else 
    print_endline "No person queried.";

  if issue <> "none" then
    Query_reports.issues_reactions issue
  else 
    print_endline "No issue queried."


;;

 
(* 
   TODO: check what happens for GH users without name registered, can they still be queries? 
*)


(* Command-line interface *) 
open Cmdliner

(* Capture the arguments *)
let notify =
  let tgs =  Arg.enum [ 
    "github", Notify.Github ; "slack", Notify.Slack ; "all", Notify.All  ; "none", Notify.NoTarget 
    ] in
  let doc =
    "Where to send notifications.\n\
    \             $(docv) may be $(b,github), $(b,slack), $(b,all), or $(b,none)."
  in

  Arg.(value & opt tgs Notify.NoTarget & info [ "n"; "notify" ] ~docv:"NOTIFY" ~doc)
;;

let person =
  let doc = "Name of person to query. $(docv) must be a string argument." in
  Arg.(value & opt string "none" & info [ "p"; "person" ] ~docv:"PERSON" ~doc)
;;

let issue =
  let doc = "Issue to query for team reactions. Can be entered as issue $(i,title) or $(i,issue number), \n\
  \             but must be a string argument." in
  Arg.(value & opt string "none" & info [ "i"; "issue" ] ~docv:"ISSUE" ~doc)
;;



let cmd =
  Cmd.v
    (Cmd.info "whatwhat" ~doc:"Report current project status")
    Term.(const whatwhat $ notify $ person $ issue)
;;

let main () = exit (Cmd.eval cmd)
let () = main ()