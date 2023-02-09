(** Placeholder: attempts to download the schedule from Forecast *)

(* Note: to run under dune and pass arguments use
   dune exec -- whatwhat <args>

  e.g. dune exec whatwhat --issue="1216"
 *)

open Whatwhat

let whatwhat notify person issue =
  (* notification reports*)
  if notify <> Notify.NoTarget
  then (
    let people, projects, assignments = Schedule.get_the_schedule () in
    print_endline "Whatwhat downloaded:";
    Printf.printf " %d people; " (List.length people);
    Printf.printf "%d projects; and " (List.length projects);
    Printf.printf "%d assignments\n\n" (List.length assignments);

    (* Emit errors and warnings *)
    if notify = Notify.All || notify = Notify.Github
    then
      print_endline "CATCH: this would post reports to github."
      (*To post to github replace CATCH string with 
       Notify.post_metadata_reports ()*)
    else Notify.print_metadata_reports ())
  else print_endline "No notifications requested.";

  (* query person's reactions*)
  if person <> "none"
  then QueryReports.individuals_reactions person
  else print_endline "No person queried.";

  (* query issue reactions*)
  if issue <> "none"
  then QueryReports.issues_reactions issue
  else print_endline "No issue queried."
;;

(* Command-line interface *)
open Cmdliner

(* Capture the arguments *)
let notify =
  let tgs =
    Arg.enum
      [ "github", Notify.Github
      ; "slack", Notify.Slack
      ; "all", Notify.All
      ; "none", Notify.NoTarget
      ]
  in
  let doc =
    "Where to send notifications.\n\
    \           $(docv) may be $(b,github), $(b,slack), $(b,all), or $(b,none)."
  in

  Arg.(value & opt tgs Notify.NoTarget & info [ "n"; "notify" ] ~docv:"NOTIFY" ~doc)
;;

let person =
  let doc = "Name of person to query. $(docv) must be a string argument." in
  Arg.(value & opt string "none" & info [ "p"; "person" ] ~docv:"PERSON" ~doc)
;;

let issue =
  let doc =
    "Issue to query for team reactions. \n\
    \             Can be entered as issue title or number, \n\
    \             but must be a string argument."
  in
  Arg.(value & opt string "none" & info [ "i"; "issue" ] ~docv:"ISSUE" ~doc)
;;

let cmd =
  Cmd.v
    (Cmd.info "whatwhat" ~doc:"Report current project status")
    Term.(const whatwhat $ notify $ person $ issue)
;;

let main () = exit (Cmd.eval cmd)
let () = main ()
