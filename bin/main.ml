(** Placeholder: attempts to download the schedule from Forecast *)

(* Note: to run under dune and pass arguments use
   dune exec -- whatwhat <args>
 *)

open Whatwhat

(* let whatwhat_notify target =
  let people, projects, assignments = Schedule.get_the_schedule () in
  print_endline "Whatwhat downloaded:";
  Printf.printf " %d people; " (List.length people);
  Printf.printf "%d projects; and " (List.length projects);
  Printf.printf "%d assignments\n\n" (List.length assignments);

  (* Emit errors and warnings *)
  if target = Notify.All || target = Notify.Github
  then Notify.post_metadata_reports ()
  else Notify.print_metadata_reports ()
;; *)

let individuals_reactions target = 
  let bl, hl, table_body, difference = Query_reports.person_summary "Finding people" target in

  (* print the person's reactions *)
  print_endline ("\n" ^ target ^ " has reacted to " ^ string_of_int (List.length table_body) ^ " issues:\n");

  print_endline bl;
  print_endline hl;
  print_endline bl;
  List.iter print_endline (table_body);
  print_endline bl;

  print_endline ("\nThey have not reacted to " ^ string_of_int (List.length difference) ^
  " issues: " ^ String.concat ", " difference)
;;

(* let issues_reactions target = 
  let issue = Query_reports.issue_summary "Finding people" target in
  
  print_endline "";
  Query_reports.print_issue(issue) ;
  
  let bl, hl, table_body = Query_reports.get_reaction_table(issue) in

  print_endline ("There are " ^ string_of_int (List.length table_body) ^ " reactions for this issue:\n" );
 
  (* Print the table *)
  print_endline (bl);
  print_endline (hl);
  print_endline (bl);
  List.iter print_endline (table_body);
  print_endline (bl)
;; *)

 
(* 
   TODO: check what happens for GH users without name registered, can they still be queries? 
*)

(* Command-line interface *) 
open Cmdliner

let target =
  let tgs = Arg.enum [ "1216", "1216"] in
  let doc =
    "Where to send notifications.\n\
    \             $(docv) may be $(b,github), $(b,slack), $(b,all), or $(b,none)."
  in

  Arg.(value & opt tgs "martin" & info [ "t"; "target" ] ~docv:"TARGET" ~doc)
;;

let cmd =
  Cmd.v
    (Cmd.info "individuals_reactions" ~doc:"Report current project status")
    Term.(const individuals_reactions $ target)
;;

let main () = exit (Cmd.eval cmd)
let () = main ()