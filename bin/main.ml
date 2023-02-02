(** Placeholder: attempts to download the schedule from Forecast *)

(* Note: to run under dune and pass arguments use
   dune exec -- whatwhat <args>
 *)

open Whatwhat

(* Can pass in issue number or title*)
(* let issue = Query_reports.issue_summary "Finding people" "1216";;

let issue_report (issue) = 
  print_endline ("\n" ) ;
  Query_reports.print_issue(issue) ;
  print_endline ("\n" ) ;
  Query_reports.get_reaction_table(issue)
;;

let () = issue_report (issue) ;; *)


let person_reactions = Query_reports.person_summary "Finding people" "Joe Palmer";;

let person_report (person_reactions) = 
  print_endline ("\n" ) ;
  print_endline ("Number of issues " ^ string_of_int (List.length person_reactions))
;;

let () = person_report (person_reactions) ;;