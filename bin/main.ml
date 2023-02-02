(** Placeholder: attempts to download the schedule from Forecast *)

(* Note: to run under dune and pass arguments use
   dune exec -- whatwhat <args>
 *)

open Whatwhat

let issue = Query_reports.issue_summary "Finding people" "1216";;

let issue_report (issue) = 
  print_endline ("\n" ) ;
  Query_reports.print_issue(issue) ;
  print_endline ("\n" ) ;
  Query_reports.get_reaction_table(issue)
;;

let () = issue_report (issue) ;;