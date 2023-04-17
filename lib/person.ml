(** Print an overview of a person. *)

open Domain
open Pretty
module ANSI = ANSITerminal

let print_info ~(use_color : bool) (psn : Domain.person) =
  let s =
    String.concat
      "\n"
      (match psn.github_handle with
       | None -> [ psn.full_name; psn.email ]
       | Some g -> [ psn.full_name; psn.email; "@" ^ g ])
  in
  prout ~use_color [ ANSI.Bold ] (make_box s)
;;

let print ~(use_color : bool) (psn : Domain.person) (asns : Domain.Assignment.t list)
  : unit
  =
  print_info ~use_color psn;
  print_endline "";
  ignore asns;
  print_endline "TODO: Remainder of person summary"
;;
