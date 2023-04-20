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
       | Some g -> [ psn.full_name; psn.email; "https://github.com/" ^ g ])
  in
  prout ~use_color [ ANSI.Bold ] (make_box s)
;;

let print_assignments ~(use_color : bool) (asns : Domain.assignment list) =
  let make_name asn = 
    Printf.sprintf "#%-4d %s" asn.project.number asn.project.name
  in
  print_heading ~use_color "Assignments on Forecast";
  match asns with
  | [] -> Printf.printf "None found.\n"
  | this_asns ->
    (* The assignments themselves *)
    let project_names = List.map make_name this_asns in
    let name_fieldwidth =
      Utils.max_by ~default:0 wcswidth project_names
    in
    let print_asn asn =
      let string =
        Printf.sprintf
          "%9s %s  %18s, %s to %s"
          ("(" ^ Assignment.show_time_status asn ^ ")")
          (pad name_fieldwidth (make_name asn))
          (FTE.show_t (Assignment.to_fte_weeks asn))
          (CalendarLib.Printer.Date.to_string (get_first_day asn.allocation))
          (CalendarLib.Printer.Date.to_string (get_last_day asn.allocation))
      in
      print_endline string;
    in
    List.iter print_asn this_asns
;;

let print ~(use_color : bool) (psn : Domain.person) (asns : Domain.assignment list)
  : unit
  =
  let this_asns =
    asns
    |> List.filter (fun a -> a.entity = Person psn)
    |> List.sort Assignment.compare_by_date
  in
  print_info ~use_color psn;
  print_endline "";
  print_endline "";
  print_assignments ~use_color this_asns;
  print_endline "";
  print_endline "TODO: Remainder of person summary"
;;
