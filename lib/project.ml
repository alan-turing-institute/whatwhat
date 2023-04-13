(** Print an overview of a project. *)

open Domain
open Pretty
open Utf8
module ANSI = ANSITerminal

let ftes_of_assignments (prj : project) (asns : assignment list) : (string * FTE.t) list =
  asns
  |> List.filter (fun a -> a.project.number = prj.number)
  |> List.map (fun a -> get_entity_name a.entity, Domain.ftes_of_assignment a)
;;

let print_budget_and_assignments ~use_color (prj : project) (asns : assignment list) =
  let total_fte_time = ftes_of_assignments prj asns |> List.map snd |> FTE.sum in
  let budget = prj.plan.budget in
  let discrepancy = FTE.div (FTE.sub total_fte_time budget) budget in

  print_heading ~use_color "Current assignments on Forecast";

  (* Print all current assignments on Forecast *)
  let this_project_asns = asns |> List.filter (fun a -> a.project.number = prj.number) in
  match this_project_asns with
  | [] -> Printf.printf "None found.\n"
  | this_asns ->
    (* The assignments themselves *)
    let entity_names = List.map (fun a -> get_entity_name a.entity) this_asns in
    let name_fieldwidth = Utils.max_by ~default:0 wcswidth entity_names in
    let print_and_return_string asn =
      let name = get_entity_name asn.entity in
      let is_people_required = Utils.contains name "People Required" in
      let string =
        Printf.sprintf
          "%s  %18s, %s to %s\n"
          (pad name_fieldwidth (get_entity_name asn.entity))
          (FTE.show_t (ftes_of_assignment asn))
          (CalendarLib.Printer.Date.to_string (get_first_day asn.allocation))
          (CalendarLib.Printer.Date.to_string (get_last_day asn.allocation))
      in
      prout ~use_color:(use_color && is_people_required) [ ANSI.red ] string;
      string
    in
    let assignment_strings = List.map print_and_return_string this_asns in
    (* Horizontal line *)
    let max_length = Utils.max_by ~default:0 wcswidth assignment_strings in
    print_endline (String.make max_length '-');
    (* Then the comparison of assignments vs budget *)
    Printf.printf
      "%s  %18s"
      (pad name_fieldwidth "Allocations found")
      (FTE.show_t total_fte_time);
    prout
      ~use_color:(use_color && Float.abs discrepancy > 0.1)
      [ ANSI.red ]
      (Printf.sprintf " (%+.2f%%)" (100. *. discrepancy));
    print_endline "";
    Printf.printf
      "%s  %18s\n"
      (pad name_fieldwidth "Allocations expected")
      (FTE.show_t budget)
;;

let print_title ~(use_color : bool) (prj : project) =
  let s = Printf.sprintf "Project %d: %s" prj.number prj.name in
  prout ~use_color [ ANSI.Bold ] (make_box s);
  Printf.printf "\n";
  let url =
    String.concat
      "/"
      [ "https://github.com"
      ; Config.get_github_repo_owner ()
      ; Config.get_github_repo_name ()
      ; "issues"
      ; string_of_int prj.number
      ]
  in
  prout ~use_color [ ANSI.Bold ] url
;;

let print_metadata ~(use_color : bool) (prj : project) =
  print_heading ~use_color "Details";
  let open Printf in
  let earliest_start_date_string =
    match prj.plan.earliest_start_date with
    | None -> "None"
    | Some d -> CalendarLib.Printer.Date.to_string d
  in
  let latest_end_date_string =
    match prj.plan.latest_end_date with
    | None -> "None"
    | Some d -> CalendarLib.Printer.Date.to_string d
  in
  printf "State               : %s\n" (State.show_t prj.state);
  (match prj.programme with
   | None -> prout ~use_color [ ANSI.red ] "Programme           : Not found\n"
   | Some s -> printf "Programme           : %s\n" s);
  (match prj.plan.finance_codes with
   | [] -> prout ~use_color [ ANSI.red ] "Finance codes       : Not found\n"
   | xs -> printf "Finance codes       : %s\n" (String.concat ", " xs));
  printf "Earliest start date : %s\n" earliest_start_date_string;
  printf
    "Latest start date   : %s\n"
    (CalendarLib.Printer.Date.to_string prj.plan.latest_start_date);
  printf "Latest end date     : %s\n" latest_end_date_string;
  printf "Minimum FTE         : %.0f%%\n" prj.plan.min_fte_percent;
  printf "Nominal FTE         : %.0f%%\n" prj.plan.nominal_fte_percent;
  printf "Maximum FTE         : %.0f%%\n" prj.plan.max_fte_percent
;;

type emoji =
  | LAUGH
  | THUMBS_UP
  | THUMBS_DOWN
  | OTHER

(* Possible emoji responses*)
let parse_emoji e =
  match e with
  | "laugh" -> LAUGH
  | "+1" -> THUMBS_UP
  | "-1" -> THUMBS_DOWN
  | _ -> OTHER
;;

let get_name (single_person : GithubRaw.person) =
  if single_person.name <> None
  then Option.get single_person.name
  else single_person.login
;;

let print_reactions ~use_color (prj : Domain.project) =
  let issue = GithubRaw.get_issue_r prj.number in
  let sorted_reactions =
    issue.reactions
    |> List.map (fun (e, n) -> parse_emoji e, get_name n)
    |> List.sort (fun (_, n1) (_, n2) -> compare n1 n2)
    |> List.sort (fun (e1, _) (e2, _) -> compare e1 e2)
    |> List.filter (fun (e, _) -> e <> OTHER)
  in
  let header = [ "Name"; "😄"; "👍"; "👎" ] in
  let rows =
    sorted_reactions
    |> List.map (fun (e, n) ->
         match e with
         | LAUGH -> [ n; "x"; ""; "" ]
         | THUMBS_UP -> [ n; ""; "x"; "" ]
         | THUMBS_DOWN -> [ n; ""; ""; "x" ]
         | OTHER -> [ n; ""; ""; "" ]
         (* Should not happen *))
  in
  print_heading ~use_color "Reactions";
  print_endline (make_table ~header_rows:1 ~column_padding:1 (header :: rows))
;;

let print ~(use_color : bool) (prj : project) (asns : assignment list) =
  print_title ~use_color prj;
  print_endline "";
  print_endline "";
  print_metadata ~use_color prj;
  print_endline "";
  print_budget_and_assignments ~use_color prj asns;
  print_endline "";
  print_reactions ~use_color prj
;;
