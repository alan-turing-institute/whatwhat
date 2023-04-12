(** Functions to print stuff about projects *)

open Domain
module ANSI = ANSITerminal

(** Print a bold, underlined heading *)
let print_heading ~(use_color : bool) heading =
  let n = String.length heading in
  Utils.prcol ~use_color [ Bold ] (heading ^ "\n");
  Utils.prcol ~use_color [ Bold ] (String.make n '-' ^ "\n")
;;

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
    let name_fieldwidth = Utils.max_by ~default:0 String.length entity_names in
    let print_and_return_string asn =
      let name = get_entity_name asn.entity in
      let is_people_required = Utils.contains name "People Required" in
      let string =
        Printf.sprintf
          "%s  %18s, %s to %s\n"
          (Utils.pad name_fieldwidth (get_entity_name asn.entity))
          (FTE.show_t (ftes_of_assignment asn))
          (CalendarLib.Printer.Date.to_string (get_first_day asn.allocation))
          (CalendarLib.Printer.Date.to_string (get_last_day asn.allocation))
      in
      Utils.prcol ~use_color:(use_color && is_people_required) [ ANSI.red ] string;
      string
    in
    let assignment_strings = List.map print_and_return_string this_asns in
    (* Horizontal line *)
    let max_length = Utils.max_by ~default:0 String.length assignment_strings in
    print_endline (String.make max_length '-');
    (* Then the comparison of assignments vs budget *)
    Printf.printf
      "%s  %18s"
      (Utils.pad name_fieldwidth "Allocations found")
      (FTE.show_t total_fte_time);
    Utils.prcol
      ~use_color:(use_color && Float.abs discrepancy > 0.1)
      [ ANSI.red ]
      (Printf.sprintf " (%+.2f%%)" (100. *. discrepancy));
    print_endline "";
    Printf.printf
      "%s  %18s\n"
      (Utils.pad name_fieldwidth "Allocations expected")
      (FTE.show_t budget)
;;

let make_box s =
  let n = String.length s in
  let top_and_bottom_row = "+" ^ String.make (n + 2) '-' ^ "+" in
  let middle_row = "| " ^ s ^ " |" in
  String.concat "\n" [ top_and_bottom_row; middle_row; top_and_bottom_row ]
;;

let print_title ~(use_color : bool) (prj : project) =
  let s = Printf.sprintf "Project %d: %s" prj.number prj.name in
  Utils.prcol ~use_color [ Bold ] (make_box s);
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
  Utils.prcol ~use_color [ Bold ] url
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
   | None -> Utils.prcol ~use_color [ ANSI.red ] "Programme           : Not found\n"
   | Some s -> printf "Programme           : %s\n" s);
  (match prj.plan.finance_codes with
   | [] -> Utils.prcol ~use_color [ ANSI.red ] "Finance codes       : Not found\n"
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

open QueryReports

(* Reactions *)
(* 
  TODO: collapse people with multiple reactions (example: issue 1216) 
  This probably involves counting the reactions instead
  then refactoring the table according to the counts. 
*)
let get_reaction_table (issue : GithubRaw.issue_r) =
  (* Get issue reactions then sort by most love -> least love, 
     then alphabetically *)
  let issue_reactions =
    issue.reactions
    |> List.sort (fun (_, n1) (_, n2) -> compare_names n1 n2)
    |> List.sort (fun (e1, _) (e2, _) -> compare_emojis e1 e2)
  in

  (* Get all emoji reactions and names *)
  let all_emoji, all_names = List.split issue_reactions in
  let all_emoji = List.map refactor_emoji all_emoji in
  let all_names = List.map get_name all_names in

  (* Find the longest name for cell size *)
  let max_name_length = List.fold_left (fun x y -> max x (String.length y)) 0 all_names in

  (* table format emojis*)
  let table_format_emojis = List.map get_outcome all_emoji in
  let table_body = List.map2 (body_list max_name_length) all_names table_format_emojis in

  let bl = border_line max_name_length max_emoji_length in
  let hl = header_line max_name_length max_emoji_length in

  bl, hl, table_body
;;

let print_reactions ~use_color (prj : Domain.project) =
  let issue = GithubRaw.get_issue_r prj.number in
  let bl, hl, table_body = get_reaction_table issue in
  print_heading ~use_color "Reactions";
  print_endline bl;
  print_endline hl;
  print_endline bl;
  List.iter print_endline table_body;
  print_endline bl
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
