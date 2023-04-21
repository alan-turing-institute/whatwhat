(** Print an overview of a project. *)

open Domain
open Pretty
module ANSI = ANSITerminal

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
  printf "State               : %s\n" (State.show_t prj.state);
  (match prj.programme with
   | None -> prout ~use_color [ ANSI.red ] "Programme           : Not found\n"
   | Some s -> printf "Programme           : %s\n" s);
  match prj.plan with
  | None -> print_endline "Remaining metadata could not be parsed from GitHub."
  | Some plan ->
      let earliest_start_date_string =
        match plan.earliest_start_date with
        | None -> "None"
        | Some d -> CalendarLib.Printer.Date.to_string d
      in
      let latest_end_date_string =
        match plan.latest_end_date with
        | None -> "None"
        | Some d -> CalendarLib.Printer.Date.to_string d
      in
      (match plan.finance_codes with
       | [] -> prout ~use_color [ ANSI.red ] "Finance codes       : Not found\n"
       | xs -> printf "Finance codes       : %s\n" (String.concat ", " xs));
      printf "Earliest start date : %s\n" earliest_start_date_string;
      printf
        "Latest start date   : %s\n"
        (CalendarLib.Printer.Date.to_string plan.latest_start_date);
      printf "Latest end date     : %s\n" latest_end_date_string;
      printf "Minimum FTE         : %.0f%%\n" plan.min_fte_percent;
      printf "Nominal FTE         : %.0f%%\n" plan.nominal_fte_percent;
      printf "Maximum FTE         : %.0f%%\n" plan.max_fte_percent
;;

(** [asns] must be subsetted to only those belonging to this project *)
let print_budget_and_assignments ~use_color (prj : project) (asns : assignment list) =
  print_heading ~use_color "Assignments on Forecast";

  let alloc_found = "Allocations found" in
  let alloc_expected = "Allocations expected" in
  match asns with
  | [] -> Printf.printf "None found.\n"
  | this_asns ->
    (* The assignments themselves *)
    let entity_names = List.map Assignment.get_entity_name this_asns in
    let name_fieldwidth =
      Utils.max_by ~default:0 wcswidth (alloc_found :: alloc_expected :: entity_names)
    in
    let print_and_return_string asn =
      let name = Assignment.get_entity_name asn in
      let is_people_required = Utils.contains name "People Required" in
      let is_current = Assignment.get_time_status asn = Current in
      let string =
        Printf.sprintf
          "%9s %s  %18s, %s to %s"
          ("(" ^ Assignment.show_time_status asn ^ ")")
          (pad name_fieldwidth (Assignment.get_entity_name asn))
          (FTE.show_t (Assignment.to_fte_weeks asn))
          (CalendarLib.Printer.Date.to_string (get_first_day asn.allocation))
          (CalendarLib.Printer.Date.to_string (get_last_day asn.allocation))
      in
      prout ~use_color:(use_color && is_people_required && is_current) [ ANSI.red ] string;
      print_endline "";
      string
    in
    let assignment_strings = List.map print_and_return_string this_asns in
    (* Horizontal line *)
    let max_length = Utils.max_by ~default:0 wcswidth assignment_strings in
    print_endline (String.make max_length '-');

  (* Then the comparison of assignments vs budget *)
  let total_fte_time = asns |> List.map Assignment.to_fte_weeks |> FTE.sum in
  Printf.printf
    "%9s %s  %18s"
    ""
    (pad name_fieldwidth alloc_found)
    (FTE.show_t total_fte_time);

  match prj.plan with
  | None -> print_endline "";
  | Some plan ->
    let budget = plan.budget in
    let discrepancy = FTE.div (FTE.sub total_fte_time budget) budget in
      prout
        ~use_color:(use_color && Float.abs discrepancy > 0.1)
        [ ANSI.red ]
        (Printf.sprintf " (%+.2f%%)" (100. *. discrepancy));
      print_endline "";
      Printf.printf
        "%9s %s  %18s\n"
        ""
        (pad name_fieldwidth alloc_expected)
        (FTE.show_t budget)
;;

let print_reactions ~use_color (ppl : person list) (prj : project) =
  let issue = GithubRaw.get_issue_r prj.number in
  let get_full_name (gh_p : Github.person) =
    match List.find_opt (fun (p : person) -> p.github_handle = Some gh_p.login) ppl with
    | None -> "@" ^ gh_p.login
    | Some p -> p.full_name
  in
  let sorted_reactions =
    issue.reactions
    |> List.map (fun (e, p) -> parse_emoji e, get_full_name p)
    |> List.sort (fun (_, n1) (_, n2) -> compare n1 n2)
    |> List.sort (fun (e1, _) (e2, _) -> compare e1 e2)
    |> List.filter (fun (e, _) -> e <> Other)
  in
  let header = [ "Name"; "😄"; "👍"; "👎" ] in
  let rows =
    sorted_reactions
    |> List.map (fun (e, n) ->
         match e with
         | Laugh -> [ n; "x"; ""; "" ]
         | ThumbsUp -> [ n; ""; "x"; "" ]
         | ThumbsDown -> [ n; ""; ""; "x" ]
         | Other -> [ n; ""; ""; "" ])
    (* Last case should not happen *)
  in
  print_heading ~use_color "Reactions";
  print_endline (make_table ~header_rows:1 ~column_padding:1 (header :: rows))
;;

let print_log_events ~use_color (prj : project) =
  print_heading ~use_color "Errors and warnings";
  Log.pretty_print
    ~use_color
    ~verbose:0
    ~restrict_codes:Log.All
    ~restrict_issues:(Some [ prj.number ])
;;

let print ~(use_color : bool) (prj : project) (ppl : person list) (asns : assignment list)
  =
  let this_asns =
    asns
    |> List.filter (fun a -> a.project.number = prj.number)
    |> List.sort Assignment.compare_by_date
  in
  print_title ~use_color prj;
  print_endline "";
  print_endline "";
  print_metadata ~use_color prj;
  print_endline "";
  print_budget_and_assignments ~use_color prj this_asns;
  print_endline "";
  print_reactions ~use_color ppl prj;
  print_endline "";
  print_log_events ~use_color prj
;;
