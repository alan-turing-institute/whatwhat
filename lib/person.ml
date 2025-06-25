(** Print an overview of a person. *)

open Domain
open Pretty
module ANSI = ANSITerminal
module CL = CalendarLib

let get_fte_weeks_in_week (asns : assignment list) (week : CL.Date.t) =
  (* Calculate number of FTE-weeks in one assignment *)
  let get_one asn =
    let alloc = asn.allocation in
    let days = Utils.get_weekdays_in_week week in
    let hours =
      List.map
        (fun d ->
          match DateMap.find_opt d alloc with
          | None -> FTE.zero
          | Some h -> h)
        days
    in
    let is_placeholder =
      match asn.entity with
      | Placeholder _ -> true
      | _ -> false
    in
    FTE.sum_to_weeks ~is_placeholder hours
  in
  (* Then sum them up *)
  List.map get_one asns |> FTE.sum
;;

let calculate_allocation_percentage
  (asns : assignment list)
  (month : [> `Year | `Month ] CL.Date.date)
  =
  let weeks = Utils.get_turing_weeks_in_month month in
  let fte_weeks = List.map (get_fte_weeks_in_week asns) weeks |> FTE.sum in
  let expected_fte_weeks = FTE.Weeks (float_of_int @@ List.length weeks) in
  FTE.div fte_weeks expected_fte_weeks
;;

let print_info ~(use_color : bool) (psn : person) =
  let s =
    String.concat
      "\n"
      (match psn.github_handle with
       | None -> [ psn.full_name; psn.email ]
       | Some g -> [ psn.full_name; psn.email; "https://github.com/" ^ g ])
  in
  prout ~use_color [ ANSI.Bold ] (make_box s)
;;

let print_github_assignments ~(use_color : bool) (prjs : project list) =
  print_heading ~use_color "GitHub issue assignments";
  match prjs with
  | [] -> print_endline "None found."
  | _ ->
    let print_prj prj = Printf.printf "#%-4d %s\n" prj.number prj.name in
    List.iter print_prj prjs
;;

let print_assignments ~(use_color : bool) (asns : assignment list) =
  let make_name asn = Printf.sprintf "#%-4d %s" asn.project.number asn.project.name in
  print_heading ~use_color "Recent Forecast assignments";
  match asns with
  | [] -> Printf.printf "None found.\n"
  | this_asns ->
    (* The assignments themselves *)
    let project_names = List.map make_name this_asns in
    let max_length = 50 in
    let elided_project_names = List.map (Utils.elide ~max_length) project_names in
    let name_fieldwidth = Utils.max_by ~default:0 wcswidth elided_project_names in
    let print_asn asn =
      let string =
        Printf.sprintf
          "%9s %s %17s, %s to %s"
          ("(" ^ Assignment.show_time_status asn ^ ")")
          (asn |> make_name |> Utils.elide ~max_length |> pad name_fieldwidth)
          (FTE.show_t (Assignment.to_fte_weeks asn))
          (CL.Printer.Date.to_string (get_first_day asn.allocation))
          (CL.Printer.Date.to_string (get_last_day asn.allocation))
      in
      print_endline string
    in
    List.iter print_asn this_asns
;;

let print_capacity ~(use_color : bool) (asns : assignment list) =
  print_heading ~use_color "Allocation percentage by month";
  let today = CL.Date.today () in
  let this_month =
    CL.Date.make_year_month
      (CL.Date.year today)
      (CL.Date.int_of_month @@ CL.Date.month today)
  in
  let months =
    List.map
      (fun i -> CL.Date.add this_month (CL.Date.Period.month i))
      (Utils.range (-1) 4)
  in
  let allocation_pcts = List.map (calculate_allocation_percentage asns) months in
  List.iter
    (fun (m, pct) ->
      prout
        ~use_color:(use_color && Float.abs (1. -. pct) > 0.01)
        [ ANSI.red ]
        (Printf.sprintf
           "%s %d: %.1f%%\n"
           (m |> CL.Date.month |> CL.Date.int_of_month |> Utils.show_month)
           (CL.Date.year m)
           (pct *. 100.)))
    (List.combine months allocation_pcts)
;;

let print_reactions ~(use_color : bool) (psn : person) (prjs : project Domain.IntMap.t) =
  let prj_numbers = Domain.IntMap.bindings prjs |> List.map fst in
  let reactions = GithubRaw.get_multiple_reactions prj_numbers in
  let parsed_reactions =
    Domain.IntMap.filter_map
      (fun number reactions ->
        let prj = Domain.IntMap.find number prjs in
        match
          List.find_opt
            (fun (_, (person : GithubRaw.person)) ->
              Some person.login = psn.github_handle)
            reactions
        with
        | None -> None (* Person didn't react to the project *)
        | Some (e, _) -> Some (prj, parse_emoji e))
      reactions
  in
  match parsed_reactions |> Domain.IntMap.bindings |> List.map snd with
  | [] -> print_endline "No reactions found."
  | rs ->
    let header = [ "#"; "Project title"; "😄"; "👍"; "👎" ] in
    let table_rows =
      rs
      |> List.stable_sort (fun (_, e1) (_, e2) -> compare e1 e2)
      |> List.filter (fun (_, e) -> e <> Other)
      |> List.map (fun (p, e) ->
           let open Utils in
           match e with
           | Laugh -> [ string_of_int p.number; elide p.name; "x"; ""; "" ]
           | ThumbsUp -> [ string_of_int p.number; elide p.name; ""; "x"; "" ]
           | ThumbsDown -> [ string_of_int p.number; elide p.name; ""; ""; "x" ]
           | Other -> [ string_of_int p.number; elide p.name; ""; ""; "" ])
    in
    print_heading ~use_color "Reactions";
    print_endline (make_table ~header_rows:1 ~column_padding:1 (header :: table_rows))
;;

let print
  ~(use_color : bool)
  (psn : person)
  (prjs : project Domain.IntMap.t)
  (asns : assignment list)
  : unit
  =
  let this_asns =
    asns
    |> List.filter (fun a ->
         a.entity = Person psn
         && get_last_day a.allocation
            >= CL.Date.add (CL.Date.today ()) (CL.Date.Period.month (-2)))
    |> List.sort Assignment.compare_by_date
  in
  let this_github_prjs =
    prjs
    |> IntMap.filter (fun _ v -> List.mem psn v.assignees)
    |> IntMap.bindings
    |> List.map snd
  in
  print_info ~use_color psn;
  print_endline "";
  print_endline "";
  print_github_assignments ~use_color this_github_prjs;
  print_endline "";
  print_assignments ~use_color this_asns;
  print_endline "";
  print_capacity ~use_color this_asns;
  print_endline "";
  print_reactions ~use_color psn prjs
;;

let get_info (psn : person) =
  String.concat
    "\n"
    (match psn.github_handle with
     | None ->
       [ Printf.sprintf "*%s*" psn.full_name
       ; Printf.sprintf " :incoming_envelope: %s" psn.email
       ]
     | Some g ->
       [ Printf.sprintf "*%s*" psn.full_name
       ; Printf.sprintf " :incoming_envelope: `%s`" psn.email
       ; Printf.sprintf " :cat: `https://github.com/%s`" g
       ])
;;

let get_github_assignments (prjs : project list) =
  let print_prj prj =
    Printf.sprintf
      ":star2: <https://github.com/%s/%s/issues/%d|#%d %s>"
      (Config.get_github_repo_owner ())
      (Config.get_github_repo_name ())
      prj.number
      prj.number
      prj.name
  in
  let prj_details =
    match prjs with
    | [] -> [ "None found." ]
    | _ -> List.map print_prj prjs
  in
  String.concat "\n" ("*GitHub issue assignments*" :: prj_details)
;;

let get_assignments (asns : assignment list) =
  let assignment_details =
    match asns with
    | [] -> [ "None found." ]
    | this_asns ->
      let print_asn asn =
        Printf.sprintf
          ":star2: %s <https://github.com/%s/%s/issues/%d|#%d %s> \n        ↳%s, %s to %s"
          ("(" ^ Assignment.show_time_status asn ^ ")")
          (Config.get_github_repo_owner ())
          (Config.get_github_repo_name ())
          asn.project.number
          asn.project.number
          asn.project.name
          (FTE.show_t (Assignment.to_fte_weeks asn))
          (CL.Printer.Date.to_string (get_first_day asn.allocation))
          (CL.Printer.Date.to_string (get_last_day asn.allocation))
      in
      List.map print_asn this_asns
  in
  String.concat "\n" ("*Forecast assignments*" :: assignment_details)
;;

let make_slack_output
  (psn : person)
  (prjs : project Domain.IntMap.t)
  (asns : assignment list)
  : string Lwt.t
  =
  let this_asns =
    asns
    |> List.filter (fun a ->
         a.entity = Person psn
         && get_last_day a.allocation
            >= CL.Date.add (CL.Date.today ()) (CL.Date.Period.month (-2)))
    |> List.sort Assignment.compare_by_date
  in
  let this_github_prjs =
    prjs
    |> IntMap.filter (fun _ v -> List.mem psn v.assignees)
    |> IntMap.bindings
    |> List.map snd
  in
  let info = get_info psn in
  let github_assignments = get_github_assignments this_github_prjs in
  let forecast_assignments = get_assignments this_asns in
  (* let* capacity = print_capacity ~use_color this_asns in *)
  (* let* reactions = print_reactions ~use_color psn prjs in *)
  Lwt.return
    (String.concat
       "\n"
       [ info
       ; ""
       ; ""
       ; github_assignments
       ; ""
       ; forecast_assignments
         (* ; "" *)
         (* ; capacity *)
         (* ; "" *)
         (* ; reactions *)
       ])
;;
