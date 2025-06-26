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

module TimesheetProject = struct
  type t =
    { code : string
    ; fte_hour : FTE.hour
    ; project_name : string
    }

  let compare t1 t2 =
    match compare t1.fte_hour t2.fte_hour with
    | 0 -> String.compare t1.code t2.code
    | c -> c
  ;;
end

let get_timesheets (asns : assignment list) (month : [> `Year | `Month ] CL.Date.date) =
  let weekdays = Utils.get_weekdays_in_month month in
  let find_relevant_allocations d =
    List.filter_map
      (fun asn ->
        match DateMap.find_opt d asn.allocation with
        | None -> None
        | Some fte_hour ->
          Some
            { TimesheetProject.code =
                (match asn.project.erpx_finance_code with
                 | None -> "unknown"
                 | Some code -> code)
            ; TimesheetProject.fte_hour
            ; TimesheetProject.project_name = asn.project.name
            })
      asns
    |> List.sort TimesheetProject.compare
  in
  weekdays
  |> List.map (fun day -> day, find_relevant_allocations day)
  |> List.to_seq
  |> Seq.group (fun t1 t2 -> snd t1 = snd t2)
  |> Seq.map (fun group ->
    let lst = List.of_seq group in
    let allocations = snd (List.hd lst) in
    let dates = List.map fst lst in
    let first_day = List.hd dates in
    let last_day = List.hd (List.rev dates) in
    first_day, last_day, allocations)
  |> List.of_seq
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
  (* the keys of prjs are Forecast IDs; we want to convert to GitHub issue
     numbers for the purposes of this function *)
  let prjs_indexed_by_github =
    Domain.IntMap.fold
      (fun _ prj acc -> Domain.IntMap.add prj.number prj acc)
      prjs
      Domain.IntMap.empty
  in
  let prj_numbers = Domain.IntMap.bindings prjs_indexed_by_github |> List.map fst in
  let reactions = GithubRaw.get_multiple_reactions prj_numbers in
  let parsed_reactions =
    Domain.IntMap.filter_map
      (fun number reactions ->
        let prj = Domain.IntMap.find number prjs_indexed_by_github in
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

let print_timesheets
  ~(use_color : bool)
  (timesheets : (CL.Date.t * CL.Date.t * TimesheetProject.t list) list)
  : unit
  =
  if List.length timesheets = 0
  then print_endline "No timesheets found for this month."
  else
    let open TimesheetProject in
    let first_date = List.hd timesheets |> fun (first_day, _, _) -> first_day in
    let year = CL.Date.year first_date in
    let month = CL.Date.month first_date |> CL.Date.int_of_month in
    print_heading ~use_color
    @@ "Timesheets for "
    ^ Utils.show_month month
    ^ " "
    ^ string_of_int year;
    (* To be useful, we try to print the timesheets in the format that it
       is entered on ERPx. *)
    let all_project_names_and_codes =
      List.flatten
        (List.map
           (fun (_, _, ts) -> List.map (fun ts -> ts.project_name, ts.code) ts)
           timesheets)
    in
    let header_row =
      "Forecast project name"
      :: "Work order"
      :: List.map
           (fun (first, last, _) ->
             string_of_int (CL.Date.day_of_month first)
             ^ " to "
             ^ string_of_int (CL.Date.day_of_month last))
           timesheets
    in
    let project_rows =
      List.map
        (fun (name, code) ->
          Utils.elide ~max_length:45 name
          :: code
          :: List.map
               (fun (_, _, ts) ->
                 match List.filter (fun t -> t.project_name = name) ts with
                 | [] -> "-"
                 | [ x ] -> FTE.show_hour_adjusted_for_lunch x.fte_hour
                 | _ ->
                   failwith "Multiple timesheet entries for the same project in one day")
               timesheets)
        all_project_names_and_codes
    in
    print_endline (make_table ~header_rows:1 (header_row :: project_rows))
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
  (* For timesheets, we get the previous month if today is in the first 7 days of the month,
     otherwise we get the current month. *)
  let today = CL.Date.today () in
  let year, month =
    if CL.Date.day_of_month today > 7
    then CL.Date.year today, CL.Date.month today |> CL.Date.int_of_month
    else (
      match CL.Date.month today with
      | Jan -> CL.Date.year today - 1, 12
      | m -> CL.Date.year today, CL.Date.int_of_month m - 1)
  in
  let timesheets = get_timesheets this_asns (CL.Date.make_year_month year month) in
  print_info ~use_color psn;
  print_endline "";
  print_endline "";
  print_github_assignments ~use_color this_github_prjs;
  print_endline "";
  print_assignments ~use_color this_asns;
  print_endline "";
  print_timesheets ~use_color timesheets;
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
