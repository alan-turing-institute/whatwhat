open ForecastRaw
module DateMap = Map.Make (CalendarLib.Date)

(** [ndays_of_week_in d start_date end_date] counts the number of weekdays in
    the week commencing [d] between [start_date] and [end_date].
    [d] must be a Monday. *)
let ndays_of_week_in ?(include_weekends = false) d start_date end_date =
  let open CalendarLib.Date in
  (match day_of_week d with
   | Mon -> ()
   | _ -> failwith "ndays_of_week_in needs a Monday!");
  let first_weekday = if d <= start_date then start_date else d in
  let last_day_of_week = add d (Period.day (if include_weekends then 6 else 4)) in
  let last_weekday =
    if last_day_of_week >= end_date then end_date else last_day_of_week
  in
  if first_weekday > last_weekday
  then 0
  else Period.nb_days (sub last_weekday first_weekday) + 1
;;

(** [asns] is a list of assignments which are grouped together on some basis.
    This function returns the total number of hours included in these
    assignments in a given week starting on date [d]. [d] must be a Monday. *)
let get_hours_per_week (d : CalendarLib.Date.t) (asns : assignment list) : float =
  let open CalendarLib.Date in
  (match day_of_week d with
   | Mon -> ()
   | _ -> failwith "get_hours_per_week needs a Monday!");
  let active_asns = asns |> List.filter (fun a -> not a.project.archived) in
  match active_asns with
  | [] -> 0.
  | a :: _ ->
    let is_placeholder =
      match a.entity with
      | Placeholder _ -> true
      | Person _ -> false
    in
    let get_hours_per_week_single (d : CalendarLib.Date.t) (a : assignment) =
      let ndays =
        ndays_of_week_in ~include_weekends:is_placeholder d a.start_date a.end_date
      in
      let hours_per_day = Float.of_int a.allocation /. 3600. in
      Float.of_int ndays *. hours_per_day
    in
    active_asns |> List.map (get_hours_per_week_single d) |> Utils.sum
;;

(* -------------- *)
(* Project export *)
(* -------------- *)

(** [asns] is a group of assignments belonging to the same client, project, and
    Forecast entity. Each group of assignments corresponds to one row in the CSV
    file. *)
let make_assignment_output weeks asns =
  (* Get client, project, people data from the first assignment *)
  let a = List.hd asns in

  let a_str = show_assignment a in
  let code =
    match a.project.code with
    | Some c -> c
    | None -> failwith ("project code not found for assignment: " ^ a_str)
  in
  let client_name =
    match a.project.client with
    | Some c -> c.name
    | None -> ""
  in

  (* Get hours per week data from all assignments *)
  [ client_name
  ; a.project.name
  ; code
  ; a.project.color
  ; a.project.tags |> List.sort compare |> String.concat ", "
  ; ForecastRaw.get_entity_name a.entity
  ; a.entity |> ForecastRaw.get_entity_roles |> List.sort compare |> String.concat ", "
  ]
  @ List.map (fun w -> Printf.sprintf "%.1f" (get_hours_per_week w asns)) weeks
;;

(** Export the project schedule between the given dates. *)
let export_project_schedule ~start_date ~end_date =
  let _, _, _, _, assignments = ForecastRaw.get_the_schedule ~start_date ~end_date in
  let weeks = Utils.get_xdays_between ~day:Mon ~start_date ~end_date in

  let header =
    [ "Client"
    ; "Project"
    ; "Project Code"
    ; "Project Label"
    ; "Project Tags"
    ; "Person"
    ; "Roles"
    ]
    @ List.map (CalendarLib.Printer.Date.sprint "%Y-%m-%d") weeks
  in
  let compare_assignments (a1 : assignment) (a2 : assignment) =
    (* Sort first by client, then by project, then by entity name *)
    let c1 =
      match a1.project.client with
      | Some c -> c.name
      | None -> ""
    in
    let c2 =
      match a2.project.client with
      | Some c -> c.name
      | None -> ""
    in
    match compare c1 c2 with
    | 0 ->
      (match compare a1.project.name a2.project.name with
       | 0 ->
         (match a1.entity, a2.entity with
          | Person p1, Person p2 -> compare (make_person_name p1) (make_person_name p2)
          | Placeholder p1, Placeholder p2 -> compare p1.name p2.name
          | Person _, Placeholder _ -> -1
          | Placeholder _, Person _ -> 1)
       | m -> m)
    | n -> n
  in
  let data =
    assignments
    |> List.filter (fun a ->
      (not a.project.archived) && not (ForecastRaw.get_entity_archived a.entity))
    |> List.sort compare_assignments
    |> Utils.group_by (fun a1 a2 -> compare_assignments a1 a2 = 0)
    |> List.map (make_assignment_output weeks)
  in
  header :: data
;;

(* ----------- *)
(* Team export *)
(* ----------- *)

let make_entity_row entity weeks assignments =
  let name = get_entity_name entity in
  let roles = get_entity_roles entity |> List.sort compare |> String.concat ", " in
  let assignments_to_this = assignments |> List.filter (fun a -> a.entity = entity) in
  let capacity =
    match entity with
    | Placeholder _ -> ""
    | Person p -> Printf.sprintf "%.1f" (p.weekly_capacity /. 3600.)
  in
  let hours_per_week =
    weeks
    |> List.map (fun w -> get_hours_per_week w assignments_to_this)
    |> List.map (fun f -> Printf.sprintf "%.1f" f)
  in
  name :: roles :: capacity :: hours_per_week
;;

(** Export the team schedule between the given dates. *)
let export_team_schedule ~start_date ~end_date =
  let _, people, placeholders, _, assignments =
    ForecastRaw.get_the_schedule ~start_date ~end_date
  in
  let weeks = Utils.get_xdays_between ~day:Mon ~start_date ~end_date in
  let header =
    [ "Person"; "Roles"; "Capacity" ]
    @ List.map (CalendarLib.Printer.Date.sprint "%Y-%m-%d") weeks
  in
  let placeholder_rows =
    placeholders
    |> IntMap.bindings
    |> List.map snd
    |> List.filter (fun (p : placeholder) -> not p.archived)
    |> List.sort (fun p1 p2 -> compare p1.name p2.name)
    |> List.map (fun p -> make_entity_row (Placeholder p) weeks assignments)
  in
  let people_rows =
    people
    |> IntMap.bindings
    |> List.map snd
    |> List.filter (fun (p : person) -> not p.archived)
    |> List.sort (fun p1 p2 -> compare (make_person_name p1) (make_person_name p2))
    |> List.map (fun p -> make_entity_row (Person p) weeks assignments)
  in
  header :: (placeholder_rows @ people_rows)
;;
