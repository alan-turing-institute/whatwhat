open ForecastRaw
module DateMap = Map.Make (CalendarLib.Date)

type assignment_output =
  { client : ForecastRaw.client
  ; project : ForecastRaw.project
  ; entity : ForecastRaw.entity
  ; hours_per_week : float DateMap.t
  }

let make_assignment_output weeks a =
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

  ignore weeks;

  [ client_name
  ; a.project.name
  ; code
  ; a.project.color
  ; a.project.tags |> List.sort compare |> String.concat ", "
  ; ForecastRaw.get_entity_name a.entity
  ; a.entity |> ForecastRaw.get_entity_roles |> List.sort compare |> String.concat ", "
  ]
;;

let get_mondays_between ~start_date ~end_date =
  let open CalendarLib.Date in
  let first_monday =
    match day_of_week start_date with
    | Mon -> start_date
    | Tue -> rem start_date (Period.day 1)
    | Wed -> rem start_date (Period.day 2)
    | Thu -> rem start_date (Period.day 3)
    | Fri -> rem start_date (Period.day 4)
    | Sat -> rem start_date (Period.day 5)
    | Sun -> rem start_date (Period.day 6)
  in
  let last_sunday =
    match day_of_week end_date with
    | Mon -> add end_date (Period.day 6)
    | Tue -> add end_date (Period.day 5)
    | Wed -> add end_date (Period.day 4)
    | Thu -> add end_date (Period.day 3)
    | Fri -> add end_date (Period.day 2)
    | Sat -> add end_date (Period.day 1)
    | Sun -> end_date
  in
  let rec acc monday dates =
    if monday > last_sunday
    then dates
    else acc (add monday (Period.week 1)) (monday :: dates)
  in
  acc first_monday []
;;

let is_active (asn : assignment) : bool =
  (not asn.project.archived) && not (ForecastRaw.get_entity_archived asn.entity)
;;

let export_schedule ~start_date ~end_date =
  let _, _, _, _, assignments = ForecastRaw.get_the_schedule ~start_date ~end_date in

  let weeks = get_mondays_between ~start_date ~end_date in

  (* TODO merge assignments of the same person and same project *)
  (* TODO add weeks *)

  let header =
    [ "Client"
    ; "Project"
    ; "Project Code"
    ; "Project Label"
    ; "Project Tags"
    ; "Person"
    ; "Roles"
    ]
  in
  let data =
    assignments |> List.filter is_active
    |> List.map (make_assignment_output weeks)
    (* Sort on project *)
    |> List.stable_sort (fun a b -> compare (List.nth a 1) (List.nth b 1))
    (* Sort on client *)
    |> List.stable_sort (fun a b -> compare (List.nth a 0) (List.nth b 0))
  in
  header :: data
;;
