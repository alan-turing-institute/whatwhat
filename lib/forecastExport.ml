open ForecastRaw
module DateMap = Map.Make (CalendarLib.Date)

type assignment_output =
  { client : ForecastRaw.client
  ; project : ForecastRaw.project
  ; person : ForecastRaw.person
  ; hours_per_week : float DateMap.t
  }

let make_assignment_output clients people placeholders projects weeks a =
  (* NOTE: None of the failwith's should really occur, exsept for the 'project
   * client was None' one (that can only happen if the ignoredForecastProjects.
   * does not include the blank project, i.e. time off). They are just here to
   * ensure that we get nicer errors just in case it really happens.

   * A better strategy would be to keep the full types in ForecastRaw (i.e. an
   * assignment.person should be a person, not just their id. *)

  let a_str = show_assignment a in

  let project = match IdMap.find_opt a.project_id projects with
  | Some proj -> proj
  | None      -> failwith ("project was not found for assignment: " ^ a_str) in

  let client = match project.client_id with
  | None -> failwith ("project client was None for assignment: " ^ a_str)
  | Some c -> match IdMap.find_opt c clients with
              | Some clnt -> clnt
              | None      -> failwith ("project client not found for assignment:
                " ^ a_str) in

  let entity = match a.entity with
  | Person p -> (match IdMap.find_opt p people with
                | Some prsn -> prsn
                | None      -> failwith ("person not found for assignment: " ^ a_str))
  | Placeholder p -> (match IdMap.find_opt p placeholders with
                | Some plch -> plch
                | None      -> failwith ("placeholder not found for assignment: " ^ a_str)) in

  ignore weeks;
  ignore client;
  ignore entity;
  ()


let get_mondays_between ~start_date ~end_date =
  let open CalendarLib.Date in
  let first_monday = match day_of_week start_date with
  | Mon -> start_date
  | Tue -> rem start_date (Period.day 1)
  | Wed -> rem start_date (Period.day 2)
  | Thu -> rem start_date (Period.day 3)
  | Fri -> rem start_date (Period.day 4)
  | Sat -> rem start_date (Period.day 5)
  | Sun -> rem start_date (Period.day 6)
  in
  let last_sunday = match day_of_week end_date with
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
   
let export_schedule ~start_date ~end_date output_file =
  (* let open Csv in *)

  let clients, people, placeholders, projects, assignments =
    ForecastRaw.get_the_schedule ~start_date ~end_date in

  ignore clients;
  ignore people;
  ignore placeholders;
  ignore projects;
  ignore assignments;
  ignore output_file;

  let weeks = get_mondays_between ~start_date ~end_date in
  List.iter CalendarLib.Printer.Date.dprint weeks;

  (* let csv = [] in *)
  (* save output_file csv *)


(* fields *)
(* Client
 * Project
 * Project Code
 * Project Label
 * Project Tags
 * Person
 * Roles
 * 2023-01-30 2023-02-06 2023-02-13 2023-02-20 2023-02-27 2023-03-06
 * 2023-03-13 2023-03-20 2023-03-27 2023-04-03 2023-04-10 2023-04-17
 * 2023-04-24
 *)
