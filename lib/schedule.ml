(* ---------------------------------------------------------------------- *)
(* TYPES *)

type fte_time = Github.fte_time =
  | FTEWeeks of float
  | FTEMonths of float
[@@deriving show]

type allocation = Forecast.allocation =
  { start_date : CalendarLib.Date.t [@printer DatePrinter.pp_print_date]
  ; end_date : CalendarLib.Date.t [@printer DatePrinter.pp_print_date]
  ; rate : float
  }
[@@deriving show]

type assignment = Forecast.assignment =
  { project : int
  ; person : string
  ; finance_code : string option
  ; allocations : allocation list
  }
[@@deriving show]

(* TODO Add fields for list of assignments and list of allocations *)
type person =
  { email : string
  ; name : string
  ; github_login : string
  }
[@@deriving show]

(* TODO Add fields for list of allocations, list of finance codes, programme *)
type project =
  { forecast_id : int
  ; github_id : int
  ; name : string
  ; github_assignees : string list
  ; reactions : (string * string) list
  ; column : string
  ; turing_project_code : string list option
  ; earliest_start_date : CalendarLib.Date.t option
       [@printer DatePrinter.pp_print_date_opt]
  ; latest_start_date : CalendarLib.Date.t [@printer DatePrinter.pp_print_date]
  ; latest_end_date : CalendarLib.Date.t option [@printer DatePrinter.pp_print_date_opt]
  ; fte_time : fte_time
  ; nominal_fte_percent : float
  ; max_fte_percent : float
  ; min_fte_percent : float
  }
[@@deriving show]

(* ---------------------------------------------------------------------- *)
(* MERGE PEOPLE FROM FORECAST AND GITHUB *)

let person_name (fc_p : Forecast.person) = fc_p.first_name ^ " " ^ fc_p.last_name

let compare_opts a b =
  match a, b with
  | Some x, Some y -> x = y
  | _ -> false
;;

(** Find the matching Github user for Forecast user fc_p.*)
let get_matching_gh_person_opt (gh_people : Github.person list) (fc_p : Forecast.person) =
  let person_matches (gh_p : Github.person) =
    let emails_match = compare_opts gh_p.email (Some fc_p.email) in
    let names_match = compare_opts gh_p.name (Some (person_name fc_p)) in
    emails_match || names_match
  in
  let gh_person = List.find_opt person_matches gh_people in
  let () =
    if gh_person = None
    then (
      let error_msg =
        "No matching Github user: " ^ person_name fc_p ^ " <" ^ fc_p.email ^ ">"
      in
      Log.log Log.Error Log.Schedule (Log.Person (person_name fc_p)) error_msg)
  in
  gh_person
;;

(** Create a list of all people, merging data from Forecast and Github. *)
let get_people_list (fc_people : Forecast.person list) (gh_people : Github.person list) =
  (* We fold over Forecast people, looking for the matching Github person for each,
   since Forecast is considered authoritative for people.*)
  let add_person (fc_p : Forecast.person) m =
    let gh_p_opt = get_matching_gh_person_opt gh_people fc_p in
    match gh_p_opt with
    | Some gh_p ->
      let new_person =
        { email = fc_p.email; name = person_name fc_p; github_login = gh_p.login }
      in
      m @ [ new_person ]
    | None -> m
  in
  List.fold_right add_person fc_people []
;;

(* ---------------------------------------------------------------------- *)
(* MERGE PROJECTS FROM FORECAST AND GITHUB *)

(** Find the Forecast project associated with a given Github issue. *)
let get_matching_fc_project
  (fc_projects : Forecast.project list)
  (gh_project : Github.project)
  =
  let project_matches (x : Forecast.project) = x.number = gh_project.number in
  let fc_p_opt = List.find_opt project_matches fc_projects in
  let () =
    if fc_p_opt = None
    then (
      let error_msg =
        "No Forecast project for Github issue " ^ Int.to_string gh_project.number
      in
      Log.log Log.Error Log.Schedule (Log.Project gh_project.number) error_msg)
  in
  fc_p_opt
;;

(** Find the [person] matching the given Github user. *)
let person_opt_of_gh_person (people : person list) (gh_person : Github.person) =
  let login_matches person = person.github_login = gh_person.login in
  let person_opt = List.find_opt login_matches people in
  if person_opt = None
  then (
    let error_msg =
      "People list doesn't have an entry for Github login " ^ gh_person.login
    in
    Log.log Log.Error Log.Schedule (Log.Person gh_person.login) error_msg);
  person_opt
;;

(** Create a list of all projects, merging data from Forecast and Github. *)
let get_project_list
  (fc_projects : Forecast.project list)
  (gh_issues : Github.project list)
  (people : person list)
  =
  (* We fold over Github projects, looking for the matching Forecast project for each,
   since Github is considered authoritative for people.*)
  let add_project (gh_project : Github.project) m =
    let fc_p_opt = get_matching_fc_project fc_projects gh_project in
    let get_person_opt = person_opt_of_gh_person people in
    match fc_p_opt with
    | Some fc_p ->
      let github_assignees =
        List.filter_map get_person_opt gh_project.assignees
        |> List.map (fun person -> person.email)
      in
      let reactions =
        List.filter_map
          (fun (emoji, gh_person) ->
            let person_opt = person_opt_of_gh_person people gh_person in
            match person_opt with
            | Some person -> Some (emoji, person.email)
            | None -> None)
          gh_project.reactions
      in
      let new_project =
        { forecast_id = fc_p.number
        ; github_id = gh_project.number
        ; name = gh_project.title
        ; github_assignees
        ; reactions
          (* TODO The Option.get is dangerous, handle failures more gracefully. *)
        ; column = Option.get gh_project.column
        ; turing_project_code = gh_project.metadata.turing_project_code
        ; earliest_start_date = gh_project.metadata.earliest_start_date
        ; latest_start_date = gh_project.metadata.latest_start_date
        ; latest_end_date = gh_project.metadata.latest_end_date
        ; fte_time = gh_project.metadata.fte_time
        ; nominal_fte_percent = gh_project.metadata.nominal_fte_percent
        ; max_fte_percent = gh_project.metadata.max_fte_percent
        ; min_fte_percent = gh_project.metadata.min_fte_percent
        }
      in
      m @ [ new_project ]
    | None -> m
  in
  List.fold_right add_project gh_issues []
;;

(* ---------------------------------------------------------------------- *)
(* BUILD SCHEDULE *)

(* TODO Finish this, by getting allocations as well.*)
let get_the_schedule () =
  let start_date =
    CalendarLib.Calendar.lmake ~year:2016 ~month:1 ~day:1 ()
    |> CalendarLib.Calendar.to_date
  in
  let end_date =
    CalendarLib.Date.add
      (CalendarLib.Date.today ())
      (CalendarLib.Date.Period.lmake ~year:1 ())
  in
  let fc_schedule = Forecast.get_the_schedule start_date end_date in
  (* Convert maps to lists. *)
  let fc_people = fc_schedule.people |> Forecast.StringMap.bindings |> List.map snd in
  let fc_projects = fc_schedule.projects |> Forecast.IntMap.bindings |> List.map snd in
  let gh_issues = Github.get_project_issues "Project Tracker" in
  let gh_people = Github.get_users () in
  let people = get_people_list fc_people gh_people in
  let projects = get_project_list fc_projects gh_issues people in
  let assignments = fc_schedule.assignments in
  people, projects, assignments
;;
