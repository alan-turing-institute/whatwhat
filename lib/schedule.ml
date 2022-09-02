(* ---------------------------------------------------------------------- *)
(* TYPES *)

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
  ; assignees : string list
  ; reactions : (string * string) list
  ; column : string
  ; turing_project_code : string option
  ; earliest_start_date : CalendarLib.Date.t [@printer DatePrinter.pp_print_date]
  ; latest_start_date : CalendarLib.Date.t [@printer DatePrinter.pp_print_date]
  ; latest_end_date : CalendarLib.Date.t option [@printer DatePrinter.pp_print_date_opt]
  ; fte_months : float option
  ; nominal_fte_percent : float option
  ; max_fte_percent : float option
  ; min_fte_percent : float option
  }
[@@deriving show]

type allocation =
  { person_id : int
  ; project_id : int
  ; start_date : CalendarLib.Date.t
  ; end_date : CalendarLib.Date.t
  ; rate : float
  }

(* ---------------------------------------------------------------------- *)

let compare_string_opts (a : string option) (b : string option) =
  match a, b with
  | Some x, Some y -> x = y
  | _ -> false
;;

(* Find the matching Github user for Forecast user fc_p. Log an error and return None if
  not found.*)
let get_matching_gh_person (gh_people : Github.person list) (fc_p : Forecast.person) =
  let comp (gh_p : Github.person) =
    let emails_match = compare_string_opts gh_p.email (Some fc_p.email) in
    let names_match = compare_string_opts gh_p.name (Some (Forecast.person_name fc_p)) in
    emails_match || names_match
  in
  let result_opt = gh_people |> List.find_opt comp in
  match result_opt with
  | Some result -> Some result
  | None ->
    let error_msg =
      "No matching Github user: " ^ Forecast.person_name fc_p ^ " <" ^ fc_p.email ^ ">"
    in
    let () = Log.log Log.Error error_msg in
    None
;;

(* Make a map of Turing email addresses to people. *)
let get_people_list fc_people (gh_people : Github.person list) =
  let add_person (fc_p : Forecast.person) m =
    let gh_p_opt = get_matching_gh_person gh_people fc_p in
    match gh_p_opt with
    | Some gh_p ->
      let new_person =
        { email = fc_p.email
        ; name = Forecast.person_name fc_p
        ; github_login = gh_p.login
        }
      in
      m @ [ new_person ]
    | None -> m
  in
  List.fold_right add_person fc_people []
;;

(* Find the Forecast project associated with a given Github issue. *)
let get_matching_fc_project
  (fc_projects : Forecast.project list)
  (gh_project : Github.project)
  =
  let project_matches (x : Forecast.project) = x.number = gh_project.number in
  let fc_p_opt = List.find_opt project_matches fc_projects in
  match fc_p_opt with
  | Some found_fc_project -> Some found_fc_project
  | None ->
    let error_msg =
      "No Forecast project for Github issue " ^ Int.to_string gh_project.number
    in
    let () = Log.log Log.Error error_msg in
    None
;;

(* Find, from the email->person map, the person matching the given Github user. *)
let person_opt_of_gh_person (people : person list) (gh_person : Github.person) =
  let login = gh_person.login in
  let login_matches person = person.github_login = login in
  match List.find_opt login_matches people with
  | Some found_person -> Some found_person
  | None ->
    let error_msg = "People map doesn't have an entry for Github login " ^ login in
    let () = Log.log Log.Error error_msg in
    None
;;

(* Make a map of Github issue ID to project. *)
let get_project_list
  (fc_projects : Forecast.project list)
  (gh_issues : Github.project list)
  (people : person list)
  =
  let add_project (gh_project : Github.project) m =
    let fc_p_opt = get_matching_fc_project fc_projects gh_project in
    let get_person_opt = person_opt_of_gh_person people in
    match fc_p_opt with
    | Some fc_p ->
      let assignees =
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
        ; assignees
        ; reactions
          (* TODO The Option.get is dangerous, handle failures more gracefully.
         *)
        ; column = Option.get gh_project.column
        ; turing_project_code = gh_project.metadata.turing_project_code
        ; earliest_start_date = gh_project.metadata.earliest_start_date
        ; latest_start_date = gh_project.metadata.latest_start_date
        ; latest_end_date = gh_project.metadata.latest_end_date
        ; fte_months = gh_project.metadata.fte_months
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

let make_schedule () =
  let fc_schedule = Forecast.getTheCurrentSchedule 180 in
  (* Convert maps to lists. *)
  let fc_people = fc_schedule.people |> Forecast.StringMap.bindings |> List.map snd in
  let fc_projects = fc_schedule.projects |> Forecast.IntMap.bindings |> List.map snd in
  let gh_issues = Github.get_project_issues "Project Tracker" in
  let gh_people = Github.get_users () in
  let people = get_people_list fc_people gh_people in
  let projects = get_project_list fc_projects gh_issues people in
  people, projects
;;
