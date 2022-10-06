(* ---------------------------------------------------------------------- *)
(* TYPES *)

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
  ; github_login : string option
  }
[@@deriving show]

(* TODO Add fields for list of allocations, list of finance codes, programme *)
type project =
  { forecast_id : int
  ; github_id : int
  ; name : string
  ; github_assignees : string option list
  ; reactions : (string * string option) list
  ; column : string
  ; finance_code : string option
  ; earliest_start_date : CalendarLib.Date.t option
       [@printer DatePrinter.pp_print_date_opt]
  ; latest_start_date : CalendarLib.Date.t option [@printer DatePrinter.pp_print_date_opt]
  ; latest_end_date : CalendarLib.Date.t option [@printer DatePrinter.pp_print_date_opt]
  ; fte_months : float option
  ; nominal_fte_percent : float option
  ; max_fte_percent : float option
  ; min_fte_percent : float option
  }
[@@deriving show]

(* ---------------------------------------------------------------------- *)
(* LOGGING *)

type schedule_error =
  | AllocationEndsTooLate
  | AllocationStartsTooEarly
  | FinanceCodeNotFound
  | NoMatchingForecastProject
  | NoMatchingForecastUser
  | NoMatchingGithubIssue
  | NoMatchingGithubUser
  | NoProjectColumn

(** Log an error given the error type, Github issue number, and explanatory message.*)
let log (error_type : schedule_error) msg =
  let prefix = "Schedule: " in
  let log_lvl =
    match error_type with
    | AllocationEndsTooLate -> Log.Warning
    | AllocationStartsTooEarly -> Log.Warning
    | FinanceCodeNotFound -> Log.Error
    | NoMatchingForecastProject -> Log.Error
    | NoMatchingForecastUser -> Log.Warning
    | NoMatchingGithubIssue -> Log.Error
    | NoMatchingGithubUser -> Log.Warning
    | NoProjectColumn -> Log.Error
  in
  let error_description =
    match error_type with
    | AllocationEndsTooLate -> "Allocation has an end date after project latest end: "
    | AllocationStartsTooEarly ->
      "Allocation has a start date before project earliest start: "
    | FinanceCodeNotFound -> "Assignment has a finance code not found on Github: "
    | NoMatchingForecastProject -> "No Forecast project for Github issue "
    | NoMatchingForecastUser -> "People list doesn't have an entry for Github login "
    | NoMatchingGithubIssue -> "No matching Github issue for Forecast project "
    | NoMatchingGithubUser -> "No matching Github user for "
    | NoProjectColumn -> "Github issue has no project column: "
  in
  Log.log log_lvl @@ prefix ^ error_description ^ msg
;;

(* ---------------------------------------------------------------------- *)
(* MERGE PEOPLE FROM FORECAST AND GITHUB *)

let compare_opts a b =
  match a, b with
  | Some x, Some y -> x = y
  | _ -> false
;;

(** Return a boolean for whether the Forecast and Github people provided represent the
    same meat sack. *)
let person_matches (fc_p : Forecast.person) (gh_p : Github.person) =
  let emails_match = compare_opts gh_p.email (Some fc_p.email) in
  let names_match = compare_opts gh_p.name (Some (Forecast.person_name fc_p)) in
  emails_match || names_match
;;

(** Find the matching Github user for Forecast user fc_p.*)
let get_matching_gh_person_opt (gh_people : Github.person list) (fc_p : Forecast.person) =
  let gh_person = List.find_opt (person_matches fc_p) gh_people in
  if gh_person = None
  then (
    let fc_p_str = Forecast.person_name fc_p ^ " <" ^ fc_p.email ^ ">" in
    log NoMatchingGithubUser fc_p_str);
  gh_person
;;

(** Create a list of all people, merging data from Forecast and Github. *)
let get_people_list (fc_people : Forecast.person list) (gh_people : Github.person list) =
  (* We fold over Forecast people, looking for the matching Github person for each,
   since Forecast is considered authoritative for people. *)
  let add_person (fc_p : Forecast.person) m =
    let gh_p_opt = get_matching_gh_person_opt gh_people fc_p in
    let login_opt =
      match gh_p_opt with
      | Some gh_p -> Some gh_p.login
      | None -> None
    in
    let new_person =
      { email = fc_p.email; name = Forecast.person_name fc_p; github_login = login_opt }
    in
    m @ [ new_person ]
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
  if fc_p_opt = None then log NoMatchingForecastProject @@ Int.to_string gh_project.number;
  fc_p_opt
;;

(** Find the [person] matching the given Github user. *)
let person_opt_of_gh_person (people : person list) (gh_person : Github.person) =
  let login_matches person = person.github_login = Some gh_person.login in
  let person_opt = List.find_opt login_matches people in
  if person_opt = None then log NoMatchingForecastUser gh_person.login;
  person_opt
;;

(** Check that there aren't projects on Forecast that don't match any known Github issue.

    This function only logs errors if necessary, and returns [()]. *)
let check_extra_fc_projects
  (fc_projects : Forecast.project list)
  (gh_issues : Github.project list)
  =
  let gh_issue_exists (fc_p : Forecast.project) =
    if List.exists (fun (gh_p : Github.project) -> fc_p.number = gh_p.number) gh_issues
    then ()
    else log NoMatchingGithubIssue @@ Int.to_string fc_p.number
  in
  List.iter gh_issue_exists fc_projects
;;

(** Create a list of all projects, merging data from Forecast and Github. *)
let get_project_list
  (fc_projects : Forecast.project list)
  (gh_issues : Github.project list)
  (people : person list)
  =
  let () = check_extra_fc_projects fc_projects gh_issues in
  (* We fold over Github projects, looking for the matching Forecast project for each,
   since Github is considered authoritative for projects.*)
  let add_project (gh_project : Github.project) m =
    let fc_p_opt = get_matching_fc_project fc_projects gh_project in
    let get_person_email_opt (gh_person : Github.person) =
      match person_opt_of_gh_person people gh_person with
      | Some person -> Some person.email
      | None -> None
    in
    match fc_p_opt, gh_project.column with
    | Some fc_p, Some column ->
      let github_assignees = List.map get_person_email_opt gh_project.assignees in
      let reactions =
        List.map
          (fun (emoji, gh_person) -> emoji, get_person_email_opt gh_person)
          gh_project.reactions
      in
      let new_project =
        { forecast_id = fc_p.number
        ; github_id = gh_project.number
        ; name = gh_project.title
        ; github_assignees
        ; reactions
        ; column
        ; finance_code = gh_project.metadata.turing_project_code
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
    | _, None ->
      log NoProjectColumn @@ Int.to_string gh_project.number;
      m
    | _ -> m
  in
  List.fold_right add_project gh_issues []
;;

(* ---------------------------------------------------------------------- *)
(* BUILD SCHEDULE *)

let get_the_schedule () =
  let fc_schedule = Forecast.get_the_current_schedule 180 in
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

(* ---------------------------------------------------------------------- *)
(* CHECKS ON SCHEDULE *)

let check_finance_code (prj : project) (asg : assignment) =
  let codes_match =
    match asg.finance_code, prj.finance_code with
    | Some afc, Some pfc -> afc = pfc
    | None, _ -> true
    | _ -> false
  in
  if not codes_match
  then
    (* The Option.get is safe, because the above match guarantees that
       assignment.finance_code isn't None. *)
    log FinanceCodeNotFound
    @@ Option.get asg.finance_code
    ^ ", issue "
    ^ Int.to_string prj.github_id
;;

let check_end_date (prj : project) (asg : assignment) =
  match prj.latest_end_date with
  | None -> ()
  | Some end_date ->
    List.iter
      (fun all ->
        if end_date < all.end_date
        then log AllocationEndsTooLate @@ CalendarLib.Printer.Date.to_string all.end_date)
      asg.allocations
;;

let check_start_date (prj : project) (asg : assignment) =
  match prj.latest_start_date with
  | None -> ()
  | Some start_date ->
    List.iter
      (fun all ->
        if start_date > all.start_date
        then
          log AllocationStartsTooEarly
          @@ CalendarLib.Printer.Date.to_string all.start_date)
      asg.allocations
;;

let check_assignment people projects (asg : assignment) =
  let p = List.find (fun (prj : project) -> prj.github_id = asg.project) projects in
  let () = check_finance_code prj asg in
  let () = check_end_date prj asg in
  let () = check_start_date prj asg in
  ()
;;

let check_assignments people projects assignments =
  List.iter (check_assignment people projects) assignments
;;
