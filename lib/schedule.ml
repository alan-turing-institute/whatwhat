(* Merge data sources and provide a unified view *)

open Domain

(* ---------------------------------------------------------------------- *)
(* LOGGING *)

type schedule_error =
  | AllocationEndsTooLate of assignment
  | AllocationStartsTooEarly of assignment
  | FinanceCodeNotFound of assignment * string
  | NoMatchingGithubIssue of string
  | NoMatchingGithubUser of string
(*
  | NoMatchingForecastProject
  | NoMatchingForecastUser
  | NoProjectColumn
  *)

(** Log an error given the error type, Github issue number, and explanatory message.*)
let log (error : schedule_error) =
  let level =
    match error with
    | AllocationEndsTooLate _ -> Log.Warning
    | AllocationStartsTooEarly _ -> Log.Warning
    | FinanceCodeNotFound _ -> Log.Error
    | NoMatchingGithubIssue _ -> Log.Error
    | NoMatchingGithubUser _ -> Log.Warning
    (*
    | NoMatchingForecastProject -> Log.Error
    | NoMatchingForecastUser -> Log.Warning
    | NoProjectColumn -> Log.Error
    *)
  in
  let entity =
    match error with
    | AllocationEndsTooLate asg -> Log.Assignment (asg.project, asg.person)
    | AllocationStartsTooEarly asg -> Log.Assignment (asg.project, asg.person)
    | FinanceCodeNotFound (asg, _) -> Log.Assignment (asg.project, asg.person)
    | NoMatchingGithubIssue name -> Log.RawForecastProject name
    | NoMatchingGithubUser email -> Log.RawForecastPerson email
    (*
    | NoMatchingForecastProject -> Log.Error
    | NoMatchingForecastUser -> Log.Warning
    | NoProjectColumn -> Log.Error
    *)
  in
  let msg =
    match error with
    | AllocationEndsTooLate asg ->
      "Assignment of "
      ^ asg.person
      ^ " to project "
      ^ Int.to_string asg.project
      ^ " has an end date after project latest end"
    | AllocationStartsTooEarly asg ->
      "Assignment of "
      ^ asg.person
      ^ " to project "
      ^ Int.to_string asg.project
      ^ " has a start date before project latest end"
    | FinanceCodeNotFound (asg, code) ->
      "Assignment of "
      ^ asg.person
      ^ " to project "
      ^ Int.to_string asg.project
      ^ " has an unknown finance code: "
      ^ code
    | NoMatchingGithubIssue name ->
      "No matching Github issue for Forecast project " ^ name
    | NoMatchingGithubUser name -> "No matching Github user for " ^ name
    (*
    | NoMatchingForecastProject -> "No Forecast project for Github issue "
    | NoMatchingForecastUser -> "People list doesn't have an entry for Github login "
    | NoProjectColumn -> "Github issue has no project column: "
    *)
  in
  Log.log level Log.Schedule entity msg
;;

(* ---------------------------------------------------------------------- *)
(* MERGE PEOPLE FROM FORECAST AND GITHUB *)

let compare_opts a b =
  match a, b with
  | Some x, Some y -> x = y
  | _ -> false
;;

(** Find the matching Github user for Forecast user fc_p.*)
let get_matching_gh_person_opt (gh_people : Github.person list) (fc_p : person) =
  let person_matches (gh_p : Github.person) =
    let emails_match = compare_opts gh_p.email (Some fc_p.email) in
    let names_match = compare_opts gh_p.name (Some fc_p.full_name) in
    emails_match || names_match
  in
  let gh_person = List.find_opt person_matches gh_people in
  if gh_person = None then log (NoMatchingGithubUser fc_p.email);
  gh_person
;;

(** Create a list of all people, merging data from Forecast and Github. *)
let get_people_list (fc_people : person list) (gh_people : Github.person list) =
  (* We fold over Forecast people, looking for the matching Github person for each,
     since Forecast is considered authoritative for people. *)
  let add_person (fc_p : person) m =
    let gh_p_opt = get_matching_gh_person_opt gh_people fc_p in
    let login_opt =
      match gh_p_opt with
      | Some gh_p -> Some gh_p.login
      | None -> None
    in
    let new_person =
      { email = fc_p.email
      ; full_name = fc_p.full_name
      ; github_handle = login_opt
      ; slack_handle = None
      }
    in
    m @ [ new_person ]
  in
  List.fold_right add_person fc_people []
;;

(* ---------------------------------------------------------------------- *)
(* MERGE PROJECTS FROM FORECAST AND GITHUB *)

(*
(** Find the Forecast project associated with a given Github issue. *)
let get_matching_fc_project 
   (fc_projects : Forecast.project list) 
   (gh_project : project) 
   = 
   let project_matches (x : Forecast.project) = x.number = gh_project.nmbr in 
   let fc_p_opt = List.find_opt project_matches fc_projects in 
   if fc_p_opt = None 
   then log NoMatchingForecastProject @@ Int.to_string gh_project.nmbr;
   fc_p_opt 
 ;; 
*)

(*
(** Find the [person] matching the given Github user. *)
let person_opt_of_gh_person (people : person list) (gh_person : Github.person) = 
  let login_matches person = person.github_handle = Some gh_person.login in 
  let person_opt = List.find_opt login_matches people in 
  if person_opt = None 
  if person_opt = None then log NoMatchingForecastUser gh_person.login;
  person_opt 
;;
*)

(* Check that each Forecast project has a hut23 code which matches that of a
   Github project

   In addition, this function should (but currently does not)
   - check that the "Client" on Forecast is the same as the "Programme" on GitHub
   - (maybe) check that the name is identical
   - replace the people in "assignees" and "emojis" with the matching person from Forecast

   TODO: This is O(N^2). Tut tut. 
 *)
let merge_projects (fc_projects : Forecast.project list) (gh_issues : project list) =
  let check_project_exists (fc_p : Forecast.project) =
    if not (List.exists (fun p -> p.nmbr = fc_p.number) gh_issues)
    then log (NoMatchingGithubIssue fc_p.name)
  in
  List.iter check_project_exists fc_projects;
  gh_issues
;;

(* OLD CODE *)
(*
  (* We fold over Github projects, looking for the matching Forecast project for each,
   since Github is considered authoritative for projects.*)
  let add_project (gh_project : project) m =
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
  *)

(* ---------------------------------------------------------------------- *)
(* BUILD SCHEDULE *)

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
  let fc_projects, fc_people, assignments =
    let fcpp, fcpr, fcas = Forecast.get_the_schedule start_date end_date in
    ( fcpp |> Forecast.IntMap.bindings |> List.map snd
    , fcpr |> Forecast.StringMap.bindings |> List.map snd
    , fcas )
  in
  let gh_issues = Github.get_project_issues @@ Config.get_github_project_name () in
  let gh_people = Github.get_users () in
  let people = get_people_list fc_people gh_people in
  let projects = merge_projects fc_projects gh_issues in
  people, projects, assignments
;;

(* ---------------------------------------------------------------------- *)
(* CHECKS ON SCHEDULE *)

let check_finance_code (prj : project) (asg : assignment) =
  let codes_match =
    Option.is_none asg.finance_code
    || List.mem (Option.get asg.finance_code) prj.plan.finance_codes
  in
  if not codes_match
     (* The Option.get is safe, because the above match guarantees that asg.finance_code
        isn't None. *)
  then log (FinanceCodeNotFound (asg, Option.get asg.finance_code))
;;

let check_end_date (prj : project) (asg : assignment) =
  match prj.plan.latest_end_date with
  | None -> ()
  | Some end_date ->
    let check_simple_allocation simple_aln =
      if CalendarLib.Date.add simple_aln.start_date simple_aln.days > end_date
      then log (AllocationEndsTooLate asg)
    in
    List.iter check_simple_allocation asg.allocation
;;

let check_start_date (prj : project) (asg : assignment) =
  match prj.plan.earliest_start_date with
  | None -> ()
  | Some start_date ->
    let check_simple_allocation simple_aln =
      if simple_aln.start_date < start_date then log (AllocationStartsTooEarly asg)
    in
    List.iter check_simple_allocation asg.allocation
;;

let check_assignment people projects (asg : assignment) =
  let prj = List.find (fun (prj : project) -> prj.nmbr = asg.project) projects in
  let () = check_finance_code prj asg in
  let () = check_end_date prj asg in
  let () = check_start_date prj asg in
  ()
;;

let check_assignments people projects assignments =
  List.iter (check_assignment people projects) assignments
;;
