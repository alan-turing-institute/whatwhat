(* Merge data sources and provide a unified view *)

open Domain

(* ---------------------------------------------------------------------- *)
(* LOGGING *)

type schedule_event =
  | MissingGithubProjectError of Forecast.project (* E3001 *)
  | FinanceCodeNotMatchingError of project (* E3002 *)
  | MissingForecastProjectWarning of project (* W3001 *)
  | AllocationEndsTooLateWarning of assignment (* W3002 *)
  | AllocationStartsTooEarlyWarning of assignment (*W3003 *)
  | ActiveProjectWithoutAssignmentWarning of project (* W3007 *)
  | ProjectStartOverdueWarning of project (* W3009 *)
  | NoMatchingGithubUserWarning of Forecast.person (* W3010 *)
  | DifferentClientWarning of project (* W3011 *)
  | DifferentNameWarning of project (* W3012 *)
  | AssignmentWithoutProjectDebug of Forecast.assignment
  | AssignmentWithoutPersonDebug of Forecast.assignment

let log_event (error : schedule_event) =
  match error with
  | MissingGithubProjectError fc_proj ->
    Log.log'
      { level = Log.Error' 3001
      ; source = Log.Schedule
      ; entity = Log.Project fc_proj.number
      ; message =
          Printf.sprintf
            "No matching GitHub issue for Forecast project <%s>."
            fc_proj.name
      }
  | FinanceCodeNotMatchingError proj ->
    Log.log'
      { level = Log.Error' 3002
      ; source = Log.Schedule
      ; entity = Log.Project proj.number
      ; message = Printf.sprintf "Finance codes on Forecast and GitHub do not match."
      }
  | MissingForecastProjectWarning proj ->
    Log.log'
      { level = Log.Warning 3001
      ; source = Log.Schedule
      ; entity = Log.Project proj.number
      ; message = Printf.sprintf "No matching Forecast project found."
      }
  | AllocationEndsTooLateWarning asn ->
    Log.log'
      { level = Log.Warning 3002
      ; source = Log.Schedule
      ; entity = Log.ForecastProject asn.project.number
      ; message =
          Printf.sprintf
            "Assignment of <%s> ends after project latest end."
            asn.person.full_name
      }
  | AllocationStartsTooEarlyWarning asn ->
    Log.log'
      { level = Log.Warning 3003
      ; source = Log.Schedule
      ; entity = Log.ForecastProject asn.project.number
      ; message =
          Printf.sprintf
            "Assignment of <%s> begins before project earliest start."
            asn.person.full_name
      }
  | ActiveProjectWithoutAssignmentWarning proj ->
    Log.log'
      { level = Log.Warning 3007
      ; source = Log.Schedule
      ; entity = Log.Project proj.number
      ; message = Printf.sprintf "Project is Active (or later) but has no assignments."
      }
  | ProjectStartOverdueWarning proj ->
    Log.log'
      { level = Log.Warning 3009
      ; source = Log.Schedule
      ; entity = Log.Project proj.number
      ; message = Printf.sprintf "Project is past latest start date, but not yet Active."
      }
  | NoMatchingGithubUserWarning person ->
    Log.log'
      { level = Log.Warning 3010
      ; source = Log.Schedule
      ; entity = Log.ForecastPerson person.email
      ; message =
          Printf.sprintf "Could not find matching GitHub user for <%s>." person.full_name
      }
  | DifferentClientWarning proj ->
    Log.log'
      { level = Log.Warning 3011
      ; source = Log.Schedule
      ; entity = Log.Project proj.number
      ; message = Printf.sprintf "Project programmes on Forecast and GitHub do not match."
      }
  | DifferentNameWarning proj ->
    Log.log'
      { level = Log.Warning 3012
      ; source = Log.Schedule
      ; entity = Log.Project proj.number
      ; message = Printf.sprintf "Project name does not match on Forecast and GitHub."
      }
  | AssignmentWithoutProjectDebug asn ->
    Log.log'
      { level = Log.Debug
      ; source = Log.Schedule
      ; entity = Log.ForecastProject asn.project.number
      ; message = Printf.sprintf "Assignment made to project that has been deleted."
      }
  | AssignmentWithoutPersonDebug asn ->
    Log.log'
      { level = Log.Debug
      ; source = Log.Schedule
      ; entity = Log.ForecastPerson asn.person.full_name
      ; message = Printf.sprintf "Assignment made to person that has been deleted."
      }
;;

(* ---------------------------------------------------------------------- *)
(* MERGE PEOPLE FROM FORECAST AND GITHUB *)

(** Find the matching Github user for Forecast user fc_p.*)
let get_matching_gh_person_opt (gh_people : Github.person list) (fc_p : Forecast.person) =
  let person_matches (gh_p : Github.person) =
    gh_p.email = Some fc_p.email || gh_p.name = Some fc_p.full_name
  in
  let gh_person = List.find_opt person_matches gh_people in
  if gh_person = None then log_event (NoMatchingGithubUserWarning fc_p);
  gh_person
;;

(** Create a list of all people, merging data from Forecast and Github. *)
let merge_people (fc_people : Forecast.person list) (gh_people : Github.person list) =
  (* We map over Forecast people, looking for the matching Github person for
     each, since Forecast is considered authoritative for people. *)
  (* TODO: Get Slack handle. *)
  let merge_person (fc_p : Forecast.person) =
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
    new_person
  in
  List.map merge_person fc_people
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

type project_pair = Pair of (Forecast.project option * project option)

(* Check that each Forecast project has a hut23 code which matches that of a
   GitHub project. Additionally, check that the programmes and names of the
   projects are the same on both platforms.
 *)
let merge_projects
  (fc_projects : Forecast.project IntMap.t)
  (gh_issues : project IntMap.t)
  =
  let pair_projects _ fc_opt gh_opt =
    match fc_opt, gh_opt with
    | None, None -> None
    | x, y -> Some (Pair (x, y))
  in
  let combined_map = IntMap.merge pair_projects fc_projects gh_issues in
  let check_projects _ (pair : project_pair) =
    (* Check that they both exist *)
    match pair with
    | Pair (None, None) -> ()
    | Pair (Some fc_p, None) -> log_event (MissingGithubProjectError fc_p)
    | Pair (None, Some gh_p) -> log_event (MissingForecastProjectWarning gh_p)
    | Pair (Some fc_p, Some gh_p) ->
      (* Check that their client/programme match *)
      if Some fc_p.programme <> gh_p.programme
      then log_event (DifferentClientWarning gh_p);
      (* Check that their names match *)
      if fc_p.name <> gh_p.name then log_event (DifferentNameWarning gh_p);
      (* Check that their project codes match *)
      let finance_codes_match =
        match fc_p.finance_code with
        | None -> false
        | Some cd -> List.mem cd gh_p.plan.finance_codes
      in
      if not finance_codes_match then log_event (FinanceCodeNotMatchingError gh_p)
  in
  IntMap.iter check_projects combined_map;
  gh_issues
;;

(* ---------------------------------------------------------------------- *)
(* MERGE ASSIGNMENTS FROM FORECAST AND GITHUB *)

let check_end_date (prj : project) (asg : assignment) =
  match prj.plan.latest_end_date with
  | None -> ()
  | Some latest_end_date ->
    if get_last_day asg.allocation > latest_end_date
    then log_event (AllocationEndsTooLateWarning asg)
;;

let check_start_date (prj : project) (asg : assignment) =
  match prj.plan.earliest_start_date with
  | None -> ()
  | Some earliest_start_date ->
    if get_first_day asg.allocation < earliest_start_date
    then log_event (AllocationStartsTooEarlyWarning asg)
;;

let merge_assignment people projects (asn : Forecast.assignment) : assignment option =
  match IntMap.find_opt asn.project.number projects with
  | None ->
    log_event (AssignmentWithoutProjectDebug asn);
    None
  | Some prj ->
    (match List.find_opt (fun p -> p.full_name = asn.person.full_name) people with
     | None ->
       log_event (AssignmentWithoutPersonDebug asn);
       None
     | Some psn ->
       let new_asn = { project = prj; person = psn; allocation = asn.allocation } in
       check_start_date prj new_asn;
       check_end_date prj new_asn;
       Some new_asn)
;;

(* ---------------------------------------------------------------------- *)
(* CHECKS ON SCHEDULE *)

module IntMap = Map.Make (Int)

let today = CalendarLib.Date.today ()

let check_is_overdue prj =
  if prj.plan.latest_start_date < today && prj.state < Active
  then log_event (ProjectStartOverdueWarning prj)
;;

let check_projects_active (assignments_map : assignment list IntMap.t) (prj : project) =
  let has_active_assignments =
    match IntMap.find_opt prj.number assignments_map with
    | None -> false
    | Some asns ->
      let today = CalendarLib.Date.today () in
      List.exists
        (fun a ->
          get_first_day a.allocation <= today && get_last_day a.allocation >= today)
        asns
  in
  if prj.state >= Active && not has_active_assignments
  then log_event (ActiveProjectWithoutAssignmentWarning prj)
;;

let check_projects projects assignments =
  let folder acc asg =
    IntMap.update
      asg.project.number
      (function
       | None -> Some [ asg ]
       | Some other_asgs -> Some (asg :: other_asgs))
      acc
  in
  let assignments_by_project = List.fold_left folder IntMap.empty assignments in
  IntMap.iter (fun _ p -> check_is_overdue p) projects;
  IntMap.iter (fun _ p -> check_projects_active assignments_by_project p) projects
;;

(* ---------------------------------------------------------------------- *)
(* BUILD SCHEDULE *)

let get_the_schedule ~start_date ~end_date =
  let fc_projects, fc_people', fc_assignments =
    Forecast.get_the_schedule ~start_date ~end_date
  in
  let fc_people = fc_people' |> Forecast.StringMap.bindings |> List.map snd in
  let gh_issues =
    Github.get_project_issues ()
    |> List.map (fun i -> i.number, i)
    |> List.to_seq
    |> IntMap.of_seq
  in
  let gh_people = Github.all_users in
  let people = merge_people fc_people gh_people in
  let projects = merge_projects fc_projects gh_issues in
  let assignments = List.filter_map (merge_assignment people projects) fc_assignments in
  check_projects projects assignments;

  people, projects, assignments
;;
