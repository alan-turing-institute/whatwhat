(* Merge data sources and provide a unified view *)

open Domain

(* ---------------------------------------------------------------------- *)
(* LOGGING *)

type schedule_event =
  | MissingGithubProjectError of Forecast.project (* E3001 *)
  | FinanceCodeNotMatchingError of Github.issue (* E3002 *)
  (* MissingForecastProjectWarning cannot ever happen now because projects
     are fetched from Forecast rather than from GitHub. *)
  (* | MissingForecastProjectWarning of Github.issue (* W3001 *) *)
  | AllocationEndsTooLateWarning of assignment (* W3002 *)
  | AllocationStartsTooEarlyWarning of assignment (* W3003 *)
  | FTEDiscrepancyWarning of project (* W3004 *)
  (* We cannot detect these without knowing the state of the project, i.e.
     which column of the GitHub project tracker it is on.
     See https://github.com/alan-turing-institute/whatwhat/issues/114 *)
  (* | ActiveProjectWithoutAssignmentWarning of project (* W3007 *) *)
  (* | AssignmentsToInactiveProjectWarning of project (* W3008 *) *)
  (* | ProjectStartOverdueWarning of project (* W3009 *) *)
  | NoMatchingGithubUserWarning of Forecast.person (* W3010 *)
  | DifferentClientWarning of Github.issue * string * string option (* W3011 *)
  | DifferentNameWarning of Github.issue * string * string (* W3012 *)
  | MultipleMatchingGithubUsersWarning of Forecast.person * string list (* W3013 *)
  | AssignmentWithoutProjectDebug of Forecast.assignment
  | AssignmentWithoutPersonDebug of Forecast.person
  | GithubUserGuessedInfo of Forecast.person * string
  | GithubUserObtainedFromConfigInfo of Forecast.person * string

let log_event (error : schedule_event) =
  match error with
  | MissingGithubProjectError fc_proj ->
    Log.log'
      { level = Log.Error' 3001
      ; entity = Log.Project fc_proj.number
      ; message =
          Printf.sprintf
            "No matching GitHub issue for Forecast project <%s>."
            fc_proj.name
      }
  | FinanceCodeNotMatchingError issue ->
    Log.log'
      { level = Log.Error' 3002
      ; entity = Log.Project issue.number
      ; message = "Finance codes on Forecast and GitHub do not match."
      }
  (* | MissingForecastProjectWarning issue -> *)
  (*   Log.log' *)
  (*     { level = Log.Warning 3001 *)
  (*     ; entity = Log.Project issue.number *)
  (*     ; message = "No matching Forecast project found." *)
  (*     } *)
  | AllocationEndsTooLateWarning asn ->
    Log.log'
      { level = Log.Warning 3002
      ; entity = Log.ForecastProject asn.project.number
      ; message =
          Printf.sprintf
            "Assignment of <%s> ends after project latest end."
            (get_entity_name asn.entity)
      }
  | AllocationStartsTooEarlyWarning asn ->
    Log.log'
      { level = Log.Warning 3003
      ; entity = Log.ForecastProject asn.project.number
      ; message =
          Printf.sprintf
            "Assignment of <%s> begins before project earliest start."
            (get_entity_name asn.entity)
      }
  | FTEDiscrepancyWarning proj ->
    Log.log'
      { level = Log.Warning 3004
      ; entity = Log.Project proj.number
      ; message = "Total allocations in Forecast differ from GitHub metadata."
      }
  (* We cannot detect these without knowing the state of the project, i.e.
     which column of the GitHub project tracker it is on.
     See https://github.com/alan-turing-institute/whatwhat/issues/114 *)
  (* | ActiveProjectWithoutAssignmentWarning proj -> *)
  (*   Log.log' *)
  (*     { level = Log.Warning 3007 *)
  (*     ; entity = Log.Project proj.number *)
  (*     ; message = "Project is Active but has no current assignments." *)
  (*     } *)
  (* | AssignmentsToInactiveProjectWarning proj -> *)
  (*   Log.log' *)
  (*     { level = Log.Warning 3008 *)
  (*     ; entity = Log.Project proj.number *)
  (*     ; message = "Project is not Active but has current assignments." *)
  (*     } *)
  (* | ProjectStartOverdueWarning proj -> *)
  (*   Log.log' *)
  (*     { level = Log.Warning 3009 *)
  (*     ; entity = Log.Project proj.number *)
  (*     ; message = "Project is past latest start date but not yet Active." *)
  (*     } *)
  | NoMatchingGithubUserWarning person ->
    Log.log'
      { level = Log.Warning 3010
      ; entity = Log.ForecastPerson person.email
      ; message =
          Printf.sprintf "Could not find matching GitHub user for <%s>." person.full_name
      }
  | DifferentClientWarning (issue, fc_name, gh_name) ->
    Log.log'
      { level = Log.Warning 3011
      ; entity = Log.Project issue.number
      ; message =
          Printf.sprintf
            "Project programmes on Forecast (%s) and GitHub (%s) do not match."
            fc_name
            (match gh_name with
             | Some s -> s
             | None -> "absent")
      }
  | DifferentNameWarning (issue, fc_name, gh_name) ->
    Log.log'
      { level = Log.Warning 3012
      ; entity = Log.Project issue.number
      ; message =
          Printf.sprintf
            "Project names on Forecast (%s) and GitHub (%s) do not match."
            fc_name
            gh_name
      }
  | MultipleMatchingGithubUsersWarning (person, usernames) ->
    Log.log'
      { level = Log.Warning 3013
      ; entity = Log.ForecastPerson person.email
      ; message =
          Printf.sprintf
            "Multiple possible matching GitHub users for <%s> found (%s)."
            person.full_name
            (usernames |> List.map (fun s -> "@" ^ s) |> String.concat ", ")
      }
  | GithubUserGuessedInfo (psn, uname) ->
    Log.log'
      { level = Log.Info
      ; entity = Log.ForecastPerson psn.full_name
      ; message =
          Printf.sprintf
            "Guessed GitHub username <@%s> for Forecast person <%s>."
            uname
            psn.full_name
      }
  | GithubUserObtainedFromConfigInfo (psn, uname) ->
    Log.log'
      { level = Log.Info
      ; entity = Log.ForecastPerson psn.full_name
      ; message =
          Printf.sprintf
            "Obtained GitHub username <@%s> for Forecast person <%s> from configuration \
             file."
            uname
            psn.full_name
      }
  | AssignmentWithoutProjectDebug asn ->
    Log.log'
      { level = Log.Debug
      ; entity = Log.ForecastProject asn.project.number
      ; message = "Assignment made to project that has been deleted."
      }
  | AssignmentWithoutPersonDebug psn ->
    Log.log'
      { level = Log.Debug
      ; entity = Log.ForecastPerson psn.full_name
      ; message = "Assignment made to person that has been deleted."
      }
;;

(* ---------------------------------------------------------------------- *)
(* MERGE PEOPLE FROM FORECAST AND GITHUB *)

(* I don't understand why OCaml makes me write this boilerplate. *)
module FcSet = Set.Make (struct
    type t = Forecast.person

    let compare = compare
  end)

module GhSet = Set.Make (struct
    type t = Github.person

    let compare = compare
  end)

module PsnSet = Set.Make (struct
    type t = Domain.person

    let compare = compare
  end)

(** Create a list of all people, merging data from Forecast and Github. Our
    approach here is generally to map over Forecast people, because Forecast is
    considered authoritative for people.

    TODO: It would be nice to get Slack handles for people. *)
let merge_people
  (fc_people : Forecast.person list)
  (gh_people : Github.person list)
  (fc_assignments : Forecast.assignment list)
  =
  (* Helper function to check if assignment is current *)
  let is_current (asn : Forecast.assignment) =
    get_first_day asn.allocation < CalendarLib.Date.today ()
    && get_last_day asn.allocation > CalendarLib.Date.today ()
  in
  (* In a first pass, we remove anyone who is not in REG (as determined by
     roles) or who is only assigned to UNAVAILABLE, as we probably don't need to
     care about these. *)
  let is_available (fc_p : Forecast.person) =
    (* Has REG role *)
    List.mem "REG" fc_p.roles
    && ((* Is currently assigned to something that isn't just UNAVAILABLE *)
        List.exists
          (fun (a : Forecast.assignment) ->
            is_current a && a.entity = Person fc_p && a.project.programme <> "UNAVAILABLE")
          fc_assignments
        || (* Or is not assigned to anything at all *)
        not
          (List.exists
             (fun (a : Forecast.assignment) -> is_current a && a.entity = Person fc_p)
             fc_assignments))
  in
  let fc_all = List.filter is_available fc_people |> FcSet.of_list in
  let gh_all = GhSet.of_list gh_people in

  let make_new_person (fc_p : Forecast.person) (gh_p_opt : Github.person option) =
    { email = fc_p.email
    ; full_name = fc_p.full_name
    ; github_handle = Option.map (fun (p : Github.person) -> p.login) gh_p_opt
    ; slack_handle = None
    }
  in

  (* Then, we fetch data from the config file (as this should override all other
     'automatic' checks. *)
  let accum1 (name, login) (fc_found, gh_remaining, ppl_found) =
    match
      ( FcSet.filter (fun (p : Forecast.person) -> p.full_name = name) fc_all
        |> FcSet.elements
      , GhSet.filter (fun (p : Github.person) -> p.login = login) gh_all |> GhSet.elements
      )
    with
    | [ fc_p ], [ gh_p ] ->
      log_event (GithubUserObtainedFromConfigInfo (fc_p, gh_p.login));
      let new_person = make_new_person fc_p (Some gh_p) in
      ( FcSet.add fc_p fc_found
      , GhSet.remove gh_p gh_remaining
      , PsnSet.add new_person ppl_found )
    | _ -> fc_found, gh_remaining, ppl_found
  in
  let fc_found, gh_remaining, ppl_found =
    List.fold_right accum1 (Config.get_extra_users ()) (FcSet.empty, gh_all, PsnSet.empty)
  in
  let fc_remaining = FcSet.diff fc_all fc_found in

  (* Then we try to match Forecast people to GitHub accounts, based on an exact
     match between their names. *)
  let person_matches_perfectly (fc_p : Forecast.person) (gh_p : Github.person) =
    gh_p.email = Some fc_p.email || gh_p.name = Some fc_p.full_name
  in
  let accum2 fc_p (fc_found, gh_remaining, ppl_found) =
    match GhSet.filter (person_matches_perfectly fc_p) gh_remaining |> GhSet.elements with
    | [] -> fc_found, gh_remaining, ppl_found
    | [ gh_p ] ->
      let new_person = make_new_person fc_p (Some gh_p) in
      ( FcSet.add fc_p fc_found
      , GhSet.remove gh_p gh_remaining
      , PsnSet.add new_person ppl_found )
    | gh_ps ->
      (* Multiple exact matches, EXTREMELY unlikely *)
      let unames = List.map (fun (p : Github.person) -> p.login) gh_ps in
      log_event (MultipleMatchingGithubUsersWarning (fc_p, unames));
      fc_found, gh_remaining, ppl_found
  in
  let fc_found, gh_remaining, ppl_found =
    FcSet.fold accum2 fc_remaining (fc_found, gh_remaining, ppl_found)
  in

  (* Next, for anyone who wasn't found yet, we try to match Forecast people
     to GitHub accounts, based on some weaker heuristics. *)
  let fc_remaining = FcSet.diff fc_all fc_found in
  let person_matches_fuzzily (fc_p : Forecast.person) (gh_p : Github.person) =
    let names = fc_p.full_name |> String.split_on_char ' ' in
    let first_name = List.hd names in
    let last_name = List.hd (List.rev names) in
    Utils.contains ~case_sensitive:false gh_p.login last_name
    || Utils.contains ~case_sensitive:false gh_p.login first_name
    ||
    match gh_p.name with
    | Some n ->
      Utils.contains ~case_sensitive:false n last_name
      || Utils.contains ~case_sensitive:false n first_name
    | None -> false
  in
  let accum' fc_p (fc_found, gh_remaining, ppl_found) =
    match GhSet.filter (person_matches_fuzzily fc_p) gh_remaining |> GhSet.elements with
    | [] ->
      log_event (NoMatchingGithubUserWarning fc_p);
      fc_found, gh_remaining, ppl_found
    | [ gh_p ] ->
      log_event (GithubUserGuessedInfo (fc_p, gh_p.login));
      let new_person = make_new_person fc_p (Some gh_p) in
      ( FcSet.add fc_p fc_found
      , GhSet.remove gh_p gh_remaining
      , PsnSet.add new_person ppl_found )
    | gh_ps ->
      (* Multiple fuzzy matches *)
      let unames = List.map (fun (p : Github.person) -> p.login) gh_ps in
      log_event (MultipleMatchingGithubUsersWarning (fc_p, unames));
      fc_found, gh_remaining, ppl_found
  in
  let _, _, ppl_found =
    FcSet.fold accum' fc_remaining (fc_found, gh_remaining, ppl_found)
  in
  PsnSet.elements ppl_found
;;

(* ---------------------------------------------------------------------- *)
(* MERGE PROJECTS FROM FORECAST AND GITHUB *)

(* Convert a Forecast.project to a Domain.project without using any info from
   GitHub. *)
let upconvert (prj : Forecast.project) : project =
  { number = prj.number
  ; name = prj.name
  ; programme = Some prj.programme
  ; plan = None
  ; assignees = []
  ; erpx_finance_code = prj.erpx_finance_code
  }
;;

(* Check that each Forecast project has a hut23 code which matches that of a
   GitHub project. Additionally, check that the programmes and names of the
   projects are the same on both platforms.
*)
let merge_projects
  (fc_projects : Forecast.project IntMap.t)
  (gh_issues : Github.issue IntMap.t)
  (people : person list)
  =
  let add_github_info (fc_p : Forecast.project) : Domain.project =
    (* Try to find the matching GitHub issue *)
    match IntMap.find_opt fc_p.number gh_issues with
    | None ->
      (* If there is no matching GitHub issue, we can still return the Forecast
         project, but without any GitHub info. *)
      log_event (MissingGithubProjectError fc_p);
      upconvert fc_p
    | Some gh_p ->
      (* Check that their client/programme match *)
      if Some fc_p.programme <> gh_p.programme
      then log_event (DifferentClientWarning (gh_p, fc_p.programme, gh_p.programme));
      (* If the programme is not set on GitHub, we can use the one from Forecast
      *)
      let programme =
        match gh_p.programme with
        | Some p -> Some p
        | None -> Some fc_p.programme
      in
      (* Check that their names match *)
      if fc_p.name <> gh_p.name
      then log_event (DifferentNameWarning (gh_p, fc_p.name, gh_p.name));
      (* Check that their project codes match *)
      let finance_codes_match =
        match fc_p.old_finance_code, gh_p.plan with
        | Some cd, Some plan -> List.mem cd plan.finance_codes
        | _, None -> true
        | _ -> false
      in
      if not finance_codes_match then log_event (FinanceCodeNotMatchingError gh_p);
      (* Create a new project with the merged data *)
      let assignees =
        List.filter_map
          (fun p -> List.find_opt (fun psn -> psn.github_handle = Some p) people)
          gh_p.assignees
      in
      { number = gh_p.number
      ; name = gh_p.name
      ; programme
      ; plan = gh_p.plan
      ; assignees
      ; erpx_finance_code = fc_p.erpx_finance_code
      }
  in
  IntMap.map add_github_info fc_projects
;;

(* ---------------------------------------------------------------------- *)
(* MERGE ASSIGNMENTS FROM FORECAST AND GITHUB *)

let check_end_date (prj : project) (asg : assignment) =
  match asg.entity with
  | Placeholder _ -> ()
  | Person _ ->
    (match prj.plan with
     | None -> ()
     | Some plan ->
       (match plan.latest_end_date with
        | None -> ()
        | Some latest_end_date ->
          if get_last_day asg.allocation > latest_end_date
          then log_event (AllocationEndsTooLateWarning asg)))
;;

let check_start_date (prj : project) (asg : assignment) =
  match asg.entity with
  | Placeholder _ -> ()
  | Person _ ->
    (match prj.plan with
     | None -> ()
     | Some plan ->
       (match plan.earliest_start_date with
        | None -> ()
        | Some earliest_start_date ->
          if get_first_day asg.allocation < earliest_start_date
          then log_event (AllocationStartsTooEarlyWarning asg)))
;;

let merge_assignment people projects (asn : Forecast.assignment) : assignment option =
  match IntMap.find_opt asn.project.number projects with
  | None ->
    log_event (AssignmentWithoutProjectDebug asn);
    (match asn.entity with
     | Placeholder pl ->
       Some
         { project = upconvert asn.project
         ; entity = Placeholder pl
         ; allocation = asn.allocation
         }
     | Person asn_p ->
       (match List.find_opt (fun p -> p.full_name = asn_p.full_name) people with
        | None ->
          log_event (AssignmentWithoutPersonDebug asn_p);
          None
        | Some psn ->
          Some
            { project = upconvert asn.project
            ; entity = Person psn
            ; allocation = asn.allocation
            }))
  | Some prj ->
    (match asn.entity with
     | Placeholder p ->
       let new_asn =
         { project = prj; entity = Placeholder p; allocation = asn.allocation }
       in
       check_start_date prj new_asn;
       check_end_date prj new_asn;
       Some new_asn
     | Person asn_p ->
       (match List.find_opt (fun p -> p.full_name = asn_p.full_name) people with
        | None ->
          log_event (AssignmentWithoutPersonDebug asn_p);
          None
        | Some psn ->
          let new_asn =
            { project = prj; entity = Person psn; allocation = asn.allocation }
          in
          check_start_date prj new_asn;
          check_end_date prj new_asn;
          Some new_asn))
;;

(* ---------------------------------------------------------------------- *)
(* CHECKS ON SCHEDULE *)

module IntMap = Map.Make (Int)

(* Checks that the sum of FTEs assigned on Forecast matches the number of
   FTE-weeks or FTE-months specified on GitHub metadata *)
let check_assignment_sum asns prj =
  match prj.plan with
  | None -> ()
  | Some plan ->
    let total_fte_time = asns |> List.map Domain.Assignment.to_fte_weeks |> FTE.sum in
    let budget = plan.budget in
    let discrepancy = FTE.div (FTE.sub total_fte_time budget) budget in
    if discrepancy < -0.1 || discrepancy > 0.1
    then log_event (FTEDiscrepancyWarning prj)
    else ()
;;

(* Checks for People Required placeholders *)
(* TODO: Implement *)
let check_people_required asns prj =
  ignore asns;
  ignore prj;
  ()
;;

(* Aggregates all the checks above *)
let check_projects projects assignments =
  let asns_map : assignment list IntMap.t =
    assignments
    |> Utils.sort_and_group_by (fun a -> a.project.number)
    |> List.to_seq
    |> IntMap.of_seq
  in
  let run_all_checks _ p =
    let this_proj_asns =
      match IntMap.find_opt p.number asns_map with
      | Some asns -> asns
      | None -> []
    in
    check_assignment_sum this_proj_asns p;
    check_people_required this_proj_asns p
  in

  IntMap.iter run_all_checks projects
;;

(* ---------------------------------------------------------------------- *)
(* BUILD SCHEDULE *)

let get_the_schedule_async ~start_date ~end_date =
  let open Lwt.Syntax in
  let* fc_projects, fc_people', fc_assignments =
    Forecast.get_the_schedule_async ~start_date ~end_date
  in
  let fc_people = fc_people' |> Forecast.StringMap.bindings |> List.map snd in
  let issue_numbers =
    fc_projects |> IntMap.bindings |> List.map (fun (_, prj) -> prj.Forecast.number)
  in
  let* gh_issues = Github.get_issues_async issue_numbers in
  let gh_issues_map =
    gh_issues
    |> List.map (fun (i : Github.issue) -> i.number, i)
    |> List.to_seq
    |> IntMap.of_seq
  in
  let* gh_people = Lazy.force Github.all_users in
  let people = merge_people fc_people gh_people fc_assignments in
  let projects = merge_projects fc_projects gh_issues_map people in
  let assignments = List.filter_map (merge_assignment people projects) fc_assignments in
  check_projects projects assignments;

  Lwt.return (people, projects, assignments)
;;

let get_the_schedule ~start_date ~end_date =
  Lwt_main.run (get_the_schedule_async ~start_date ~end_date)
;;
