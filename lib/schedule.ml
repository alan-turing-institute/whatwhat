open CalendarLib

type person =
  { email : string
  ; name : string
  ; github_login : string (* TODO Add list of assignments and allocations *)
  }
[@@deriving show]

(* TODO I would like for these to be partially ordered. How do I most easily
 achieve that? *)
type _project_column =
  | Suggested
  | Proposal
  | ExtraInfoNeeded
  | ProjectAppraisal
  | AwaitingGoNoGo
  | FindingPeople
  | AwaitingStart
  | Active
  | CompletionReview
  | Done
  | Cancelled
  | Rejected

type _project =
  { forecast_id : int
  ; github_id : int
  ; earliest_start_date : Date.t
  ; latest_start_date : Date.t
  ; latest_end_date : Date.t
  ; fte_months : float
  ; nominal_fte_percent : float
  ; max_fte_percent : float
  ; min_fte_percent : float
  ; name : string (* TODO Add list of allocations, list of finance codes, programme *)
  ; start_date : Date.t option
  ; end_date : Date.t option
  ; column : _project_column
  }

(* TODO Our sketch said that maybe we should have a unique id field for
   allocation too, but thinking of this again now, maybe if that's needed it
   can be implemented as a function, that just concatenates some of the other
   fields into a string. *)
type allocation =
  { person_id : int
  ; project_id : int
  ; start_date : Date.t
  ; end_date : Date.t
  ; rate : float (* Hours per day. *)
  }

module StringMap = Map.Make (String)

(* Extract all the people mentioned in a Github issue, either through
   assignments or reactions, and add them all to a StringMap m. *)
let add_people_from_issue (issue : GithubRaw.issue) m =
  let add_reaction (_, (p : GithubRaw.person)) = StringMap.add p.login p in
  let add_assignment (p : GithubRaw.person) = StringMap.add p.login p in
  List.fold_right add_reaction issue.reactions
  @@ List.fold_right add_assignment issue.assignees m
;;

let get_gh_people_map issues =
  List.fold_right add_people_from_issue issues StringMap.empty
;;

let string_eq_opt (a : string option) (b : string option) =
  match a, b with
  | Some x, Some y -> x = y
  | _ -> false
;;

(* Find the matching Github user for Forecast user fc_p. Raise an error if not
   found. *)
let get_matching_gh_person
  (gh_people : GithubRaw.person StringMap.t)
  (fc_p : Forecast.person)
  =
  let comp (_, (gh_p : GithubRaw.person)) =
    string_eq_opt gh_p.email (Some fc_p.email)
    || string_eq_opt gh_p.name (Some (Forecast.person_name fc_p))
  in
  let result_opt = gh_people |> StringMap.to_seq |> Seq.find comp in
  match result_opt with
  | Some (_, result) -> Some result
  | None ->
    let error_msg =
      "No matching Github user: " ^ fc_p.email ^ " " ^ Forecast.person_name fc_p
    in
    let () = Log.log Log.Error error_msg in
    None
;;

let get_people_map fc_people (gh_people : GithubRaw.person StringMap.t) =
  let add_person email (fc_p : Forecast.person) m =
    let gh_p_opt = get_matching_gh_person gh_people fc_p in
    match gh_p_opt with
    | Some gh_p ->
      let new_person =
        { email = fc_p.email
        ; name = Forecast.person_name fc_p
        ; github_login = gh_p.login
        }
      in
      StringMap.add email new_person m
    | None -> m
  in
  StringMap.fold add_person fc_people StringMap.empty
;;

let make_schedule () =
  let fc_schedule = Forecast.getTheCurrentSchedule () in
  let _fc_projects, fc_people, _fc_assignments =
    fc_schedule.projects, fc_schedule.people, fc_schedule.assignments
  in
  let gh_issues = GithubRaw.get_project_issues "NowWhat Test Project" in
  let gh_people = get_gh_people_map gh_issues in
  let people = get_people_map fc_people gh_people in
  people |> StringMap.to_seq |> List.of_seq |> List.map snd
;;
