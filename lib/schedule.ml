open CalendarLib

type person =
  { email : string
  ; name : string
  ; github_login : string (* TODO Add list of assignments and allocations *)
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
  ; earliest_start_date : Date.t option
       [@printer fun fmt x -> Format.pp_print_string fmt (Dateprinter.dateprint_opt x)]
  ; latest_start_date : Date.t option
       [@printer fun fmt x -> Format.pp_print_string fmt (Dateprinter.dateprint_opt x)]
  ; latest_end_date : Date.t option
       [@printer fun fmt x -> Format.pp_print_string fmt (Dateprinter.dateprint_opt x)]
  ; fte_months : float option
  ; nominal_fte_percent : float option
  ; max_fte_percent : float option
  ; min_fte_percent : float option
  }
[@@deriving show]

type allocation =
  { person_id : int
  ; project_id : int
  ; start_date : Date.t
  ; end_date : Date.t
  ; rate : float (* Hours per day. *)
  }

module StringMap = Map.Make (String)
module IntMap = Map.Make (Int)

let string_eq_opt (a : string option) (b : string option) =
  match a, b with
  | Some x, Some y -> x = y
  | _ -> false
;;

(* Find the matching Github user for Forecast user fc_p. Raise an error if not
   found. *)
let get_matching_gh_person (gh_people : Github.person list) (fc_p : Forecast.person) =
  let comp (gh_p : Github.person) =
    let emails_match = string_eq_opt gh_p.email (Some fc_p.email) in
    let names_match = string_eq_opt gh_p.name (Some (Forecast.person_name fc_p)) in
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
let get_people_map fc_people (gh_people : Github.person list) =
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

(* Find the Forecast project associated with a given Github issue. *)
let get_matching_fc_project
  (fc_projects : Forecast.project IntMap.t)
  (gh_project : Github.project)
  =
  let fc_p_opt = fc_projects |> IntMap.find_opt gh_project.number in
  match fc_p_opt with
  | Some result -> Some result
  | None ->
    let error_msg =
      "No Forecast project for Github issue " ^ Int.to_string gh_project.number
    in
    let () = Log.log Log.Error error_msg in
    None
;;

(* Find, from the email->person map, the person matching the given Github user. *)
let person_of_gh_person (people : person StringMap.t) (gh_person : Github.person) =
  let login = gh_person.login in
  let _, found_person =
    people
    |> StringMap.to_seq
    |> List.of_seq
    |> List.find (fun (_, person) -> person.github_login = login)
  in
  found_person
;;

(* Make a map of Github issue ID to project. *)
let get_project_map
  (fc_projects : Forecast.project IntMap.t)
  (gh_issues : Github.project list)
  (people : person StringMap.t)
  =
  let add_project (gh_project : Github.project) m =
    let fc_p_opt = get_matching_fc_project fc_projects gh_project in
    match fc_p_opt with
    | Some fc_p ->
      let assignees =
        List.map (person_of_gh_person people) gh_project.assignees
        |> List.map (fun person -> person.email)
      in
      let reactions =
        List.map
          (fun (emoji, gh_person) -> emoji, (person_of_gh_person people gh_person).email)
          gh_project.reactions
      in
      let new_project =
        { forecast_id = fc_p.number
        ; github_id = gh_project.number
        ; name = gh_project.title
        ; assignees
        ; reactions
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
      IntMap.add gh_project.number new_project m
    | None -> m
  in
  List.fold_right add_project gh_issues IntMap.empty
;;

let make_schedule () =
  let fc_schedule = Forecast.getTheCurrentSchedule 180 in
  let fc_projects, fc_people, _fc_assignments =
    fc_schedule.projects, fc_schedule.people, fc_schedule.assignments
  in
  let gh_issues = Github.get_project_issues "NowWhat Test Project" in
  let gh_people = Github.get_users () in
  let people = get_people_map fc_people gh_people in
  let projects = get_project_map fc_projects gh_issues people in
  ( people |> StringMap.to_seq |> List.of_seq |> List.map snd
  , projects |> IntMap.to_seq |> List.of_seq |> List.map snd )
;;
