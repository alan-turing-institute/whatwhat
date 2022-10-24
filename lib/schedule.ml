(* Merge data sources and provide a unified view *)

open Domain

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
  let () =
    if gh_person = None
    then (
      let error_msg =
        "No matching Github user: " ^ fc_p.full_name ^ " <" ^ fc_p.email ^ ">"
      in
      Log.log Log.Error Log.Schedule (Log.Person fc_p.full_name) error_msg)
  in
  gh_person
;;

(** Create a list of all people, merging data from Forecast and Github. *)
let get_people_list (fc_people : person list) (gh_people : Github.person list) =
  (* We fold over Forecast people, looking for the matching Github person for each,
     since Forecast is considered authoritative for people. *)
  let add_person (fc_p : person) m =
    let gh_p_opt = get_matching_gh_person_opt gh_people fc_p in
    match gh_p_opt with
    | Some gh_p ->
      let new_person =
        { email = fc_p.email;
          full_name = fc_p.full_name;
          github_handle = Some gh_p.login;
          slack_handle = None
        }
      in
      m @ [ new_person ]
    | None -> m
  in
  List.fold_right add_person fc_people []
;;

(* ---------------------------------------------------------------------- *)
(* MERGE PROJECTS FROM FORECAST AND GITHUB *)

(** Find the Forecast project associated with a given Github issue. *)
(* let get_matching_fc_project *)
(*   (fc_projects : Forecast.project list) *)
(*   (gh_project : project) *)
(*   = *)
(*   let project_matches (x : Forecast.project) = x.number = gh_project.nmbr in *)
(*   let fc_p_opt = List.find_opt project_matches fc_projects in *)
(*   let () = *)
(*     if fc_p_opt = None *)
(*     then ( *)
(*       let error_msg = *)
(*         "No Forecast project for Github issue " ^ Int.to_string gh_project.nmbr *)
(*       in *)
(*       Log.log Log.Error Log.Schedule (Log.Project gh_project.nmbr) error_msg) *)
(*   in *)
(*   fc_p_opt *)
(* ;; *)

(** Find the [person] matching the given Github user. *)
(* let person_opt_of_gh_person (people : person list) (gh_person : Github.person) = *)
(*   let login_matches person = *)
(*     person.github_handle = Some gh_person.login in *)
(*   let person_opt = List.find_opt login_matches people in *)
(*   if person_opt = None *)
(*   then ( *)
(*     let error_msg = *)
(*       "People list doesn't have an entry for Github login " ^ gh_person.login *)
(*     in *)
(*     Log.log Log.Error Log.Schedule (Log.Person gh_person.login) error_msg); *)
(*   person_opt *)
;;

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
    if not (List.exists (fun p -> p.nmbr = fc_p.number) gh_issues) then
      let open Log in
      log Error Schedule (RawForecastProject fc_p.name) "No matching GitHub project"
  in begin
      List.iter check_project_exists fc_projects;
      gh_issues
    end
  
    (* OLD CODE *)
  (* Fold over Github projects, looking for the matching Forecast project for each,
     since Github is authoritative for projects. *)
  (* let add_project (gh_project : project) m = *)
    (* let fc_p_opt = get_matching_fc_project fc_projects gh_project in *)
    (* let get_person_opt = person_opt_of_gh_person people in *)
    (* match fc_p_opt with *)
    (* | Some fc_p -> *)
    (*   let github_assignees = *)
    (*     List.filter_map get_person_opt gh_project.assignees *)
    (*     |> List.map (fun person -> person.email) *)
    (*   in *)
    (*   let reactions = *)
    (*     List.filter_map *)
    (*       (fun (emoji, gh_person) -> *)
    (*         let person_opt = person_opt_of_gh_person people gh_person in *)
    (*         match person_opt with *)
    (*         | Some person -> Some (emoji, person.email) *)
    (*         | None -> None) *)
    (*       gh_project.reactions *)
    (*   in *)
  (*     let new_project = *)
  (*       { nmbr = fc_p.num *)
  (*       ; name = gh_project.title *)
  (*       ; state = state_of_column gh_project.column *)
  (*       ; plan = gh_project.plan  *)
  (*       } *)
  (*     in *)
  (*     m @ [ new_project ] *)
  (*   | None -> m *)
  (* in *)
  (* List.fold_right add_project gh_issues [] *)
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
  let fc_projects, fc_people, assignments =
    let fcpp, fcpr, fcas = Forecast.get_the_schedule start_date end_date in
    (fcpp |> Forecast.IntMap.bindings |> List.map snd,
     fcpr |> Forecast.StringMap.bindings |> List.map snd,
     fcas) in
  let gh_issues = Github.get_project_issues @@ Config.get_github_project_name () in
  let gh_people = Github.get_users () in
  let people = get_people_list fc_people gh_people in
  let projects = merge_projects fc_projects gh_issues in
  people, projects, assignments
;;
