(* High-level interface to the Forecast API

   Returns a valid schedule from Forecast. A valid schedule is one such that:
   - Every project is identified by a conformant project number (like 999)
   - Every person is identified by an email (which is not checked)
   
   All other projects, persons, AND related assignments are removed

   "Errors" are emitted when an entity is invalid and is removed;
   "Warnings" are emitted when an entity is not removed but there is some issue.

 *)

open CalendarLib
module Raw = ForecastRaw
module IntMap = Map.Make (Int) (* Issue number => project *)
module StringMap = Map.Make (String) (* email => person *)

type project =
  { number : int (* Ought to be a GitHub issue number *)
  ; name : string
  ; programme : string
  }

type person =
  { email : string
  ; first_name : string
  ; last_name : string
  }

type assignment =
  { project : int (* The project code *)
  ; person : string (* An email *)
  ; finance_code : string option (* TODO: And an allocation ... *)
  }

type schedule =
  { projects : project IntMap.t
  ; people : person StringMap.t
  ; assignments : assignment list
  }

(* ------------------------------------------------------------ *)
(* Utilities *)

(* Like "short-circuit and".  See "let-punning",
   https://v2.ocaml.org/manual/bindingops.html *)
let ( let+ ) o f = Option.map f o
let person_name p = (p.first_name ^ " ") ^ p.last_name

(* ------------------------------------------------------------ *)
(* Logging for errors and warnings *)

let log_raw_project (lvl : Log.log_type) (p : Raw.project) msg =
  Log.log lvl @@ msg ^ " for project id=" ^ string_of_int p.id
;;

let log_project (lvl : Log.log_type) (p : project) msg =
  let make_hut23_code n = " [hut23-" ^ string_of_int n ^ "]" in
  Log.log lvl @@ msg ^ " for project " ^ make_hut23_code p.number
;;

let log_raw_person (lvl : Log.log_type) (p : Raw.person) msg =
  let name = p.first_name ^ " " ^ p.last_name in
  Log.log lvl @@ msg ^ " for " ^ name
;;

let log_assignment (lvl : Log.log_type) (a : Raw.assignment) msg =
  let aid (a : Raw.assignment) = "(" ^ string_of_int a.id ^ ")" in
  Log.log lvl @@ msg ^ aid a
;;

(* ------------------------------------------------------------ *)
(* domain-specific data *)

(* Regex to match the `NNN` in `hut23-NNN` *)
let hut23_code_re = Re.compile Re.(seq [ start; str "hut23-"; group (rep1 digit); stop ])

(* The Forecast internal id of a single, hard-coded project in Forecast called
   "Time off", which we do not use *)
let timeoff_project_id = 1684536

(* ------------------------------------------------------------ *)
(* Utilities for extracting data from particular fields and entities *)

let extract_finance_code _ (project : Raw.project) =
  match project.tags with
  | [] ->
    log_raw_project Log.Warning project "Missing Finance Code";
    None
  | fc :: [] -> Some fc
  | fc :: _ ->
    log_raw_project
      Log.Warning
      project
      "More than one potential Finance code (using the first tag)";
    Some fc
;;

let extract_project_number (project : Raw.project) =
  let cd = project.code in
  try
    Some
      (Option.get cd (* Raises Invalid_argument *)
      |> Re.exec hut23_code_re (* Raises Not_found *)
      |> (fun gp -> Re.Group.get gp 1) (* Raises Not_found *)
      |> int_of_string)
  with
  | Invalid_argument _ ->
    log_raw_project Log.Error project "Missing project code";
    None
  | Not_found ->
    log_raw_project Log.Error project "Malformed project code";
    None
;;

(* ------------------------------------------------------------ *)

let validate_project (clients : Raw.client Raw.IdMap.t) id (p : Raw.project) =
  if p.archived || id = timeoff_project_id
  then None
  else (
    let nmbr = extract_project_number p in
    let client = Raw.IdMap.find (Option.get p.client_id) clients in
    let+ nmbr = nmbr in
    { number = nmbr; name = p.name; programme = client.name })
;;

let validate_person _ (p : Raw.person) =
  if p.archived
  then None
  else (
    match p.email with
    | None ->
      log_raw_person Log.Error p "Missing email";
      None
    | Some "" ->
      log_raw_person Log.Error p "Email is the empty string";
      None
    | Some email -> Some { email; first_name = p.first_name; last_name = p.last_name })
;;

(* if StringMap.mem email m then *)
(*     begin *)
(*       log_raw_person Log.Error p "Another person has the same email as this"; *)
(*       m *)
(*     end *)
(*   else *)
(*     StringMap.add email {email = email; first_name = p.first_name; last_name = p.last_name} m *)
(* in *)
(* Raw.IdMap.to_seq people *)
(* |> Seq.fold_left add_person StringMap.empty  *)

let validate_assignment fcs people projects (a : Raw.assignment) =
  match a.person_id with
  (* If there is no person_id, this assignment is to a Placeholder and we ignore it *)
  | None -> None
  | Some person_id ->
    (match IntMap.find_opt person_id people with
     (* We may have deleted the corresponding person or project already because they were invalid *)
     | None ->
       log_assignment Log.Error a "Deleting an assignment because of missing person";
       None
     | Some person ->
       (match IntMap.find_opt a.project_id projects with
        | None ->
          log_assignment Log.Error a "Deleting as assignment because of a missing project";
          None
        | Some project ->
          Some
            { project = project.number
            ; person = person.email
            ; finance_code = Raw.IdMap.find_opt a.project_id fcs
            }))
;;

(* ------------------------------------------------------------ *)

(* Make map email => person *)
let make_people_map people_id =
  people_id |> IntMap.to_seq |> Seq.map (fun (_, p) -> p.email, p) |> StringMap.of_seq
;;

(* Make map number => project, with the slight complication that there may be
   two projects with the same issue number *)
let make_project_map projects_id =
  let add_project m (_, (p : project)) =
    match IntMap.find_opt p.number m with
    | None -> IntMap.add p.number p m
    | Some existing_p ->
      if p.name <> existing_p.name
      then
        log_project
          Log.Warning
          p
          "A project with the same number as this has a different name";
      (* TODO Might we want to have an else-clause that would also warn if
        there are two copies the same project, with the same number and name?
        *)
      m
  in
  Seq.fold_left add_project IntMap.empty (IntMap.to_seq projects_id)
;;

(* ------------------------------------------------------------ *)
(* Interface *)

let get_the_schedule (start_date : Date.t) (end_date : Date.t) =
  let clnts, peopl, _, projs, asnts = Raw.get_the_schedule start_date end_date in

  (* A things_id is a map from forecast ids to the thing *)
  let fcs_id = Raw.IdMap.filter_map extract_finance_code projs in

  let projects_id = IntMap.filter_map (validate_project clnts) projs in
  let people_id = IntMap.filter_map validate_person peopl in
  let assignments =
    List.filter_map (validate_assignment fcs_id people_id projects_id) asnts
  in

  let projects = make_project_map projects_id in
  let people = make_people_map people_id in

  { projects; people; assignments }
;;

let get_the_current_schedule days =
  let start_date = Date.today () in
  let end_date = Date.add start_date (Date.Period.day days) in
  get_the_schedule start_date end_date
;;
