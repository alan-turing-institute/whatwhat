(* High-level interface to the Forecast API

   Returns a valid schedule from Forecast. A valid schedule is one such that:
   - Every project is identified by a conformant project number (like 999)
   - Every person is identified by an email (which is not checked)
   
   All other projects, persons, AND related assignments are removed

   "Errors" are emitted when an entity is invalid and is removed;
   "Warnings" are emitted when an entity is not removed but there is some issue.

 *)

open Domain
module Raw = ForecastRaw
module IntMap = Map.Make (Int) (* Issue number => project *)
module StringMap = Map.Make (String) (* email => person *)

type project =
  { number : int (* Ought to be a GitHub issue number *)
  ; name : string
  ; programme : string
  }

(* ------------------------------------------------------------ *)
(* Utilities *)

(* `let+` is like "short-circuit and".  See "let-punning",
   https://v2.ocaml.org/manual/bindingops.html *)
let ( let+ ) o f = Option.map f o

(* ------------------------------------------------------------ *)
(* Logging for errors and warnings *)

let log_event lvl code ent msg = Log.log lvl code Log.Forecast ent msg

let log_raw_project (lvl : Log.level) code (rp : Raw.project) msg =
  log_event lvl code (Log.RawForecastProject rp.name) msg
;;

let log_project (lvl : Log.level) code (p : project) msg =
  log_event lvl code (Log.Project p.number) msg
;;

let log_raw_person (lvl : Log.level) code (p : Raw.person) msg =
  let name = p.first_name ^ " " ^ p.last_name in
  log_event lvl code (Log.RawForecastPerson name) msg
;;

let log_assignment (lvl : Log.level) code (a : Raw.assignment) msg =
  let aid (a : Raw.assignment) = "(" ^ string_of_int a.id ^ ")" in
  log_event lvl code Log.RawForecastAssignment (msg ^ aid a)
;;

(* ------------------------------------------------------------ *)
(* Domain-specific data *)

(* Regex to match the `NNN` in `hut23-NNN` *)
let hut23_code_re = Re.compile Re.(seq [ start; str "hut23-"; group (rep1 digit); stop ])
let ignored_project_ids = Config.get_forecast_ignored_projects ()

(* ------------------------------------------------------------  *)
(* Utilities for converting raw entities to nicer ones *)

(* Projects *)

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
    log_raw_project Log.Error 1001 project "Missing project code";
    None
  | Not_found ->
    log_raw_project Log.Error 1002 project @@ "Malformed project code: " ^ Option.get cd;
    None
;;

let validate_project id (p : Raw.project) =
  if p.archived || List.mem id ignored_project_ids || Option.is_none p.client
  then None
  else (
    let nmbr = extract_project_number p in
    let client = Option.get p.client in
    let+ nmbr = nmbr in
    { number = nmbr; name = p.name; programme = client.name })
;;

let extract_finance_code (projects : project IntMap.t) _ (rp : Raw.project) =
  let log_appropriate_project_warning code (rp : Raw.project) msg =
    match IntMap.find_opt rp.id projects with
    | None -> log_raw_project Log.Warning code rp msg
    | Some p -> log_project Log.Warning code p msg
  in
  match rp.tags with
  | [] ->
    log_appropriate_project_warning 1001 rp "Missing Finance Code";
    None
  | fc :: [] -> Some fc
  | fc :: _ ->
    log_appropriate_project_warning
      1002
      rp
      "More than one potential Finance code (using the first tag)";
    Some fc
;;

(* People *)

let validate_person _ (p : Raw.person) : person option =
  if p.archived
  then None
  else (
    match p.email with
    | None ->
      log_raw_person Log.Error 1003 p "Missing email";
      None
    | Some "" ->
      log_raw_person Log.Error 1004 p "Email is empty";
      None
    | Some email ->
      Some
        { email
        ; full_name = p.first_name ^ " " ^ p.last_name
        ; github_handle = None
        ; slack_handle = None
        })
;;

(* Allocations *)

(** Forecast reports rates as seconds in a day. We divide by the number of seconds in 8h
    to get the FTE percentage.*)
let forecast_rate_to_fte_percent n = float_of_int n /. float_of_int (60 * 60 * 8)

(* Parse Raw.assignments into Forecast.assignments.

   At this stage we ignore the fact that many assignments may actually concern the same
   person, project, finance code combination, and just create a single assignment for each
   Raw.assignment, each with a single allocation. We will later merge these, collating the
   allocations.
   *)
let validate_assignment fcs (a : Raw.assignment) =
  let log_func = fun code msg -> log_assignment Log.Error code a msg in
  match a.entity with
  (* We ignore assignments to placeholders here. *)
  | Placeholder _ ->
    log_func 9999 "Deleting an assignment to a placeholder";
    None
  | Person person ->
    let start_date_opt = Utils.date_of_string a.start_date in
    let end_date_opt = Utils.date_of_string a.end_date in
    (match person.email, start_date_opt, end_date_opt with
     | Some email, Ok start_date, Ok end_date ->
       Some
         { project = a.project.id
         ; person = email
         ; finance_code = Raw.IntMap.find_opt a.project.id fcs
         ; allocation =
             [ { start_date
               ; days = CalendarLib.Date.sub end_date start_date
               ; rate = Rate (forecast_rate_to_fte_percent a.allocation)
               }
             ]
         }
     | _ ->
       if person.email = None
       (* TODO: why is this being checked twice? *)
       then log_func 1003 "Deleting an assignment because a person had no email";
       if start_date_opt = Error ()
       then failwith ("F1008 Unable to parse assignment start_date " ^ a.start_date);
       if end_date_opt = Error ()
       then failwith ("F1009 Unable to parse assignment end_date " ^ a.end_date);
       None)
;;

(* A Map module that can have as keys tuples of project.id, person.email, finance_code
   option, or PPF for short. *)
module PPF = struct
  type t = int * string * string option

  let compare (x0, y0, z0) (x1, y1, z1) =
    match Stdlib.compare x0 x1 with
    | 0 ->
      (match Stdlib.compare y0 y1 with
       | 0 -> Stdlib.compare z0 z1
       | c -> c)
    | c -> c
  ;;
end

module AssignmentMap = Map.Make (PPF)

(* Collect all the assignments related to a set of project, person, and finance code
   together, and join their allocations. *)
let collate_allocations assignments =
  let f (m : assignment AssignmentMap.t) (a : assignment) =
    let ppf = a.project, a.person, a.finance_code in
    let existing_assignment_opt = AssignmentMap.find_opt ppf m in
    let assignment_to_add =
      match existing_assignment_opt with
      (* If there is already an assignment in the map m with this ppf value, add a new
       allocation to it. *)
      | Some existing_assignment ->
        { project = a.project
        ; person = a.person
        ; finance_code = a.finance_code
        ; allocation = existing_assignment.allocation @ a.allocation
        }
      (* Otherwise add a new assignment to the map. *)
      | None -> a
    in
    AssignmentMap.add ppf assignment_to_add m
  in
  List.fold_left f AssignmentMap.empty assignments
  |> AssignmentMap.to_seq
  |> Seq.map snd
  |> List.of_seq
;;

(* ------------------------------------------------------------ *)

(* Make map email => person *)
let make_people_map people_id =
  people_id |> IntMap.to_seq |> Seq.map (fun (_, p) -> p.email, p) |> StringMap.of_seq
;;

(* Make map number => project, with the slight complication that there may be
   two projects with the same issue number *)
let make_project_map projects_id : project IntMap.t =
  let add_project m (_, (p : project)) =
    match IntMap.find_opt p.number m with
    | None -> IntMap.add p.number p m
    | Some existing_p ->
      if p.name <> existing_p.name
      then
        log_project
          Log.Warning
          1003
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

let get_the_schedule ~start_date ~end_date =
  let _, peopl, _, projs, asnts = Raw.get_the_schedule ~start_date ~end_date in

  (* A things_id is a map from raw Forecast ids to the thing *)
  let projects_id = IntMap.filter_map validate_project projs in
  let fcs_id = IntMap.filter_map (extract_finance_code projects_id) projs in
  let people_id = IntMap.filter_map validate_person peopl in
  let assignments =
    List.filter_map (validate_assignment fcs_id) asnts
    |> collate_allocations
  in
  make_project_map projects_id, make_people_map people_id, assignments
;;

let get_the_current_schedule days =
  let start_date = CalendarLib.Date.today () in
  let end_date = CalendarLib.Date.add start_date (CalendarLib.Date.Period.day days) in
  get_the_schedule ~start_date ~end_date
;;
