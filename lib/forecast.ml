(* High-level interface to the Forecast AP I

   Returns a valid schedule from Forecast. A valid schedule is one such that:
   - Every project is identified by a conformant project number (like 999). This
     project number refers to a GitHub issue number, not the Forecast ID.
   - Every person is identified by an email (which is not checked)

   All other projects, persons, AND related assignments are removed

   "Errors" are emitted when an entity is invalid and is removed;
   "Warnings" are emitted when an entity is not removed but there is some issue.
*)

module Raw = ForecastRaw
module IntMap = Map.Make (Int) (* Issue number => project *)
module StringMap = Map.Make (String) (* email => person *)

type project =
  { number : int (* Ought to be a GitHub issue number *)
  ; name : string
  ; programme : string
  ; old_finance_code : string option
  ; erpx_finance_code : string option
  }

(* ------------------------------------------------------------ *)
(* Logging for errors and warnings *)

type forecast_event =
  | NoProjectCodeError of Raw.project (* E1001 *)
  | BadProjectCodeError of Raw.project * string (* E1002 *)
  | NoEmailError of Raw.person (* E1003 *)
  | InvalidEmailError of Raw.person * string (* E1004 *)
  | NoClientError of Raw.project (* E1005 *)
  | NoFinanceCodeWarning of (Raw.project, project) Either.t (* W1001 *)
  | MultipleFinanceCodesWarning of (Raw.project, project) Either.t (* W1002 *)
  | DuplicateIssueNumberWarning of project (* W1003 *)
  | AssignmentToRemovedPersonDebug of Raw.assignment
  | AssignmentToRemovedProjectDebug of Raw.assignment
  | NonREGPersonInfo of Raw.person
  | ArchivedPersonDebug of Raw.person
  | ArchivedPlaceholderDebug of Raw.placeholder

let log_event (fc_event : forecast_event) : unit =
  let make_log_raw_project (raw_proj : Raw.project) =
    match raw_proj.code with
    | None -> Log.RawForecastProject (Either.Left raw_proj.name)
    | Some c ->
      (match Tyre.(exec (compile (str "hut23-" *> int))) c with
       | Ok issue_number -> Log.RawForecastProject (Either.Right issue_number)
       | Error _ -> Log.RawForecastProject (Either.Left raw_proj.name))
  in
  match fc_event with
  | NoProjectCodeError raw_proj ->
    Log.log'
      { level = Log.Error' 1001
      ; entity = make_log_raw_project raw_proj
      ; message =
          Printf.sprintf
            "Project code (i.e. GitHub issue number) not found in project <%s>."
            raw_proj.name
      }
  | BadProjectCodeError (raw_proj, code) ->
    Log.log'
      { level = Log.Error' 1002
      ; entity = make_log_raw_project raw_proj
      ; message =
          Printf.sprintf
            "Malformed project code (i.e. GitHub issue number) <%s> found in project \
             <%s>."
            code
            raw_proj.name
      }
  | NoEmailError raw_person ->
    let name = Raw.make_person_name raw_person in
    Log.log'
      { level = Log.Error' 1003
      ; entity = Log.RawForecastPerson name
      ; message = Printf.sprintf "No email found for person <%s>." name
      }
  | InvalidEmailError (raw_person, email) ->
    let name = Raw.make_person_name raw_person in
    Log.log'
      { level = Log.Error' 1004
      ; entity = Log.RawForecastPerson name
      ; message = Printf.sprintf "Invalid email: <%s> found for person <%s>." email name
      }
  | NoClientError rp ->
    Log.log'
      { level = Log.Error' 1005
      ; entity = make_log_raw_project rp
      ; message = Printf.sprintf "Client for project <%s> not found." rp.name
      }
  | NoFinanceCodeWarning rp_or_p ->
    Log.log'
      { level = Log.Warning 1001
      ; entity =
          (match rp_or_p with
           | Left rp -> make_log_raw_project rp
           | Right p -> Log.Project p.number)
      ; message =
          (match rp_or_p with
           | Left rp ->
             Printf.sprintf "Finance code not found in tags for project <%s>." rp.name
           | Right _ -> "Finance code not found in Forecast project tags.")
      }
  | MultipleFinanceCodesWarning rp_or_p ->
    Log.log'
      { level = Log.Warning 1002
      ; entity =
          (match rp_or_p with
           | Left rp -> make_log_raw_project rp
           | Right p -> Log.Project p.number)
      ; message =
          (match rp_or_p with
           | Left rp ->
             Printf.sprintf
               "Multiple finance codes found in tags for project <%s>."
               rp.name
           | Right _ -> "Multiple finance codes found in Forecast project tags.")
      }
  | DuplicateIssueNumberWarning p ->
    Log.log'
      { level = Log.Warning 1003
      ; entity = Log.Project p.number
      ; message = "Another Forecast project with the same issue number was found."
      }
  | AssignmentToRemovedPersonDebug raw_assignment ->
    Log.log'
      { level = Log.Debug
      ; entity = Log.RawForecastAssignment raw_assignment.id
      ; message =
          Printf.sprintf
            "Assignment made to removed entity <%s>."
            (Raw.get_entity_name raw_assignment.entity)
      }
  | AssignmentToRemovedProjectDebug raw_assignment ->
    Log.log'
      { level = Log.Debug
      ; entity = Log.RawForecastAssignment raw_assignment.id
      ; message =
          Printf.sprintf
            "Assignment made to removed project <%s>."
            raw_assignment.project.name
      }
  | NonREGPersonInfo raw_person ->
    let name = Raw.make_person_name raw_person in
    Log.log'
      { level = Log.Info
      ; entity = Log.RawForecastPerson name
      ; message = Printf.sprintf "Ignoring non-REG person <%s>." name
      }
  | ArchivedPersonDebug raw_person ->
    let name = Raw.make_person_name raw_person in
    Log.log'
      { level = Log.Debug
      ; entity = Log.RawForecastPerson name
      ; message = Printf.sprintf "Ignoring archived person <%s>." name
      }
  | ArchivedPlaceholderDebug raw_placeholder ->
    let name = raw_placeholder.name in
    Log.log'
      { level = Log.Debug
      ; entity = Log.RawForecastPlaceholder name
      ; message = Printf.sprintf "Ignoring archived person <%s>." name
      }
;;

(* ------------------------------------------------------------  *)
(* Utilities for converting raw entities to nicer ones *)

(* Projects *)

(** [extract_project_number p] inspects the Forecast project code and picks out
    the digits at the end if it satisfies the regex 'hut23-\d+'. *)
let extract_project_number (p : Raw.project) =
  let hut23_re = Tyre.compile Tyre.(str "hut23-" *> int) in
  match p.code with
  | None ->
    log_event (NoProjectCodeError p);
    None
  | Some code ->
    (match Tyre.exec hut23_re code with
     | Ok num -> Some num
     | Error _ ->
       log_event (BadProjectCodeError (p, code));
       None)
;;

(** [extract_finance_code p] checks the tags of the project [p] to find the
    old finance code (X-ABC-NNN), and the new ERPx finance code (a five-digit
    number). *)
let extract_finance_codes (rp : Raw.project) =
  let is_digit = function
    | '0' .. '9' -> true
    | _ -> false
  in
  let is_old_finance_code s =
    match String.split_on_char '-' s with
    | [ char; _; _ ] -> String.length char = 1
    | _ -> false
  in
  let is_erpx_finance_code s = String.length s = 5 && String.for_all is_digit s in
  match rp.tags with
  (* No codes *)
  | [] ->
    log_event (NoFinanceCodeWarning (Left rp));
    None, None
  (* Only one *)
  | [ fc ] ->
    (match is_old_finance_code fc, is_erpx_finance_code fc with
     | true, false -> Some fc, None
     | false, true -> None, Some fc
     (* If it can't be parsed, we assume it's an old finance code *)
     | false, false -> Some fc, None
     | true, true ->
       failwith @@ "finance code " ^ fc ^ " was ambiguous; this should not happen")
  (* Multiple codes *)
  | _ ->
    let old_fc =
      match List.filter is_old_finance_code rp.tags with
      | [] -> None
      | [ old_fc ] -> Some old_fc
      | old_fcs ->
        log_event (MultipleFinanceCodesWarning (Left rp));
        Some (List.hd old_fcs)
    in
    let erpx_fc =
      match List.filter is_erpx_finance_code rp.tags with
      | [] -> None
      | [ erpx_fc ] -> Some erpx_fc
      | erpx_fcs ->
        log_event (MultipleFinanceCodesWarning (Left rp));
        Some (List.hd erpx_fcs)
    in
    old_fc, erpx_fc
;;

(** [validate_project _ p] checks that a Forecast project [p] is valid, in that:
    1. It is not archived.
    2. It is not one of the ignored projects in the [whatwhat] configuration
    file.
    3. It has a 'project code' matching the regex 'hut23-\d+'. The digits at the
    end are interpreted as the GitHub issue number, although this is not
    verified at this stage. Note that this is the {i Forecast} project code,
    which is not the same as the finance code (which, on Forecast, is stored
    in the [tags]; see [extract_finance_codes].)
    4. It has a valid client. *)
let validate_project _ (p : Raw.project) =
  (* silently remove *)
  if p.archived || List.mem p.id Config.forecast_ignored_projects
  then None
  else (
    match p.client with
    | None ->
      log_event (NoClientError p);
      None
    | Some c ->
      (match extract_project_number p with
       | Some n ->
         let old_fc, erpx_fc = extract_finance_codes p in
         Some
           { number = n
           ; name = p.name
           ; programme = c.name
           ; old_finance_code = old_fc
           ; erpx_finance_code = erpx_fc
           }
       | None -> None))
;;

(* People *)

(** This type represents all the useful information we can obtain from a Forecast
    person. It is not the same as Domain.person; that represents a complete
    overview of a person after the GitHub data has been merged in. *)
type person =
  { full_name : string
  ; email : string
  ; roles : string list
  }
[@@deriving ord]

(** An entity is a person or a placeholder. Placeholders are represented
    directly using the [Domain.placeholder] type, because there is no extra
    information about placeholders to be gained from GitHub. *)
type entity =
  | Person of person
  | Placeholder of Domain.placeholder

(** Get the name of an entity. *)
let get_entity_name e =
  match e with
  | Person p -> p.full_name
  | Placeholder p -> "Placeholder: " ^ p.name
;;

(** [validate_entity p] ensures that the entity [e]:
    1. has not been archived; and
    2. if it is a person, has the 'REG' tag; and
    3. if it is a person, has a valid email address. *)
let validate_person (p : Raw.person) : person option =
  let email_re =
    Tyre.compile (Tyre.pcre {|^[A-Za-z0-9._%+-]+@[A-Za-z0-9.+-]+\.[A-Za-z]{2,}$|})
  in
  if p.archived
  then (
    log_event (ArchivedPersonDebug p);
    None)
  else if not (List.mem "REG" p.roles)
  then (
    log_event (NonREGPersonInfo p);
    None)
  else (
    match p.email with
    | None ->
      log_event (NoEmailError p);
      None
    | Some email ->
      (match Tyre.exec email_re email with
       | Error _ ->
         log_event (InvalidEmailError (p, email));
         None
       | Ok _ -> Some { email; full_name = Raw.make_person_name p; roles = p.roles }))
;;

(* Allocations *)

(* This type represents all the useful information we can obtain from a Forecast
   assignment. It is not the same as Domain.assignment, that represents a
   complete overview of an assignment after the GitHub data has been merged in.
*)
type assignment =
  { project : project
  ; entity : entity
  ; allocation : Domain.allocation
  }

(* Parse Raw.assignments into Forecast.assignments.

   At this stage we ignore the fact that many assignments may actually concern the same
   person, project, finance code combination, and just create a single assignment for each Raw.assignment, each with a single allocation. We will later merge these, collating the
   allocations.
*)
let validate_assignment people projects (a : Raw.assignment) =
  (* First check that the project wasn't deleted *)
  match IntMap.find_opt a.project.id projects with
  | None ->
    log_event (AssignmentToRemovedProjectDebug a);
    None
  | Some prj ->
    (* Then check the entity to which it's assigned *)
    (match a.entity with
     | Placeholder placeholder ->
       if placeholder.archived
       then (
         log_event (ArchivedPlaceholderDebug placeholder);
         None)
       else
         Some
           { project = prj
           ; entity = Placeholder { name = placeholder.name }
           ; allocation =
               Domain.make_allocation
                 ~with_weekends:true
                 a.start_date
                 a.end_date
                 (Domain.FTE.from_forecast_rate a.allocation)
           }
     | Person person ->
       (match IntMap.find_opt person.id people with
        (* Check that the person wasn't deleted. *)
        | None ->
          log_event (AssignmentToRemovedPersonDebug a);
          None
        | Some valid_person ->
          Some
            { project = prj
            ; entity = Person valid_person
            ; allocation =
                Domain.make_allocation
                  ~with_weekends:false
                  a.start_date
                  a.end_date
                  (Domain.FTE.from_forecast_rate a.allocation)
            }))
;;

(* A Map module that can have as keys tuples of project.id, entity.name, finance_code
   option, or PPF for short. *)
module PNF = struct
  type t = int * string * string option

  let compare = Stdlib.compare
end

module AssignmentMap = Map.Make (PNF)
module DateMap = Map.Make (CalendarLib.Date)

(* Collect all the assignments related to a set of project, person, and finance code
   together, and join their allocations. *)
let collate_allocations assignments =
  let f (m : assignment AssignmentMap.t) (a : assignment) =
    let pnf = a.project.number, get_entity_name a.entity, a.project.old_finance_code in
    let existing_assignment_opt = AssignmentMap.find_opt pnf m in
    let assignment_to_add =
      match existing_assignment_opt with
      (* If there is already an assignment in the map m with this ppf value, add a new
         allocation to it. *)
      | Some existing_assignment ->
        { project = a.project
        ; entity = a.entity
        ; allocation =
            Domain.combine_allocations a.allocation existing_assignment.allocation
        }
      (* Otherwise add a new assignment to the map. *)
      | None -> a
    in
    AssignmentMap.add pnf assignment_to_add m
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

(* Issue a warning if any two projects on Forecast have the same issue number *)
let warn_duplicate_projects projects_id : unit =
  let add_project m (_, (p : project)) =
    match IntMap.find_opt p.number m with
    | None -> IntMap.add p.number p m
    | Some _ ->
      if not (List.mem p.number Config.forecast_duplicates_okay)
      then log_event (DuplicateIssueNumberWarning p);
      m
  in
  ignore @@ Seq.fold_left add_project IntMap.empty (IntMap.to_seq projects_id)
;;

(* ------------------------------------------------------------ *)
(* Interface *)

let get_the_schedule_async ~start_date ~end_date =
  let open Lwt.Syntax in
  let* _, peopl, _, projs, asnts = Raw.get_the_schedule_async ~start_date ~end_date in
  let projects_id = IntMap.filter_map validate_project projs in
  let people_id = IntMap.filter_map (fun _ e -> validate_person e) peopl in
  let assignments =
    List.filter_map (validate_assignment people_id projects_id) asnts
    |> collate_allocations
  in
  warn_duplicate_projects projects_id;
  Lwt.return (projects_id, make_people_map people_id, assignments)
;;

let get_the_schedule ~start_date ~end_date =
  get_the_schedule_async ~start_date ~end_date |> Lwt_main.run
;;
