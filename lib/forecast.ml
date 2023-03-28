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
  ; finance_code : string option
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
  | DuplicateIssueNumberNameWarning of project (* W1004 *)
  | AssignmentToRemovedPersonInfo of Raw.assignment
  | AssignmentToRemovedProjectInfo of Raw.assignment
  | NonREGPersonInfo of Raw.person
  | ArchivedPersonDebug of Raw.person
  | ArchivedPlaceholderDebug of Raw.placeholder
  | ChoseOneFinanceCodeDebug of (Raw.project, project) Either.t * string list * string

let log_event (fc_event : forecast_event) : unit =
  match fc_event with
  | NoProjectCodeError raw_proj ->
    Log.log'
      { level = Log.Error' 1001
      ; source = Log.Forecast
      ; entity = Log.RawForecastProject raw_proj.name
      ; message =
          Printf.sprintf
            "Project code (i.e. GitHub issue number) not found in project <%s>."
            raw_proj.name
      }
  | BadProjectCodeError (raw_proj, code) ->
    Log.log'
      { level = Log.Error' 1002
      ; source = Log.Forecast
      ; entity = Log.RawForecastProject raw_proj.name
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
      ; source = Log.Forecast
      ; entity = Log.RawForecastPerson name
      ; message = Printf.sprintf "No email found for person <%s>." name
      }
  | InvalidEmailError (raw_person, email) ->
    let name = Raw.make_person_name raw_person in
    Log.log'
      { level = Log.Error' 1004
      ; source = Log.Forecast
      ; entity = Log.RawForecastPerson name
      ; message = Printf.sprintf "Invalid email: <%s> found for person <%s>." email name
      }
  | NoClientError rp ->
    Log.log'
      { level = Log.Error' 1005
      ; source = Log.Forecast
      ; entity = Log.RawForecastProject rp.name
      ; message = Printf.sprintf "Client for project <%s> not found." rp.name
      }
  | NoFinanceCodeWarning rp_or_p ->
    Log.log'
      { level = Log.Warning 1001
      ; source = Log.Forecast
      ; entity =
          (match rp_or_p with
           | Left rp -> Log.RawForecastProject rp.name
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
      ; source = Log.Forecast
      ; entity =
          (match rp_or_p with
           | Left rp -> Log.RawForecastProject rp.name
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
      ; source = Log.Forecast
      ; entity = Log.Project p.number
      ; message =
          "Another Forecast project with the same issue number but different name was \
           found."
      }
  | DuplicateIssueNumberNameWarning p ->
    Log.log'
      { level = Log.Warning 1003
      ; source = Log.Forecast
      ; entity = Log.Project p.number
      ; message =
          "Another Forecast project with the same issue number and same name was found."
      }
  | AssignmentToRemovedPersonInfo raw_assignment ->
    Log.log'
      { level = Log.Info
      ; source = Log.Forecast
      ; entity = Log.RawForecastAssignment raw_assignment.id
      ; message =
          Printf.sprintf
            "Assignment made to removed entity <%s>."
            (Raw.get_entity_name raw_assignment.entity)
      }
  | AssignmentToRemovedProjectInfo raw_assignment ->
    Log.log'
      { level = Log.Info
      ; source = Log.Forecast
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
      ; source = Log.Forecast
      ; entity = Log.RawForecastPerson name
      ; message = Printf.sprintf "Ignoring non-REG person <%s>." name
      }
  | ArchivedPersonDebug raw_person ->
    let name = Raw.make_person_name raw_person in
    Log.log'
      { level = Log.Debug
      ; source = Log.Forecast
      ; entity = Log.RawForecastPerson name
      ; message = Printf.sprintf "Ignoring archived person <%s>." name
      }
  | ArchivedPlaceholderDebug raw_placeholder ->
    let name = raw_placeholder.name in
    Log.log'
      { level = Log.Debug
      ; source = Log.Forecast
      ; entity = Log.RawForecastPlaceholder name
      ; message = Printf.sprintf "Ignoring archived person <%s>." name
      }
  | ChoseOneFinanceCodeDebug (rp_or_p, fcs, chosen_fc) ->
    Log.log'
      { level = Log.Debug
      ; source = Log.Forecast
      ; entity =
          (match rp_or_p with
           | Left rp -> Log.RawForecastProject rp.name
           | Right p -> Log.Project p.number)
      ; message =
          Printf.sprintf
            "From multiple finance codes <%s>, the following was chosen: <%s>"
            (fcs |> List.map (fun s -> "'" ^ s ^ "'") |> String.concat ", ")
            chosen_fc
      }
;;

(* ------------------------------------------------------------  *)
(* Utilities for converting raw entities to nicer ones *)

(* Projects *)

let ignored_project_ids = Config.get_forecast_ignored_projects ()

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
    Turing finance code. *)
let extract_finance_code (rp : Raw.project) =
  (* [rp_or_p] is either the raw project, or the project itself if it was found
     inside the [projects] map. *)
  (* let rp_or_p = *)
  (*   match IntMap.find_opt rp.id projects with *)
  (*   | None -> Either.Left rp *)
  (*   | Some p -> Either.Right p *)
  (* in *)
  match rp.tags with
  | [ fc ] -> Some fc
  | [] ->
    log_event (NoFinanceCodeWarning (Left rp));
    None
  | _ ->
    log_event (MultipleFinanceCodesWarning (Left rp));
    (* Try to find the one with the form <char>-<str>-<str>. *)
    let looks_like_finance_code s =
      match String.split_on_char '-' s with
      | [ char; _; _ ] -> String.length char = 1
      | _ -> false
    in
    (match List.filter looks_like_finance_code rp.tags with
     | [ fc ] ->
       log_event (ChoseOneFinanceCodeDebug (Left rp, rp.tags, fc));
       Some fc
     | _ -> None)
;;

(** [validate_project _ p] checks that a Forecast project [p] is valid, in that:
    1. It is not archived.
    2. It is not one of the ignored projects in the [whatwhat] configuration
       file.
    3. It has a 'project code' matching the regex 'hut23-\d+'. The digits at the
       end are interpreted as the GitHub issue number, although this is not
       verified at this stage. Note that this is the {i Forecast} project code,
       which is not the same as the finance code (which, on Forecast, is stored
       in the [tags]; see [extract_finance_code].)
    4. It has a valid client.
    *)
let validate_project _ (p : Raw.project) =
  (* silently remove *)
  if p.archived || List.mem p.id ignored_project_ids
  then None
  else (
    match p.client with
    | None ->
      log_event (NoClientError p);
      None
    | Some c ->
      (match extract_project_number p with
       | Some n ->
         Some
           { number = n
           ; name = p.name
           ; programme = c.name
           ; finance_code = extract_finance_code p
           }
       | None -> None))
;;

(* People *)

(** This type represents all the useful information we can obtain from a Forecast
    person. It is not the same as Domain.person; that represents a complete
    overview of a person after the GitHub data has been merged in.
    *)
type person =
  { full_name : string
  ; email : string
  }

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
    3. if it is a person, has a valid email address.
    *)
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
       | Ok _ -> Some { email; full_name = Raw.make_person_name p }))
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
    log_event (AssignmentToRemovedProjectInfo a);
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
                 a.start_date
                 a.end_date
                 (Domain.FTE.from_forecast_rate a.allocation)
           }
     | Person person ->
       (match IntMap.find_opt person.id people with
        (* Check that the person wasn't deleted. *)
        | None ->
          log_event (AssignmentToRemovedPersonInfo a);
          None
        | Some valid_person ->
          Some
            { project = prj
            ; entity = Person valid_person
            ; allocation =
                Domain.make_allocation
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
    let pnf = a.project.number, get_entity_name a.entity, a.project.finance_code in
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

(* Make map number => project, with the slight complication that there may be
   two projects with the same issue number *)
let make_project_map projects_id : project IntMap.t =
  let add_project m (_, (p : project)) =
    match IntMap.find_opt p.number m with
    | None -> IntMap.add p.number p m
    | Some existing_p ->
      if p.name <> existing_p.name
      then log_event (DuplicateIssueNumberWarning p)
      else log_event (DuplicateIssueNumberNameWarning p);
      m
  in
  Seq.fold_left add_project IntMap.empty (IntMap.to_seq projects_id)
;;

(* ------------------------------------------------------------ *)
(* Interface *)

let get_the_schedule ~start_date ~end_date =
  let _, peopl, _, projs, asnts = Raw.get_the_schedule ~start_date ~end_date in
  let projects_id = IntMap.filter_map validate_project projs in
  let people_id = IntMap.filter_map (fun _ e -> validate_person e) peopl in
  let assignments =
    List.filter_map (validate_assignment people_id projects_id) asnts
    |> collate_allocations
  in
  make_project_map projects_id, make_people_map people_id, assignments
;;
