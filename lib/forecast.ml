(* High-level interface to the Forecast AP I

   Returns a valid schedule from Forecast. A valid schedule is one such that:
   - Every project is identified by a conformant project number (like 999). This
   project number refers to a GitHub issue number, not the Forecast ID.
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
(* Logging for errors and warnings *)

type forecast_event =
  | NoProjectCodeError of Raw.project (* E1001 *)
  | BadProjectCodeError of Raw.project (* E1002 *)
  | NoEmailError of Raw.person (* E1003 *)
  | InvalidEmailError of Raw.person (* E1004 *)
  | AssignmentToRemovedPersonError of Raw.assignment (* E1005 *)
  | AssignmentToRemovedProjectError of Raw.assignment (* E1006 *)
  | NoClientError of Raw.project (* E1007 *)
  | NoFinanceCodeWarning of (Raw.project, project) Either.t (* W1001 *)
  | MultipleFinanceCodesWarning of (Raw.project, project) Either.t (* W1002 *)
  | DuplicateIssueNumberWarning of project (* W1003 *)
  | DuplicateIssueNumberNameWarning of project (* W1004 *)
  | ChoseOneFinanceCodeDebug of (Raw.project, project) Either.t * string list * string

let log_event (fc_event : forecast_event) : unit =
  match fc_event with
  | NoProjectCodeError raw_proj ->
    Log.log'
      { level = Log.Error' 1001
      ; source = Log.Forecast
      ; entity = Log.RawForecastProject raw_proj.name
      ; message = "Project code (i.e. GitHub issue number) not found."
      }
  | BadProjectCodeError raw_proj ->
    Log.log'
      { level = Log.Error' 1002
      ; source = Log.Forecast
      ; entity = Log.RawForecastProject raw_proj.name
      ; message =
          Printf.sprintf
            "Malformed project code (i.e. GitHub issue number) %s found."
            (Option.get raw_proj.code)
      }
  | NoEmailError raw_person ->
    Log.log'
      { level = Log.Error' 1003
      ; source = Log.Forecast
      ; entity = Log.RawForecastProject (Raw.make_person_name raw_person)
      ; message = "No email found."
      }
  | InvalidEmailError raw_person ->
    Log.log'
      { level = Log.Error' 1004
      ; source = Log.Forecast
      ; entity = Log.RawForecastProject (Raw.make_person_name raw_person)
      ; message =
          Printf.sprintf "Invalid email found: <%s>." (Option.get raw_person.email)
      }
  | AssignmentToRemovedPersonError raw_assignment ->
    Log.log'
      { level = Log.Error' 1005
      ; source = Log.Forecast
      ; entity = Log.RawForecastAssignment raw_assignment.id
      ; message = Printf.sprintf "Assignment made to removed person."
      }
  | AssignmentToRemovedProjectError raw_assignment ->
    Log.log'
      { level = Log.Error' 1006
      ; source = Log.Forecast
      ; entity = Log.RawForecastAssignment raw_assignment.id
      ; message = Printf.sprintf "Assignment made to removed project."
      }
  | NoClientError rp ->
    Log.log'
      { level = Log.Error' 1007
      ; source = Log.Forecast
      ; entity = Log.RawForecastProject rp.name
      ; message = "Client not found."
      }
  | NoFinanceCodeWarning rp_or_p ->
    Log.log'
      { level = Log.Warning 1001
      ; source = Log.Forecast
      ; entity =
          (match rp_or_p with
           | Left rp -> Log.RawForecastProject rp.name
           | Right p -> Log.Project p.number)
      ; message = "Finance code not found in project tags."
      }
  | MultipleFinanceCodesWarning rp_or_p ->
    Log.log'
      { level = Log.Warning 1002
      ; source = Log.Forecast
      ; entity =
          (match rp_or_p with
           | Left rp -> Log.RawForecastProject rp.name
           | Right p -> Log.Project p.number)
      ; message = "Multiple finance codes found in project tags."
      }
  | DuplicateIssueNumberWarning p ->
    Log.log'
      { level = Log.Warning 1003
      ; source = Log.Forecast
      ; entity = Log.Project p.number
      ; message =
          "Another project with the same issue number but different name was found."
      }
  | DuplicateIssueNumberNameWarning p ->
    Log.log'
      { level = Log.Warning 1003
      ; source = Log.Forecast
      ; entity = Log.Project p.number
      ; message = "Another project with the same issue number and same name was found."
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
       log_event (BadProjectCodeError p);
       None)
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
       | Some n -> Some { number = n; name = p.name; programme = c.name }
       | None -> None))
;;

(** [extract_finance_code p] checks the tags of the project [p] to find the
    Turing finance code. *)
let extract_finance_code (projects : project IntMap.t) _ (rp : Raw.project) =
  (* [rp_or_p] is either the raw project, or the project itself if it was found
     inside the [projects] map. *)
  let rp_or_p =
    match IntMap.find_opt rp.id projects with
    | None -> Either.Left rp
    | Some p -> Either.Right p
  in
  match rp.tags with
  | [ fc ] -> Some fc
  | [] ->
    log_event (NoFinanceCodeWarning rp_or_p);
    None
  | _ ->
    log_event (MultipleFinanceCodesWarning rp_or_p);
    (* Try to find the one with the form <char>-<str>-<str>. *)
    let looks_like_finance_code s =
      match String.split_on_char '-' s with
      | [ char; _; _ ] -> String.length char = 1
      | _ -> false
    in
    (match List.filter looks_like_finance_code rp.tags with
     | [ fc ] ->
       log_event (ChoseOneFinanceCodeDebug (rp_or_p, rp.tags, fc));
       Some fc
     | _ -> None)
;;

(* People *)

(** [validate_person p] ensures that the person [p]:
    1. has not been archived; and
    2. has a valid email address.
    *)
let validate_person (p : Raw.person) : person option =
  let email_re =
    Tyre.compile (Tyre.pcre {|^[A-Za-z0-9._%+-]+@[A-Za-z0-9.+-]+\.[A-Za-z]{2,}$|})
  in
  if p.archived
  then None
  else (
    match p.email with
    | None ->
      log_event (NoEmailError p);
      None
    | Some email ->
      (match Tyre.exec email_re email with
       | Error _ ->
         log_event (InvalidEmailError p);
         None
       | Ok _ ->
         Some
           { email
           ; full_name = Raw.make_person_name p
           ; github_handle = None
           ; slack_handle = None
           }))
;;

(* Allocations *)

(* Parse Raw.assignments into Forecast.assignments.

   At this stage we ignore the fact that many assignments may actually concern the same
   person, project, finance code combination, and just create a single assignment for each
   Raw.assignment, each with a single allocation. We will later merge these, collating the
   allocations.
   *)
let validate_assignment people projects fcs (a : Raw.assignment) =
  match a.entity with
  (* TODO: Retain placeholder assignments! *)
  | Placeholder _ -> None
  | Person person ->
    (match IntMap.find_opt person.id people with
     (* Check that the person wasn't deleted. *)
     | None ->
       log_event (AssignmentToRemovedPersonError a);
       None
     | Some valid_person ->
       (match IntMap.find_opt a.project.id projects with
        (* Check that the project wasn't deleted. *)
        | None ->
          log_event (AssignmentToRemovedProjectError a);
          None
        | Some prj ->
          Some
            { project = prj.number
            ; person = valid_person.email
            ; finance_code = Raw.IntMap.find_opt a.project.id fcs
            ; allocation =
                make_allocation
                  a.start_date
                  a.end_date
                  (FTE.from_forecast_rate a.allocation)
            }))
;;

(* A Map module that can have as keys tuples of project.id, person.email, finance_code
   option, or PPF for short. *)
module PPF = struct
  type t = int * string * string option

  let compare = Stdlib.compare
end

module AssignmentMap = Map.Make (PPF)
module DateMap = Map.Make (CalendarLib.Date)

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
        ; allocation = combine_allocations a.allocation existing_assignment.allocation
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

  (* A things_id is a map from raw Forecast ids to the thing *)
  let projects_id = IntMap.filter_map validate_project projs in
  let fcs_id = IntMap.filter_map (extract_finance_code projects_id) projs in
  let people_id = IntMap.filter_map (fun _ p -> validate_person p) peopl in
  let assignments =
    List.filter_map (validate_assignment people_id projects_id fcs_id) asnts
    |> collate_allocations
  in
  make_project_map projects_id, make_people_map people_id, assignments
;;
