(* Domain model *)

module IntMap = Map.Make(Int)
module StringMap = Map.Make(String)

type resource =
  | FTE_weeks of float
  | FTE_months of float
[@@deriving show]

type rate = Rate of float

type simple_allocation = 
  { start_date : CalendarLib.Date.t 
  ; days : CalendarLib.Date.Period.t (* [days >=0] must be true *)
  ; rate : rate
  }

type allocation = simple_allocation list 

type project_plan = 
  {
    budget : resource
  ; finance_codes : string list
  ; latest_start_date : CalendarLib.Date.t
  ; earliest_start_date : CalendarLib.Date.t option
  (** [earliest_start_date = None] means "can start as soon as you like" *)
  ; latest_end_date : CalendarLib.Date.t option
  (** [latest_end_date = None] means "can end whenever you like" *)
  ; nominal_fte_percent : float
  ; max_fte_percent : float
  ; min_fte_percent : float
  }

module State = struct
  type t =
    | Suggested
    | Proposal
    | ExtraInfoNeeded
    | ProjectAppraisal
    | AwaitingGoNogo
    | FindingPeople
    | AwaitingStart
    | Active
    | CompletionReview
    | Done
    | Cancelled
    | Rejected
end

exception UnknownColumn of string 

let state_of_column col =
  match col with
  | None -> failwith ("No GitHub column!")
  | Some colname ->
     match colname with
     | "Suggested"         -> State.Suggested
     | "Proposal"          -> State.Proposal
     | "Extra info needed" -> State.ExtraInfoNeeded
     | "Project appraisal" -> State.ProjectAppraisal
     | "Awaiting go/no-go" -> State.AwaitingGoNogo
     | "Finding people"    -> State.FindingPeople
     | "Awaiting start"    -> State.AwaitingStart
     | "Active"            -> State.Active
     | "Completion review" -> State.CompletionReview
     | "Done"              -> State.Done
     | "Cancelled"         -> State.Cancelled
     | "Rejected"          -> State.Rejected
     | _                   -> raise (UnknownColumn ("Unknown GitHub column: " ^ colname))

type project =
  { nmbr : int  (** The issue number from GitHub *)
  ; name : string
  ; state : State.t
  (* TODO: Fix this ; programme : string option *)
  ; plan : project_plan
  }

type person =
  { email : string; (** Email is the primary key for persons *)
    full_name : string;
    github_handle : string option;
    slack_handle : string option
  }

type assignment = 
  { project : int (* The project code *)
  ; person : string (* An email *)
  ; finance_code : string option
  ; allocation : allocation
  }

type schedule =
  {
    projects : project IntMap.t;
    people : person StringMap.t;
    assignments : assignment list
  }
