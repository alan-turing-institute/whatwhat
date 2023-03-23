(* Domain model *)

module IntMap = Map.Make (Int)
module StringMap = Map.Make (String)
module DateMap = Map.Make (CalendarLib.Date)

type resource =
  | FTE_weeks of float
  | FTE_months of float
[@@deriving show]

module FTE = struct
  type t = FTE of float

  let from_forecast_rate n = FTE (float_of_int n /. (60. *. 60. *. 8.))
  let add (FTE x) (FTE y) = FTE (x +. y)
  let get (FTE x) = x
end

type allocation = FTE.t DateMap.t

let make_allocation start_date end_date fte =
  let open CalendarLib.Date in
  let is_weekend date =
    match day_of_week date with
    | Sat | Sun -> true
    | _ -> false
  in
  let rec accum date map =
    if date > end_date
    then map
    else (
      let next_day = add date (Period.day 1) in
      if is_weekend date
      then accum next_day map
      else DateMap.add date fte (accum next_day map))
  in
  accum start_date DateMap.empty
;;

let combine_allocations a1 a2 = DateMap.union (fun _ v1 v2 -> Some (FTE.add v1 v2)) a1 a2

type project_plan =
  { budget : resource
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

  let show_t t =
    match t with
    | Suggested -> "Suggested"
    | Proposal -> "Proposal"
    | ExtraInfoNeeded -> "ExtraInfoNeeded"
    | ProjectAppraisal -> "ProjectAppraisal"
    | AwaitingGoNogo -> "AwaitingGoNogo"
    | FindingPeople -> "FindingPeople"
    | AwaitingStart -> "AwaitingStart"
    | Active -> "Active"
    | CompletionReview -> "CompletionReview"
    | Done -> "Done"
    | Cancelled -> "Cancelled"
    | Rejected -> "Rejected"
  ;;
end

exception UnknownColumn of string

let state_of_column col =
  match col with
  | "Suggested" -> State.Suggested
  | "Proposal" -> State.Proposal
  | "Extra info needed" -> State.ExtraInfoNeeded
  | "Project appraisal" -> State.ProjectAppraisal
  | "Awaiting go/no-go" -> State.AwaitingGoNogo
  | "Finding people" -> State.FindingPeople
  | "Awaiting start" -> State.AwaitingStart
  | "Active" -> State.Active
  | "Completion review" -> State.CompletionReview
  | "Done" -> State.Done
  | "Cancelled" -> State.Cancelled
  | "Rejected" -> State.Rejected
  | _ -> raise (UnknownColumn ("Unknown GitHub column: " ^ col))
;;

type project =
  { number : int (** The issue number from GitHub *)
  ; name : string
  ; state : State.t
  ; programme : string option
  ; plan : project_plan
  }

type person =
  { email : string (** Email is the primary key for persons *)
  ; full_name : string
  ; github_handle : string option
  ; slack_handle : string option
  }

type assignment =
  { project : int  (** GitHub issue number *)
  ; person : string
  ; finance_code : string option
  ; allocation : allocation
  }

type schedule =
  { projects : project IntMap.t
  ; people : person StringMap.t
  ; assignments : assignment list
  }
