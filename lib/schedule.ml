open CalendarLib

type person =
  { forecast_id : int
  ; email : string
  ; name : string
  ; github_login : string (* TODO Add list of assignments and allocations *)
  }

(* TODO I would like for these to be partially ordered. How do I most easily
 achieve that? *)
type project_column =
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

type project =
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
  ; column : project_column
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
