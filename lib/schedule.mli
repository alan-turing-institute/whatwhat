open CalendarLib

type person =
  { email : string
  ; name : string
  ; github_login : string
  }

type _project_column =
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

type _project =
  { forecast_id : int
  ; github_id : int
  ; earliest_start_date : Date.t
  ; latest_start_date : Date.t
  ; latest_end_date : Date.t
  ; fte_months : float
  ; nominal_fte_percent : float
  ; max_fte_percent : float
  ; min_fte_percent : float
  ; name : string
  ; start_date : Date.t option
  ; end_date : Date.t option
  ; column : _project_column
  }

type allocation =
  { person_id : int
  ; project_id : int
  ; start_date : Date.t
  ; end_date : Date.t
  ; rate : float
  }

val make_schedule : unit -> person list
val show_person : person -> string
