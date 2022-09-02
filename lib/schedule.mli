type person =
  { email : string
  ; name : string
  ; github_login : string
  }

type project =
  { forecast_id : int
  ; github_id : int
  ; name : string
  ; assignees : string list
  ; reactions : (string * string) list
  ; column : string (* TODO column could be an enum type?*)
  ; turing_project_code : string option
  ; earliest_start_date : CalendarLib.Date.t option
  ; latest_start_date : CalendarLib.Date.t option
  ; latest_end_date : CalendarLib.Date.t option
  ; fte_months : float option
  ; nominal_fte_percent : float option
  ; max_fte_percent : float option
  ; min_fte_percent : float option
  }

(** An allocation of a person to a project for a time period. [rate] is the number of
    hours per day this allocation covers.*)
type allocation =
  { person_id : int
  ; project_id : int
  ; start_date : CalendarLib.Date.t
  ; end_date : CalendarLib.Date.t
  ; rate : float
  }

val make_schedule : unit -> person list * project list
val show_person : person -> string
val show_project : project -> string
