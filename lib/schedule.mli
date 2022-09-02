open CalendarLib

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
  ; earliest_start_date : Date.t option
  ; latest_start_date : Date.t option
  ; latest_end_date : Date.t option
  ; fte_months : float option
  ; nominal_fte_percent : float option
  ; max_fte_percent : float option
  ; min_fte_percent : float option
  }

type allocation =
  { person_id : int
  ; project_id : int
  ; start_date : Date.t
  ; end_date : Date.t
  ; rate : float
  }

val make_schedule : unit -> person list * project list
val show_person : person -> string
val show_project : project -> string
