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
  ; column : string
      (* TODO column could be an enum type?*)
      (*
  ; earliest_start_date : Date.t
  ; latest_start_date : Date.t
  ; latest_end_date : Date.t
  ; fte_months : float
  ; nominal_fte_percent : float
  ; max_fte_percent : float
  ; min_fte_percent : float
  ; start_date : Date.t option
  ; end_date : Date.t option
  *)
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
