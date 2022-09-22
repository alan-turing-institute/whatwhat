(** Schedule collates data from both Github and Forecast, and joins them into single
    source-agnostic data structure. It deals with people, projects, and allocations.
    *)

(** A person, combining both a Forecast user and a matching Github user.

    Forecast is considered the authoritative source for people, and Github logins are
    attached to the Forecast information. *)
type person =
  { email : string
  ; name : string
  ; github_login : string
  }

(** A project, combining both a Github issue and a matching Forecast project.

    Github is considered the authoritative source for projects, information from
    Forecast is attached to the issues found on Github. *)
type project =
  { forecast_id : int
  ; github_id : int
  ; name : string
  ; github_assignees : string list
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

(** A working period with a start and end date, and [rate], i.e. a number of
    hours per day this work covers. Instances of [allocation] are attached to
    [assignment]s. *)
type allocation =
  { start_date : CalendarLib.Date.t
  ; end_date : CalendarLib.Date.t
  ; rate : float
  }

(** An assignment of a person to a project, together with a list of allocations, i.e. time
    periods and rates during which said person works on the project. *)
type assignment =
  { person : string
  ; project_id : int
  ; allocations : allocation list
  }

(** Return a list of people and projects for which we succesfully merged Forecast and
    Github data. In the process of doing the merge, log various warnings and errors when
    data on Forecast and/or Github is missing or malformed.

    TODO This function remains a work-in-progress. For one, we need to have it return a
    list of allocations as well. *)
val make_schedule : unit -> person list * project list

val show_person : person -> string
val show_project : project -> string
