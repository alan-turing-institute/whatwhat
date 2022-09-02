

module Raw = GithubRaw

type parseerror =
| FieldError
| FieldWarning
| LengthError
| LineError

type metadata = {
    turing_project_code : string option;
    earliest_start_date : CalendarLib.Date.t ;
    latest_start_date : CalendarLib.Date.t ;
    latest_end_date : CalendarLib.Date.t option;
    fte_months : float option;
    nominal_fte_percent : float option;
    max_fte_percent : float option;
    min_fte_percent : float option;
  }

type person = Raw.person = 
  { login : string
  ; name : string option
  ; email : string option
  }

type project = {
    number : int;
    title : string;
    body : string;
    state : string;
    assignees : person list;
    reactions : (string * person) list;
    column : string option;
    metadata : metadata;
  }


val log_parseerror : parseerror -> int -> string -> unit
val parse_fields: int -> string list -> metadata option
val metadata_of_yaml: int -> string -> metadata option
val parse_metadata: int -> string -> metadata option * string option
val validate_issue: Raw.issue -> project option
val get_project_issues: string -> project list

val show_project : project -> string
val show_metadata : metadata -> string