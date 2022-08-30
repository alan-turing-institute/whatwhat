

open CalendarLib
module Raw = GithubRaw

type parseerror =
| FieldError
| FieldWarning
| LengthError
| LineError

type metadata = {
    turing_project_code : string option;
    earliest_start_date : CalendarLib.Date.t option;
    latest_start_date : CalendarLib.Date.t option;
    latest_end_date : CalendarLib.Date.t option;
    fte_months : float option;
    nominal_fte_percent : float option;
    max_fte_percent : float option;
    min_fte_percent : float option;
  }

type project = {
    number : int;
    title : string;
    body : string;
    state : string;
    assignees : Raw.person list;
    reactions : (string * Raw.person) list;
    column : string option;
    metadata : metadata;
  }

val log_parseerror : parseerror -> int -> string -> unit
val dateprinter: Format.formatter -> Date.t -> unit
val dateprint_opt: Date.t option -> string
val maybe_null: f:(string -> 'a) -> string option -> 'a option
val maybe_null_string: string option -> string option
val maybe_null_float: string option -> float option
val catch_date_exn: int -> int -> int -> int -> Date.t option
val make_date: int -> string option -> Date.t option
val check_value: int -> string -> string -> string
val list_to_pair: int -> string list -> string * string
val parse_fields: int -> string list -> metadata option
val parse_lines:  int -> string list -> metadata option
val metadata_of_yaml: int -> string -> metadata option
val parse_metadata: int -> string -> metadata option * string option
val validate_issue: Raw.issue -> project option
val get_project_issues: string -> project list