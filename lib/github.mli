(** High-level interface to the Github API

Performs validation for Github issues.

Errors are logged to the console when metadata is malformed, and the issue is dropped from
the list of all issues.

Errors currently occur when:
  - Metadata does not have 8 keys.
  - Cannot break a metadata line into a (key,value) pair, which means it is incorrect
  yaml.
  - A crucial key cannot be parsed (missing, or there is an error when dealing with the
  value). Currently all keys are marked as non-crucial, but this may change in the future.

Warnings are given for other inconsisties (e.g. no one is assigned). Warnings are given
due to:
  - There being additional information in the value entry.
  - A non-crucial entry is missing or null. *)
module Raw = GithubRaw

(** A type for different types of errors that may be raised when parsing issue metadata. *)
type parseerror =
  | FieldError
  | FieldWarning
  | LengthWarning
  | LineError

(** A type to hold the parsed YAML metadata from an issue header. *)
type metadata =
  { turing_project_code : string option
  ; earliest_start_date : CalendarLib.Date.t option
  ; latest_start_date : CalendarLib.Date.t option
  ; latest_end_date : CalendarLib.Date.t option
  ; fte_months : float option
  ; nominal_fte_percent : float option
  ; max_fte_percent : float option
  ; min_fte_percent : float option
  }

(* We reexport the Raw.person type so that no other module ever has a need to import
   anything from GithubRaw. *)

(** A type for Github users. *)
type person = Raw.person =
  { login : string
  ; name : string option
  ; email : string option
  }

(** Projects are 1-to-1 related with Github issues. *)
type project =
  { number : int
  ; title : string
  ; body : string
  ; state : string
  ; assignees : person list
  ; reactions : (string * person) list
  ; column : string option
  ; metadata : metadata
  }

(** Given a project board name, return a list of projects, one for each issue on the
    board. *)
val get_project_issues : string -> project list

(** Return all the users in the Alan Turing Institute Github organisation. *)
val get_users : unit -> person list

val show_project : project -> string
val show_metadata : metadata -> string
val show_person : person -> string
