(* High-level interface to the Github API

Performs validation for Github issues. A valid project issue has: 
  - Metadata, with at least the entries:
    - earliest_start_date
    - latest_start_date
    - fte_months  
    - nominal_fte_percent

Errors are logged to the console when these are missing, or when metadata is malformed,
and the issue is dropped from the list of all issues.

Errors currently occur when:
  - Metadata does not have 8 keys.
  - Cannot break a metadata line into a (key,value) pair, which means it is incorrect
  yaml.
  - A crucial key cannot be parsed (missing, or there is an error when dealing with the
  value)

Warnings are given for other inconsisties (e.g. no one is assigned.) Warnings are given
due to:
  - There being additional information in the value entry. 
  - A non-crucial entry is missing or null. *)
module Raw = GithubRaw

type parseerror =
  | FieldError
  | FieldWarning
  | LengthError
  | LineError

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

(** We reexport the Raw.person type so that no other module ever has a need to import
   anything from GithubRaw.*)
type person = Raw.person =
  { login : string
  ; name : string option
  ; email : string option
  }

(** Project are 1-to-1 related with Github issues.*)
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

(** Return all the users in the Alan Turing Institute Github organisation.*)
val get_users : unit -> person list

val show_project : project -> string
val show_metadata : metadata -> string
val show_person : person -> string
