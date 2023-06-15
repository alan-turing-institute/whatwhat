(** Maintains a log of problems found with each project. *)

(** {1 Logged events}

    Each event consists of:
      - A {i level}: a unique error code which decides how serious an event is
      - An {i entity}: an associated datatype that the event is associated
      with
      - A {i message}: A string to show.

    TODO: Decide if this setup is really sufficient. For example, do we need to
    provide more information so that things can be logged / notified properly.
      *)

(** Severity of logged problem *)
type level =
  | Error' of int (** Prevents processing of some data  *)
  | Warning of int (** Likely to cause an error if not fixed *)
  | Info (** Information for end users *)
  | Debug (** Information for whatwhat developers *)

val show_level : level -> string

type entity =
  | RawForecastProject of (string, int) Either.t
      (** Project name, or issue number if it could be found. *)
  | ForecastProject of int (** The project number *)
  | Project of int (** The project number *)
  | RawForecastPlaceholder of string (** Placeholder's name *)
  | RawForecastPerson of string (** Person's name *)
  | ForecastPerson of string (** Email address *)
  | Person of string (** email address *)
  | RawForecastAssignment of int (** Assignment ID in Forecast *)
  | Assignment of (int * string) (** Pair of a Project and a person *)
  | Other

type event =
  { level : level
  ; entity : entity
  ; message : string
  }

val log' : event -> unit
val get_the_log : unit -> event Seq.t
val isError : event -> bool
val isWarning : event -> bool
val isInfo : event -> bool
val isDebug : event -> bool

(** A type which determines whether specific error codes are to be suppressed or
    filtered for. *)
type code_spec =
  | Without of level list
  | Only of level list
  | All

(** Returns a list of events taken from the log, paired with the corresponding
    GitHub issue number if one could be found. The [verbose], [restrict_codes],
    and [restrict_issues] are used to decide which events are included in the
    output. *)
val gather_events
  :  verbose:int
  -> restrict_codes:code_spec
  -> restrict_issues:int list option
  -> (int option * event) list

(** The same as above, but events with the same issue number (or lack thereof)
    are gathered into a single list. This lets you iterate over issue numbers
    instead of events. *)
val gather_events'
  :  verbose:int
  -> restrict_codes:code_spec
  -> restrict_issues:int list option
  -> (int option * event list) list

(** Pretty-print all events in the log to standard output. *)
val pretty_print
  :  use_color:bool
  -> verbose:int
  -> restrict_codes:code_spec
  -> restrict_issues:int list option
  -> unit

