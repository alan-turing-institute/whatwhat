(** Logging and notification: tell certain people about certain problems

    The following individuals may receive messages:

    - Fallback (email): A hard-coded person if the Scheduler cannot be found
    - The Scheduler (GitHub): The people listed in the GitHub service area for Scheduling
    - Project Shepherd (GitHub): Anyone listed in the project GitHub issue
    - Project Team (email): Anyone allocated to the project in the next six months
    - Programme Shepherd (GitHub): Anyone listed in the Programme Service area
    - Programe Lead (Forecast): Anyone allocated to the Programme Service area in the next six months.
    - The On-Call (Forecast): The person referred to by a particular assignment *)

(** A type for different types of log messages (e.g. various severities). *)
type log_type =
  | Error
  | Warning

(** Take a log_type and a message to log, print it to stdout in the standard logging
    format. *)
val log : log_type -> string -> unit

val show_log_type : log_type -> string
