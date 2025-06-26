(** Pretty-print a project. *)

(** Pretty-print a project. The following details are shown:

    - The project name, issue number, and GitHub link
    - Metadata from the GitHub issue
    - A comparison between the budgeted FTEs and currently assigned FTEs on
      Forecast
    - Emoji reactions on the GitHub issue *)
val print
  :  use_color:bool
  -> Domain.project
  -> Domain.person list
  -> Domain.Assignment.t list
  -> unit

(** Pretty-print a list of current people working on a project. The following
    details are shown:

    - The project name, issue number, and GitHub link
    - People working on them any time in the past or next 1 month *)
val print_current_people
  :  use_color:bool
  -> Domain.project
  -> Domain.Assignment.t list
  -> unit
