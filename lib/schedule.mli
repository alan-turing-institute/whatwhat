(** Schedule collates data from both GitHub and Forecast, and joins them into a
    single, source-agnostic data structure. It deals with people, projects,
    assignments, and allocations.

    Forecast is authoritative for persons and assignments; GitHub is
    authoritative for projects.
 *)

open Domain

(** Return a list of people, projects, and assignments for which we succesfully
    merged Forecast and GitHub data. In the process of doing the merge, log
    various warnings and errors when data on Forecast and/or GitHub is missing
    or malformed. *)
val get_the_schedule
  :  start_date:CalendarLib.Date.t
  -> end_date:CalendarLib.Date.t
  -> person list * project IntMap.t * assignment list * int list
