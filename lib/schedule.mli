(** Schedule collates data from both Github and Forecast, and joins them into a
    single, source-agnostic data structure. It deals with people, projects,
    assignments, and allocations.

    Forecast is authoritative for persons and assignments; GitHub is
    authoritative for projects.
 *)

open Domain

(* Other data that we need to have somewhere, but maybe not here? :
  ; github_assignees : string list
  ; reactions : (string * string) list
 *)

(** Return a list of people and projects for which we succesfully merged
    Forecast and Github data. In the process of doing the merge, log various
    warnings and errors when data on Forecast and/or Github is missing or
    malformed.

    TODO This function remains a work-in-progress. For one, we need to have it
    return a list of allocations as well. *)
val get_the_schedule : unit -> person list * project list * assignment list
