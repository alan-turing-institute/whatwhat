(** High-level interface to the Forecast API.

    Returns only entities that are correctly defined according to [whatwhat]'s
    domain model. *)

module IntMap : module type of Map.Make (Int)
module StringMap : module type of Map.Make (String)

(** This type contains all the useful information about a project which can be
    extracted from Forecast.

    TODO: What about project code?
    *)
type project =
  { number : int (** The issue number on GitHub. *)
  ; name : string
  ; programme : string
  }

(** Obtain a valid Forecast schedule between two given dates. *)
val get_the_schedule
  :  start_date:CalendarLib.Date.t
  -> end_date:CalendarLib.Date.t
  -> project IntMap.t * Domain.person StringMap.t * Domain.assignment list
