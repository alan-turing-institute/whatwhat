(** High-level interface to Forecast. Returns only entities that are correctly
    defined accoring to our domain model. *)

open CalendarLib
open Domain

module IntMap : module type of Map.Make (Int)
module StringMap : module type of Map.Make (String)

type project =
  { number : int
  ; name : string
  ; programme : string
  }

val get_the_schedule : Date.t -> Date.t -> schedule
val get_the_current_schedule : int -> schedule
val show_project : project -> string
val show_person : person -> string
