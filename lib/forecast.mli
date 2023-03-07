(** High-level interface to Forecast. Returns only entities that are correctly
    defined accoring to our domain model. *)

open CalendarLib
module IntMap : module type of Map.Make (Int)
module StringMap : module type of Map.Make (String)

type project =
  { number : int
  ; name : string
  ; programme : string
  }

val get_the_schedule
  :  start_date:Date.t
  -> end_date:Date.t
  -> project IntMap.t * Domain.person StringMap.t * Domain.assignment list

val get_the_current_schedule
  :  int
  -> project IntMap.t * Domain.person StringMap.t * Domain.assignment list
