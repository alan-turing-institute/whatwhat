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

type person =
  { email : string
  ; first_name : string
  ; last_name : string
  }

type allocation =
  { start_date : CalendarLib.Date.t
  ; end_date : CalendarLib.Date.t
  ; rate : float
  }

type assignment =
  { project : int
  ; person : string
  ; finance_code : string option
  ; allocations : allocation list
  }

type schedule =
  { projects : project IntMap.t
  ; people : person StringMap.t
  ; assignments : assignment list
  }

val get_the_schedule : Date.t -> Date.t -> schedule
val get_the_current_schedule : int -> schedule
val show_project : project -> string
val show_person : person -> string
val show_allocation : allocation -> string
val show_assignment : assignment -> string
