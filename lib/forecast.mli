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

type assignment =
  { project : int
  ; person : string
  ; finance_code : string option
  }

type schedule =
  { projects : project IntMap.t
  ; people : person StringMap.t
  ; assignments : assignment list
  }

val getTheSchedule : Date.t -> Date.t -> schedule
val getTheCurrentSchedule : int -> schedule
