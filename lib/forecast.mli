(** High-level interface to the Forecast API. *)

module IntMap : module type of Map.Make (Int)
module StringMap : module type of Map.Make (String)

(** The types in this module represent all the useful information we can
    obtain from Forecast (after validation). They are not the same as the types
    in the Domain module: those represent all possible information on a person /
    project / etc. after merging data from GitHub and Forecast.
    *)
type person =
  { full_name : string
  ; email : string
  ; roles : string list
  }

val compare_person : person -> person -> int

(** An entity is a person or a placeholder. Placeholders are represented
    directly using the [Domain.placeholder] type, because there is no extra
    information about placeholders to be gained from GitHub. *)
type entity =
  | Person of person
  | Placeholder of Domain.placeholder

(** Equality of entities *)
val entity_equal_P : entity -> entity -> bool 

(** Get the name of an entity. *)
val get_entity_name : entity -> string

(** This type contains all the useful information about a project which can be
    extracted from Forecast.
    *)
type project =
  { number : int (** The issue number on GitHub. *)
  ; name : string
  ; programme : string
  ; finance_code : string option
  }

type assignment =
  { project : project
  ; entity : entity
  ; allocation : Domain.allocation
  }

(** Obtain a valid Forecast schedule between two given dates. *)
val get_the_schedule
  :  start_date:CalendarLib.Date.t
  -> end_date:CalendarLib.Date.t
  -> project IntMap.t * person StringMap.t * assignment list

val get_the_schedule_async
  :  start_date:CalendarLib.Date.t
  -> end_date:CalendarLib.Date.t
  -> (project IntMap.t * person StringMap.t * assignment list) Lwt.t
