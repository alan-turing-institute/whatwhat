(** Raw interface to Forecast. Returns typed data that is essentially the JSON
    form returned by Forecast. *)

open CalendarLib
module IdMap : module type of Map.Make (Int)

type client =
  { id : int
  ; name : string
  ; archived : bool
  }

type project =
  { id : int
  ; harvest_id : int option
  ; client_id : int option
  ; name : string
  ; code : string option
  ; tags : string list
  ; notes : string option
  ; archived : bool
  }

type person =
  { id : int
  ; first_name : string
  ; last_name : string
  ; email : string option
  ; login : string
  ; roles : string list
  ; archived : bool
  }

type placeholder =
  { id : int
  ; name : string
  ; roles : string list
  ; archived : bool
  }

type assignment =
  { id : int
  ; project_id : int
  ; person_id : int option
  ; placeholder_id : int option
  ; start_date : string
  ; end_date : string
  ; allocation : int
  ; notes : string option
  }

val get_clients : unit -> client list
val show_client : client -> string
val get_projects : unit -> project list
val show_project : project -> string
val get_people : unit -> person list
val show_person : person -> string
val get_placeholders : unit -> placeholder list
val show_placeholder : placeholder -> string

(** Return all assignments which overlap a given date range. The range is from
    [start_date] to [end_date], inclusive, and all overlapping assignments are 
    returned in full. (In particular, any given returned [assignment] 
    may start earlier than [start_date] and end later than [end_date].)

    The end date may not be more than 180 days after start date.
 *)
val get_assignments : Date.t -> Date.t -> assignment list

val show_assignment : assignment -> string

val get_the_schedule
  :  Date.t
  -> Date.t
  -> client IdMap.t
     * person IdMap.t
     * placeholder IdMap.t
     * project IdMap.t
     * assignment list
