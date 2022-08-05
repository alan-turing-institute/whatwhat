(** Raw interface to Forecast. Returns typed data that is essentially the JSON
    form returned by Forecast. *)

open CalendarLib

module IdMap : module type of Map.Make (Int)

type client = { id : int; name : string; archived : bool }

type project = {
  id : int;
  harvest_id : int option;
  client_id : int option;
  name : string;
  code : string option;
  tags : string list;
  notes : string option;
  archived : bool;
}

type person = {
  id : int;
  first_name : string;
  last_name : string;
  email : string option;
  login : string;
  roles : string list;
  archived : bool;
}

type placeholder = {
  id : int;
  name : string;
  roles : string list;
  archived : bool;
}

type assignment = {
  id : int;
  project_id : int;
  person_id : int option;
  placeholder_id : int option;
  start_date : string;
  end_date : string;
  allocation : int;
  notes : string option;
}

val getClients : unit -> client list
val show_client : client -> string
val getProjects : unit -> project list
val show_project : project -> string
val getPeople : unit -> person list
val show_person : person -> string
val getPlaceholders : unit -> placeholder list
val show_placeholder : placeholder -> string

val getAssignments : Date.t -> Date.t -> assignment list
(** Return all assignments wich overlap a given date range. The range is from
    [startDate] to [endDate], inclusive, and all overlapping assignments are 
    returned in full. (In particular, any given returned [assignment] 
    may start earlier than [startDate] and end later than [endDate].)

    The end date may not be more than 180 days after start date.
 *)

val show_assignment : assignment -> string

val getTheSchedule : Date.t -> Date.t -> client IdMap.t * person IdMap.t * placeholder IdMap.t * project IdMap.t * assignment list
