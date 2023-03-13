(** Raw interface to Forecast. Returns typed data that is essentially the JSON
    form returned by Forecast. *)

open CalendarLib

(** An [IdMap] is a map from Forecast IDs. *)
module IdMap : module type of Map.Make (Int)

(** A Forecast [client] is usually what we call a programme, i.e. categories of
    projects. *)
type client =
  { id : int
  ; name : string
  ; archived : bool
  }

type project_schema =
  { id : int
  ; harvest_id : int option
  ; client_id : int option
  ; name : string
  ; code : string option
  ; tags : string list
  ; notes : string option
  ; color : string
  ; archived : bool
  }

(** A project on Forecast. *)
type project =
  { id : int
  ; harvest_id : int option
  ; client : client option
  ; name : string
  ; code : string option
  ; tags : string list
  ; notes : string option
  ; color : string
  ; archived : bool
  }

(** A person on Forecast. *)
type person =
  { id : int
  ; first_name : string
  ; last_name : string
  ; email : string option
  ; login : string
  ; roles : string list
  ; archived : bool
  }

(** A placeholder on Forecast, which fulfils the same role as a person but isn't
    actually a person. *)
type placeholder =
  { id : int
  ; name : string
  ; roles : string list
  ; archived : bool
  }

(** A person or a placeholder. *)
type entity = Person of person | Placeholder of placeholder

(** Get the name, roles, id, or archived status of an entity. *)
val get_entity_name : entity -> string
val get_entity_roles : entity -> string list
val get_entity_id : entity -> int
val get_entity_archived : entity -> bool

(** An [assignment] refers to a period where a person is placed on a project. *)
type assignment =
  { id : int
  ; project : project
  ; entity: entity
  ; start_date : string
  ; end_date : string
  ; allocation : int (** This is measured in seconds per day. *)
  ; notes : string option
  }

(** These are convenience functions to show the data retrieved from Forecast. *)

val show_client : client -> string
val show_project : project -> string
val show_person : person -> string
val show_placeholder : placeholder -> string
val show_assignment : assignment -> string

(** Finally, the main function of this module is to retrieve all data from
    Forecast between two given dates. *)
val get_the_schedule
  :  start_date:Date.t
  -> end_date:Date.t
  -> client IdMap.t
     * person IdMap.t
     * placeholder IdMap.t
     * project IdMap.t
     * assignment list
