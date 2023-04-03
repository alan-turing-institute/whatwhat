(** Raw interface to Forecast. Returns typed data that is essentially the JSON
    form returned by Forecast. *)

module IntMap : module type of Map.Make (Int)

(** {1 Clients}

    A Forecast [client] is what we usually call a programme, i.e. categories of
    projects. *)

type client =
  { id : int
  ; name : string
  ; archived : bool
  }

val show_client : client -> string

(** {1 Projects}

    Each Forecast [project] corresponds to a real-world project. *)

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

val show_project : project -> string

(** {1 People}

    Each Forecast [person] corresponds to a real-world person. These may be
    members of REG or other commercial/university partners; these can be
    differentiated based on their [roles]. *)

type person =
  { id : int
  ; first_name : string
  ; last_name : string
  ; email : string option
  ; login : string
  ; roles : string list
  ; archived : bool
  ; weekly_capacity : float  (** Seconds per week. This is not used except in Forecast team export. *)
  }

val show_person : person -> string

(** Construct the name of a person, which (for the purposes of whatwhat) is
    their first and last names joined by a space. *)
val make_person_name : person -> string

(** {1 Placeholders}

    A placeholder on Forecast fulfils the same role as a person, but is not
    actually a person. Placeholders are used on Forecast to denote other
    information about projects, e.g. whether they need people on them. *)

type placeholder =
  { id : int
  ; name : string
  ; roles : string list
  ; archived : bool
  }

val show_placeholder : placeholder -> string

(** {1 Entities}

    An entity can be a person or a placeholder. Each assignment in Forecast
    must be to one, and only one, entity. *)

type entity =
  | Person of person
  | Placeholder of placeholder

val show_entity : entity -> string

(** Get the name of an entity.
    The name of a person is their first and last names joined together by a
    space. The name of a placeholder is "Placeholder: <name in Forecast>". *)
val get_entity_name : entity -> string

(** Get the roles of an entity. *)
val get_entity_roles : entity -> string list

(** Get the ID of an entity. *)
val get_entity_id : entity -> int

(** Get the archived status of an entity. *)
val get_entity_archived : entity -> bool

(** {1 Assignments}

    An [assignment] refers to a period where an entity is placed on a project.
    The start and end dates are inclusive. *)

type assignment =
  { id : int
  ; project : project
  ; entity : entity
  ; start_date : CalendarLib.Date.t
  ; end_date : CalendarLib.Date.t
  ; allocation : int (** This is measured in seconds per day. *)
  ; notes : string option
  }

val show_assignment : assignment -> string

(** {1 Get the schedule}
    
    The main function of this module. *)

(** Retrieves all assignments from Forecast between the two given dates, as
    well as all other data required to make sense of the assignments, such as
    clients, people, placeholders, and projects. For an explanation of the data
    types involved, see the sections above. *)
val get_the_schedule
  :  start_date:CalendarLib.Date.t
  -> end_date:CalendarLib.Date.t
  -> client IntMap.t
     * person IntMap.t
     * placeholder IntMap.t
     * project IntMap.t
     * assignment list
