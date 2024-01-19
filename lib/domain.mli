(** Domain model

    This module declares the types which represent the domain model, independent
    of how GitHub and Forecast (or other systems) store the information.

    It also defines a set of utilities for working with some of those types
    (primarily the ones involving periods of time).

 *)

module IntMap : module type of Map.Make (Int)
module StringMap : module type of Map.Make (String)
module DateMap : module type of Map.Make (CalendarLib.Date)

(** {1 Measures and units} *)

module FTE : sig
  (** An [FTE.hour] is a number, representing the number of hours a person works
      on a given project on a day. 1 FTE is 8 hours per day.
      *)
  type hour

  (** Unsurprisingly, [FTE.hour] is really just an [Hour of float] wrapper;
      however, we don't export the [Hour] constructor in order to restrict the
      creation of [FTE.hour] values. The only way to create a new value with
      type [FTE.hour] is to use the [from_forecast_rate] 'smart constructor'. *)
  val from_forecast_rate : int -> hour

  (** Or, [zero] lets you make an empty value of 0 hours. *)
  val zero : hour

  (** Add up two [hour]s. *)
  val add_hours : hour -> hour -> hour

  (** [FTE.t] represent products of FTEs and a time period. 40 hours corresponds
      to 1 FTE-week. The conversion factor between FTE-weeks and FTE-months is
      12/52. *)
  type t =
    | Weeks of float
    | Months of float

  (** [show_t t] prints "t FTE-weeks" where t is rounded to 2 decimal places.
      If [t] is in FTE-months it is converted first. *)
  val show_t : t -> string

  (** Convert a list of hours to an FTE-week quantity. *)
  val sum_to_weeks : ?is_placeholder:bool -> hour list -> t

  (** Add two FTE-ts. *)
  val add : t -> t -> t

  (** Subtract the second FTE.t from the first. *)
  val sub : t -> t -> t

  (** Multiply an FTE.t period by a factor. *)
  val mul : t -> float -> t

  (** Get the ratio of two FTE.ts. *)
  val div : t -> t -> float

  (** Compare two FTE.t periods. Like the rest of the OCaml standard library,
      [compare_t a b] returns a positive integer for [a > b], negative for [a
      < b], and 0 for [a == b]. *)
  val compare : t -> t -> int

  (** Add up a list of FTE.t periods (for example, those belonging to
      different people on a project). *)
  val sum : t list -> t
end

(** {1 Periods of time} *)

(** We deal in days as the atomic unit of allocations, although, in practice, we
    try to ensure that allocations are aligned to weeks (starting on Monday and
    ending on Sunday) and, if possible, to months (starting on the Monday of the
    first week in a month).
     
    In order to assign weeks to months we use the following rule: A week belongs
    to a month if, and only if, the Thursday of the week falls in the month. (We
    use this rule to be consistent with the ISO 8601 rule for when a week
    belongs to a year.)
    
    The total resource available for a project tends to be specified in months
    of FTE. However, we occasionally need to assign people at fractions of the
    nominal maximum rate, or add non--month-aligned dates. In these cases it may
    be necessary to convert to a finer granularity of time, such as weeks of
    FTE. To make this conversion we use a ratio of 12 months to 52 weeks, no
    matter which months were originally scheduled.
 *)

(** An [allocation] is conceptually a map from days to rates, representing the
    total time that a person is allocated to a project on a given day. *)
type allocation = FTE.hour DateMap.t

(** Turn details from a Forecast assignment into an allocation. *)
val make_allocation
  :  with_weekends:bool
  -> CalendarLib.Date.t
  -> CalendarLib.Date.t
  -> FTE.hour
  -> FTE.hour DateMap.t

(** Merge two allocations. This function is commutative. *)
val combine_allocations : FTE.hour DateMap.t -> FTE.hour DateMap.t -> FTE.hour DateMap.t

(** Get the day an allocation begins. *)
val get_first_day : 'a DateMap.t -> CalendarLib.Date.t

(** Get the day an allocation ends. *)
val get_last_day : 'a DateMap.t -> CalendarLib.Date.t

(** Given a date (intended to be a Monday) and an allocation, return
    the total FTE-weeks in that allocation for the 7 days starting on
    the given date. *)
val fte_of_week :  ?is_placeholder:bool -> allocation -> CalendarLib.Date.t -> FTE.t 


(** {1 Entities relevant to scheduling and planning} *)

(** A person *)
type person =
  { email : string (** Email is the primary key for persons *)
  ; full_name : string
  ; github_handle : string option
  ; slack_handle : string option
  }

(** A placeholder on Forecast. *)
type placeholder = { name : string }

(** An entity is a person or a placeholder. *)
type entity =
  | Person of person
  | Placeholder of placeholder

(** Get the name of an entity. *)
val get_entity_name : entity -> string

(** A [plan] gives the constraints on the possible allocations to a project. *)
type project_plan =
  { budget : FTE.t
  ; finance_codes : string list
  ; latest_start_date : CalendarLib.Date.t
  ; earliest_start_date : CalendarLib.Date.t option
      (** [earliest_start_date = None] means "can start as soon as you like" *)
  ; latest_end_date : CalendarLib.Date.t option
      (** [latest_end_date = None] means "can end whenever you like" *)
  ; nominal_fte_percent : float
  ; max_fte_percent : float
  ; min_fte_percent : float
  }

(** The project status on the Project Tracker, shown by the column the project
    issue is in.
 *)
module State : sig
  type t =
    | Suggested
    | Proposal
    | ExtraInfoNeeded
    | ProjectAppraisal
    | AwaitingGoNogo
    | FindingPeople
    | AwaitingStart
    | Active
    | CompletionReview
    | Done
    | Cancelled
    | Rejected
    | Other

  val show_t : t -> string
end

(** A project, combining both a Github issue and a matching Forecast project. *)
type project =
  { number : int (** The issue number from GitHub *)
  ; name : string
  ; state : State.t
  ; programme : string option
  ; plan : project_plan option
  ; assignees : person list
  }

(** Convert the column name in GitHub to a variant type. May raise UnknownColumn *)
val state_of_column : string -> State.t

(** The types of emoji reactions we care about. *)
type emoji =
  | Laugh
  | ThumbsUp
  | ThumbsDown
  | Other

(** Parse emoji from the results returned by the GitHub API. *)
val parse_emoji : string -> emoji

(** An assignment of an entity to a project for a specific allocation. *)
type assignment =
  { project : project
  ; entity : entity
  ; allocation : allocation
  }

module Assignment : sig
  type t = assignment

  (** Check whether an assignment is to a person. *)
  val is_person : t -> bool

  (** The status of the assignment. *)
  type time =
    | Past
    | Current
    | Future

  (** Calculate the number of FTE-weeks in a given assignment. Note that
      one assignment may include more than one allocation on Forecast as they
      have been merged. *)
  val to_fte_weeks : t -> FTE.t

  (** Get the entity name *)
  val get_entity_name : t -> string

  (** Return the time status of an assignment as a [time]. *)
  val get_time_status : t -> time

  (** Return the time status of an assignment as a string. *)
  val show_time_status : t -> string

  (** First compare assignments by start date, then end date. *)
  val compare_by_date : t -> t -> int
end

type schedule =
  { projects : project IntMap.t
  ; people : person StringMap.t
  ; assignments : Assignment.t list
  }
