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
  (** An [FTE.t] is a number, representing the FTE-equivalents at which a person
      is assigned to a project. 1.0 FTE corresponds to a nominal rate of 8.0
      hours per day, or equivalently, 28800 seconds per day (the units in which
      Forecast reports allocations).
      *)
  type t
  (** [show t] prints "t FTE" where t is rounded to 2 decimal places. *)
  val show : t -> string

  (** Unsurprisingly, [FTE.t] is really just a [FTE of float] wrapper; however,
      we don't export the FTE constructor in order to restrict the creation of
      FTE values. The only way to create a new value with type [FTE.t] is to use
      the [from_forecast_rate] 'smart constructor'. *)
  val from_forecast_rate : int -> t
  val add : t -> t -> t
  (** Retrieve the float value wrapped in the FTE. Avoid using this where
      possible; it leads to ambiguity in the units. If you just want to print
      the value, use [show] instead. *)
  val get : t -> float

  (** [FTE.time] represent products of FTEs and a time period. 1 FTE, multiplied
      by 7 days, gives 1 FTE-week. The conversion factor between FTE-weeks and
      FTE-months is 12/52. *)
  type time = FTE_weeks of float | FTE_months of float
  (** [show_time t] prints "t FTE-weeks" where t is rounded to 2 decimal places.
      If [t] is in FTE-months it is converted first. *)
  val show_time : time -> string

  (** Convert a list of FTE-days to an FTE-week quantity. *)
  val sum_over_days : t list -> time
  (** Retrieve the number of FTE-weeks wrapped in the [FTE.time]. As above,
      avoid using this where possible. *)
  val weeks_in : time -> float
  (** Multiply an FTE-time period by a factor *)
  val mul_time : time -> float -> time
  (** Compare two FTE-time periods. Like the rest of the OCaml standard library,
      [compare_time a b] returns a positive integer for [a > b], negative for [a
      < b], and 0 for [a == b]. *)
  val compare_time : time -> time -> int
  (** Add up a list of FTE-time periods (for example, those belonging to
      different people on a project). *)
  val sum_time : time list -> time
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
type allocation = FTE.t DateMap.t

(** Turn details from a Forecast assignment into an allocation. *)
val make_allocation : CalendarLib.Date.t -> CalendarLib.Date.t -> FTE.t -> FTE.t DateMap.t

(** Merge two allocations. This function is commutative. *)
val combine_allocations : FTE.t DateMap.t -> FTE.t DateMap.t -> FTE.t DateMap.t

(** Get the day an allocation begins. *)
val get_first_day : FTE.t DateMap.t -> CalendarLib.Date.t

(** Get the day an allocation ends. *)
val get_last_day : FTE.t DateMap.t -> CalendarLib.Date.t

(** {1 Entities relevant to scheduling and planning} *)

(** A [plan] gives the constraints on the possible allocations to a project. *)
type project_plan =
  { budget : FTE.time
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

  val show_t : t -> string
end

(** A project, combining both a Github issue and a matching Forecast project. *)
type project =
  { number : int (** The issue number from GitHub *)
  ; name : string
  ; state : State.t
  ; programme : string option
  ; plan : project_plan
  }

exception UnknownColumn of string

(** Convert the column name in GitHub to a variant type. May raise UnknownColumn *)
val state_of_column : string -> State.t

(** A person *)
type person =
  { email : string (** Email is the primary key for persons *)
  ; full_name : string
  ; github_handle : string option
  ; slack_handle : string option
  }

(** An assignment of a person to a project for a specific allocation *)
type assignment =
  { project : project
  ; person : person
  ; allocation : allocation
  }

type schedule =
  { projects : project IntMap.t
  ; people : person StringMap.t
  ; assignments : assignment list
  }
