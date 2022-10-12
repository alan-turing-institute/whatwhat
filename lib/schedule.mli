(** Schedule collates data from both Github and Forecast, and joins them into a
    single, source-agnostic data structure. It deals with people, projects,
    assignments, and allocations.

    Forecast is authoritative for persons and assignments; GitHub is
    authoritative for projects.
    
    {1 Definitions}
    
    Roughly, an assignment says who is assigned to which project and an
    allocation says how much they are assigned. More precisely: 

    A {!rate} is a number, representing the hours per day at which a person is
    assigned to a project out of a nominal maximum of 8.0.
    
    An {!allocation} is conceptually a map from days to rates, representing the
    total time that a person is allocated to a project on a given day. We
    represent an allocation as a set of {i primitive allocations}, where a
    primitive allocation is a contiguous period of time together with a
    rate. The allocation rate on a given day is the sum of the rates of all
    primitive allocations that include that day.

    An {!assignment} is a triple of a project, a person, and an allocation.
    
    Finally a {!schedule} is a set of persons, of projects, and
    assignments. The schedule actually contains, for each of these kinds of
    entity, a map from the unique identifier to the entity.

    
    {1 Dates and rates}

    We deal in days as the atomic unit of allocations although, in practice, we
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

open Allocation

type resource = Github.fte_time =
  | FTEWeeks of float
  | FTEMonths of float (** *)
(** A total resource, represented either as FTE-weeks or FTE-months *)

type plan = Github.plan =
  {
    budget : resource
  ; latest_start_date : CalendarLib.Date.t
  ; earliest_start_date : CalendarLib.Date.t option
  (** [earliest_start_date = None] means "can start as soon as you like" *)
  ; latest_end_date : CalendarLib.Date.t option
  (** [latest_end_date = None] means "can end whenever you like" *)
  ; nominal_fte_percent : float
  ; max_fte_percent : float
  ; min_fte_percent : float
  }

type person =
  { email : string;
    name : string;
    (* Should these be in a different data structure? *)
    (* ; github_handle : string option *)
    (* ; slack_handle : string option *) 
  }
(** A person *)

type project_state =
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
(* Should this be a polymorphic variant? *)
(** The project status on the Project Tracker.
 *)

type project =
  { nmbr : int  (** The issue number from GitHub *)
  ; name : string
  ; state : project_state
  ; programme : string option
  
  ; github_assignees : string list
  ; reactions : (string * string) list

  ; budget : resource
  ; latest_start_date : CalendarLib.Date.t
  ; earliest_start_date : CalendarLib.Date.t option
  (** [earliest_start_date = None] means "can start as soon as you like" *)
  ; latest_end_date : CalendarLib.Date.t option
  (** [latest_end_date = None] means "can end whenever you like" *)
  ; nominal_fte_percent : float
  ; max_fte_percent : float
  ; min_fte_percent : float
  }
(** A project, combining both a Github issue and a matching Forecast project. *)

(* Other data that we need to have somewhere, but maybe not here? :
  ; github_assignees : string list
  ; reactions : (string * string) list
 *)

type assignment = Forecast.assignment =
  { project : int
  ; person : string
  ; finance_code : string option
  ; allocation : allocation
  }



module IntMap : module type of Map.Make(Int)
module StringMap : module type of Map.Make(String)

type schedule =
  {
   projects : project IntMap.t;
   people : person StringMap.t;
   assignments : assignment list
  }
   
(** Return a list of people and projects for which we succesfully merged
    Forecast and Github data. In the process of doing the merge, log various
    warnings and errors when data on Forecast and/or Github is missing or
    malformed.

    TODO This function remains a work-in-progress. For one, we need to have it
    return a list of allocations as well. *)
val get_the_schedule : unit -> person list * project list * assignment list

val show_person : person -> string
val show_project : project -> string
val show_assignment : assignment -> string
