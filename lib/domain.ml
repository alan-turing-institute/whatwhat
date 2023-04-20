(* Domain model *)

module IntMap = Map.Make (Int)
module StringMap = Map.Make (String)
module DateMap = Map.Make (CalendarLib.Date)

(** --- FTEs and times ----------------------- *)

module FTE = struct
  type hour = Hour of float

  let get (Hour h) = h
  let add_hours (Hour h1) (Hour h2) = Hour (h1 +. h2)
  let from_forecast_rate n = Hour (float_of_int n /. 3600.)

  type t =
    | Weeks of float
    | Months of float

  let mpw = 12. /. 52. (* Months per week *)

  let show_t = function
    | Weeks w -> Printf.sprintf "%.2f FTE-weeks" w
    | Months m -> Printf.sprintf "%.2f FTE-weeks" (m /. mpw)
  ;;

  let weeks_in = function
    | Weeks w -> w
    | Months m -> m /. mpw
  ;;

  let sum_to_weeks ?(is_placeholder = false) (hs : hour list) : t =
    let hours_per_week = if is_placeholder then 56. else 40. in
    (* Sum hours per day *)
    let rec sum_hours ts =
      match ts with
      | [] -> 0.
      | y :: ys -> get y +. sum_hours ys
    in
    (* Then convert to FTE-weeks *)
    Weeks (sum_hours hs /. hours_per_week)
  ;;

  let add tm1 tm2 =
    let w1, w2 = weeks_in tm1, weeks_in tm2 in
    Weeks (w1 +. w2)
  ;;

  let sub tm1 tm2 =
    let w1, w2 = weeks_in tm1, weeks_in tm2 in
    Weeks (w1 -. w2)
  ;;

  let mul tm y =
    match tm with
    | Weeks x -> Weeks (x *. y)
    | Months x -> Months (x *. y)
  ;;

  let div tm1 tm2 =
    let w1, w2 = weeks_in tm1, weeks_in tm2 in
    w1 /. w2
  ;;

  let compare tm1 tm2 = compare (weeks_in tm1) (weeks_in tm2)
  let sum tms = List.fold_left add (Weeks 0.) tms
end

(** --- Allocations -------------------------- *)

type allocation = FTE.hour DateMap.t

let make_allocation ~with_weekends start_date end_date fte =
  let open CalendarLib.Date in
  let is_weekend date =
    match day_of_week date with
    | Sat | Sun -> true
    | _ -> false
  in
  let rec accum date map =
    if date > end_date
    then map
    else (
      let next_day = add date (Period.day 1) in
      if is_weekend date && not with_weekends
      then accum next_day map
      else DateMap.add date fte (accum next_day map))
  in
  accum start_date DateMap.empty
;;

let combine_allocations a1 a2 =
  DateMap.union (fun _ v1 v2 -> Some (FTE.add_hours v1 v2)) a1 a2
;;

let get_first_day a1 = DateMap.min_binding a1 |> fst
let get_last_day a1 = DateMap.max_binding a1 |> fst

(** --- Project metadata --------------------- *)

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

module State = struct
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

  let show_t t =
    match t with
    | Suggested -> "Suggested"
    | Proposal -> "Proposal"
    | ExtraInfoNeeded -> "Extra info needed"
    | ProjectAppraisal -> "Project appraisal"
    | AwaitingGoNogo -> "Awaiting go/no-go"
    | FindingPeople -> "Finding people"
    | AwaitingStart -> "Awaiting start"
    | Active -> "Active"
    | CompletionReview -> "Completion review"
    | Done -> "Done"
    | Cancelled -> "Cancelled"
    | Rejected -> "Rejected"
  ;;
end

let state_of_column col =
  match col with
  | "Suggested" -> State.Suggested
  | "Proposal" -> State.Proposal
  | "Extra info needed" -> State.ExtraInfoNeeded
  | "Project appraisal" -> State.ProjectAppraisal
  | "Awaiting go/no-go" -> State.AwaitingGoNogo
  | "Finding people" -> State.FindingPeople
  | "Awaiting start" -> State.AwaitingStart
  | "Active" -> State.Active
  | "Completion review" -> State.CompletionReview
  | "Done" -> State.Done
  | "Cancelled" -> State.Cancelled
  | "Rejected" -> State.Rejected
  | _ -> failwith ("Unknown GitHub column: " ^ col)
;;

type project =
  { number : int (** The issue number from GitHub *)
  ; name : string
  ; state : State.t
  ; programme : string option
  ; plan : project_plan
  }

(** --- Entities ----------------------------- *)

type person =
  { email : string (** Email is the primary key for persons *)
  ; full_name : string
  ; github_handle : string option
  ; slack_handle : string option
  }

type placeholder = { name : string }

type entity =
  | Person of person
  | Placeholder of placeholder

let get_entity_name = function
  | Person p -> p.full_name
  | Placeholder p -> Printf.sprintf "Placeholder: %s" p.name
;;

(** --- Emoji reactions ---------------------- *)

type emoji =
  | Laugh
  | ThumbsUp
  | ThumbsDown
  | Other (** We don't actually care about the rest. *)

let parse_emoji e =
  match e with
  | "laugh" -> Laugh
  | "+1" -> ThumbsUp
  | "-1" -> ThumbsDown
  | _ -> Other
;;

(** --- Assignments -------------------------- *)

type assignment =
  { project : project
  ; entity : entity
  ; allocation : allocation
  }

module Assignment = struct
  type t = assignment

  type time =
    | Current
    | Past
    | Future

  let is_person (asn : t) =
    match asn.entity with
    | Person _ -> true
    | _ -> false
  ;;

  let compare_by_date (a1 : t) (a2 : t) =
    match
      CalendarLib.Date.compare (get_first_day a1.allocation) (get_first_day a2.allocation)
    with
    | 0 ->
      CalendarLib.Date.compare (get_last_day a1.allocation) (get_last_day a2.allocation)
    | n -> n
  ;;

  let to_fte_weeks (asn : t) =
    let is_placeholder = not (is_person asn) in
    asn.allocation |> DateMap.bindings |> List.map snd |> FTE.sum_to_weeks ~is_placeholder
  ;;

  let get_entity_name (asn : t) = get_entity_name asn.entity

  let get_time_status (asn : t) =
    let open CalendarLib in
    let today = Date.today () in
    let start_date = get_first_day asn.allocation in
    let end_date = get_last_day asn.allocation in
    if Date.compare today start_date < 0
    then Future
    else if Date.compare today end_date > 0
    then Past
    else Current
  ;;

  let show_time_status (asn : t) =
    match get_time_status asn with
    | Current -> "current"
    | Past -> "past"
    | Future -> "future"
  ;;
end

type schedule =
  { projects : project IntMap.t
  ; people : person StringMap.t
  ; assignments : Assignment.t list
  }
