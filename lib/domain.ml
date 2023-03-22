(* Domain model *)

module IntMap = Map.Make (Int)
module StringMap = Map.Make (String)

type resource =
  | FTE_weeks of float
  | FTE_months of float
[@@deriving show]

type rate = Rate of float

type simple_allocation =
  { start_date : CalendarLib.Date.t
  ; days : CalendarLib.Date.Period.t (* [days >=0] must be true *)
  ; rate : rate
  }

type allocation = simple_allocation list

type project_plan =
  { budget : resource
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
    | ExtraInfoNeeded -> "ExtraInfoNeeded"
    | ProjectAppraisal -> "ProjectAppraisal"
    | AwaitingGoNogo -> "AwaitingGoNogo"
    | FindingPeople -> "FindingPeople"
    | AwaitingStart -> "AwaitingStart"
    | Active -> "Active"
    | CompletionReview -> "CompletionReview"
    | Done -> "Done"
    | Cancelled -> "Cancelled"
    | Rejected -> "Rejected"
  ;;
end

exception UnknownColumn of string

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
  | _ -> raise (UnknownColumn ("Unknown GitHub column: " ^ col))
;;

type project =
  { number : int (** The issue number from GitHub *)
  ; name : string
  ; state : State.t
  ; programme : string option
  ; plan : project_plan
  }

type person =
  { email : string (** Email is the primary key for persons *)
  ; full_name : string
  ; github_handle : string option
  ; slack_handle : string option
  }

type assignment =
  { project : int (* The project code *)
  ; person : string (* An email *)
  ; finance_code : string option
  ; allocation : allocation
  }

let show_project_plan plan =
  let dts = CalendarLib.Printer.Date.to_string in
  String.concat
    ""
    [ "{**Domain.project_plan**"
    ; "\n"
    ; "Budget: "
    ; plan.budget |> show_resource
    ; "Finance codes: ["
    ; String.concat ";" plan.finance_codes
    ; "]"
    ; "\n"
    ; "Latest start date: "
    ; dts plan.latest_start_date
    ; "\n"
    ; "Earliest start date: "
    ; (match plan.earliest_start_date with
       | Some x -> "Some " ^ dts x
       | None -> "None")
    ; "\n"
    ; "Latest end date: "
    ; (match plan.latest_end_date with
       | Some x -> "Some " ^ dts x
       | None -> "None")
    ; "\n"
    ; Printf.sprintf "Nominal FTE percent: %f\n" plan.nominal_fte_percent
    ; Printf.sprintf "Maximum FTE percent: %f\n" plan.max_fte_percent
    ; Printf.sprintf "Minimum FTE percent: %f\n" plan.min_fte_percent
    ; "}"
    ]
;;

(* type project = *)
(*   { nmbr : int  *)
(*   ; name : string *)
(*   ; state : State.t *)
(*   ; programme : string option *)
(*   ; plan : project_plan *)
(*   } *)
let show_project proj =
  String.concat
    ""
    [ "{**Domain.project**\n"
    ; Printf.sprintf "GitHub issue number: %d\n" proj.number
    ; Printf.sprintf "Name: %s\n" proj.name
    ; Printf.sprintf "State: %s\n" (State.show_t proj.state)
    ; "Programme: "
    ; (match proj.programme with
       | Some x -> "Some " ^ x
       | None -> "None")
    ; "\n"
    ; "Project plan: "
    ; show_project_plan proj.plan
    ; "\n"
    ; "}"
    ]
;;

let show_allocation alloc =
  let days = CalendarLib.Date.Period.nb_days alloc.days in
  match alloc.rate with
  | Rate f ->
    Printf.sprintf
      " Start date: %s; Duration: %d days; Rate: %f"
      (CalendarLib.Printer.Date.to_string alloc.start_date)
      days
      f
;;

let show_assignment asn =
  let pf = Printf.sprintf in
  String.concat
    "\n"
    ([ pf "{**Domain.assignment**"
     ; pf "    Project : %d" asn.project
     ; pf "    Person : %s" asn.person
     ]
    @ (match asn.finance_code with
       | Some f -> [ pf "    Finance code : %s" f ]
       | None -> [])
    @ [ pf "    Allocations : [" ]
    @ List.map show_allocation asn.allocation
    @ [ pf "    ]"; pf "}" ])
;;

type schedule =
  { projects : project IntMap.t
  ; people : person StringMap.t
  ; assignments : assignment list
  }
