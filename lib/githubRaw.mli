type person =
  { login : string
  ; name : string option
  ; email : string option
  }

  type metadata = 
  { turing_project_code : string option
  ; earliest_start_date : string (* unbound value of Date.pp means Date doesn't like @@derviing*)
  ; latest_start_date : string (*Date.t*)
  ; latest_end_date : string (*Date.t*)
  ; fte_months : float
  ; nominal_fte_percent : float
  ; max_fte_percent : float option
  ; min_fte_percent : float option
  }

type issue =
  { number : int
  ; title : string
  ; metadata : metadata option
  ; body : string option
  ; state : string
  ; assignees : person list
  ; reactions : (string * person) list
  ; column : string option
  }

type column =
  { name : string
  ; cards : (issue * string) list
  }

type project =
  { number : int
  ; name : string
  ; columns : column list
  }

type project_root = { projects : project list }

val show_person : person -> string
val show_issue : issue -> string
val show_column : column -> string
val show_project : project -> string
val show_project_root : project_root -> string
val get_project_issues : string -> issue list
val get_users : unit -> person list
