(** GithubRaw queries the Github GraphQL API for data about issues, users, and
    project boards, and parses that into OCaml records. It throws exceptions if the data
    is too broken for even this parsing, but doesn't otherwise check for consistency or
    quality of what it receives. *)

(** A Github user. *)
type person =
  { login : string
  ; name : string option
  ; email : string option
  }

(** A Github issue. *)
type issue =
  { number : int
  ; title : string
  ; body : string
  ; state : string
  ; assignees : person list
  ; reactions : (string * person) list
  ; labels : string list
  ; column : string option
  }

(** A column in a Github project (a Kanban board). *)
type column =
  { name : string
  ; cards : (issue * string) list
  }

(** Project is what Github calls Kanban boards. It is _not_ what the rest of
    Whatwhat calls project, which is more like an issue. *)
type project =
  { number : int
  ; name : string
  ; columns : column list
  }

(** A list of Github projects in a repository. *)
type project_root = { projects : project list }

val show_person : person -> string
val show_issue : issue -> string
val show_column : column -> string
val show_project : project -> string
val show_project_root : project_root -> string

(** Return the list of issues in a project board, given the name of the board. *)
val get_project_issues : string -> issue list

(** Return all the users in the Alan Turing Institute Github organisation. *)
val get_users : unit -> person list
