(** GithubRaw queries the Github GraphQL API for data about issues, users, and
    project boards, and parses that into OCaml records. It throws exceptions if the data
    is too broken for even this parsing, but doesn't otherwise check for consistency or
    quality of what it receives. *)

type person =
  { login : string
  ; name : string option
  ; email : string option
  }
[@@deriving show]

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
[@@deriving show]

(* This is from the graphQL API *)
type column =
  { name : string
  ; cards : (issue * string) list
      (* The string is a cursor, would be nice to have a type alias for it. *)
  }
[@@deriving show]

(* This is from the REST API *)
type rest_column =
  { name : string
  ; id : int
  ; issues : (issue * string) list
  }
[@@deriving show]

type project =
  { number : int
  ; name : string
  ; columns : column list
  }
[@@deriving show]

type project_root = { projects : project list } [@@deriving show]

(** Return the list of issues in a project board, given the name of the board. *)
val get_project_issues : ?column_names:string list -> string -> issue list

(** Return all the users in the Alan Turing Institute Github organisation. *)
val all_hut23_users : person list

val run_github_query_async
  :  ?methd:string
  -> ?params:(string * string list) list
  -> ?body:string
  -> string
  -> string
  -> Yojson.Basic.t Lwt.t

val run_github_query
  :  ?methd:string
  -> ?params:(string * string list) list
  -> ?body:string
  -> string
  -> string
  -> Yojson.Basic.t

val get_issue : ?col_name:string -> int -> issue
val get_issue_numbers_in_column : rest_column -> (int * string) list
