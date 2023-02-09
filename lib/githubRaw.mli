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

(** Returns all users who can be assigned to issues in the
    alan-turing-institute/Hut23 GitHub repository. This essentially means
    anybody who can view the repository. *)
val all_hut23_users : person list

(** Run a GitHub query, returning a promise for the body JSON. *)
val run_github_query_async
  :  ?methd:string
  -> ?params:(string * string list) list
  -> ?body:string
  -> string
  -> string
  -> Yojson.Basic.t Lwt.t

(** Run a GitHub query, returning the body JSON directly. *)
val run_github_query
  :  ?methd:string
  -> ?params:(string * string list) list
  -> ?body:string
  -> string
  -> string
  -> Yojson.Basic.t

(** Get details and reactions for a GitHub issue. The [col_name] parameter
    allows the column of the issue to be specified if needed (since we are
    fetching the issue directly, the information can't be obtained via the
    project tracker).

    To populate the column name of an issue which should have one but doesn't
    yet, use the [populate_column_name] function.
    *)
val get_issue : ?col_name:string -> int -> issue

(** Find the column name of an issue (if one exists) and return the issue with
    updated metadata. Note that this function assumes that the issue lives in
    the 'Project Tracker' project. *)
val populate_column_name : ?project_issue_numbers:(int * string) list -> issue -> issue

(** Get a list of all issue numbers in a given column of a project. *)
val get_issue_numbers_in_column : rest_column -> (int * string) list

(** Get a list of all issue numbers in a given project. The [column_names]
    parameter can be used to only query specific columns of a project. *)
val get_project_issue_numbers : ?column_names:string list -> string -> (int * string) list

(** Get a list of all issues in a given project. The [column_names]
    parameter can be used to only query specific columns of a project. *)
val get_project_issues : ?column_names:string list -> string -> issue list
