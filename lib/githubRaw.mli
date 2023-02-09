(** The [GithubRaw] module queries the Github GraphQL and REST APIs for data
    about issues, users, and project boards, and parses that into OCaml records.
    It throws exceptions if the data is too broken for even this parsing, but
    doesn't otherwise check for consistency or quality of what it receives. *)

(** This module only supports GET and POST requests. *)
type http_method =
  | GET
  | POST

(** A [person] is a GitHub user: they are identified by their login username, a
    real name, and an email. The latter two are obtained from their public
    profile (not their commits) and may be absent. *)
type person =
  { login : string
  ; name : string option
  ; email : string option
  }
[@@deriving show]

(** An [issue] is a GitHub issue on the
    {{: https://github.com/alan-turing-institute/Hut23/issues} Hut23
    repository}.
    *)
type issue =
  { number : int
  ; title : string
  ; body : string
  ; state : string (** "open" or "closed" *)
  ; assignees : person list
  ; reactions : (string * person) list
  ; labels : string list
  ; column : string option (** The name of a project column the issue is in *)
  }
[@@deriving show]

(* TODO: Determine if we really need this. All columns should be castable
   into rest_columns. *)

(** A column of a project on GitHub. This data type is used for data from
    the GraphQL API. *)
type column =
  { name : string
  ; cards : (issue * string) list
      (* The string is a cursor, would be nice to have a type alias for it. *)
  }
[@@deriving show]

(** A column of a project on GitHub. This data type is used for data from the
    REST API. *)
type rest_column =
  { name : string
  ; id : int
  ; issues : (issue * string) list (* The [string] here is the column name. *)
  }
[@@deriving show]

(* TODO: Document *)
type project =
  { number : int
  ; name : string
  ; columns : column list
  }
[@@deriving show]

(* TODO: Document *)
type project_root = { projects : project list } [@@deriving show]

(** This is a list of all users who can be assigned to issues in the
    repository, which essentially means anybody who can view the repository. *)
val all_users : person list

(** Run a GitHub query, returning a promise for the body JSON. The [body]
    parameter here is the request body, which is used only for POST requests.
    The final mandatory [string] parameter is the URI.
    *)
val run_github_query_async
  :  ?methd:http_method
  -> ?params:(string * string list) list
  -> ?body:string
  -> string
  -> Yojson.Basic.t Lwt.t

(** Run a GitHub query, returning the body JSON directly. All arguments are the
    same as in {!run_github_query_async}. *)
val run_github_query
  :  ?methd:http_method
  -> ?params:(string * string list) list
  -> ?body:string
  -> string
  -> Yojson.Basic.t

(** Get details and reactions for a GitHub issue. The [col_name] parameter
    allows the column of the issue to be specified if needed (since we are
    fetching the issue directly, the information can't be obtained via the
    project tracker).

    To populate the column name of an issue which should have one but doesn't
    yet, use the {!populate_column_name} function.
    *)
val get_issue : ?col_name:string -> int -> issue

(** Find the column name of an issue (if one exists) and return the issue with
    an updated value in the [column] field.

    If the [project_issue_numbers] parameter is not passed, this function
    additionally searches the project for the given issue using
    {!get_project_issue_numbers}.
    *)
val populate_column_name : ?project_issue_numbers:(int * string) list -> issue -> issue

(** Get a list of all issue numbers in the project. Each issue number is paired
    with the name of the column. *)
val get_issue_numbers_in_column : rest_column -> (int * string) list

(** Get a list of all issue numbers in the project. The issue number is paired
    with the name of the column. *)
val get_project_issue_numbers : unit -> (int * string) list

(** Get a list of all issues in the project. *)
val get_project_issues : unit -> issue list
