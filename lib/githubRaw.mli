(** [GithubRaw] queries the Github GraphQL and REST APIs for data about issues,
    users, and project boards, and parses that into OCaml records.
    *)

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

(** The state of an issue on GitHub. *)
type issue_state =
  | Open
  | Closed
[@@deriving show]

(** An [issue] is a GitHub issue on the repository being queried.
    *)
type issue =
  { number : int
  ; title : string
  ; body : string
  ; state : issue_state
  ; assignees : person list
  ; reactions : (string * person) list
  ; labels : string list
  }
[@@deriving show]

(** A column of a project on GitHub. This data type is used for data from the
    REST API. *)
type column =
  { name : string
  ; id : int
  ; issues : issue list (* The [string] here is the column name. *)
  }
[@@deriving show]

(* TODO: Document *)
type project =
  { id : int
  ; name : string
  ; columns : column list
  }
[@@deriving show]

(** This is a list of all users who can be assigned to issues in the
    repository, which essentially means anybody who can view the repository. *)
val all_users : person list

(** Run a GitHub query, returning a promise for the body JSON. The [body]
    parameter here is the request body, which is used only for POST requests.
    The final mandatory [string] parameter is the URI.
    *)
val run_github_query_async
  :  ?http_method:http_method
  -> ?params:(string * string list) list
  -> ?body:string
  -> ?failure_msg:string
  -> string
  -> Yojson.Basic.t Lwt.t

(** Run a GitHub query, returning the body JSON directly. All arguments are the
    same as in {!run_github_query_async}. *)
val run_github_query
  :  ?http_method:http_method
  -> ?params:(string * string list) list
  -> ?body:string
  -> ?failure_msg:string
  -> string
  -> Yojson.Basic.t

(** Get details and reactions for a GitHub issue.
    *)
val get_issue : int -> issue

val get_project : unit -> project
