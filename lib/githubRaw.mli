(** [GithubRaw] queries the Github GraphQL and REST APIs for data about issues,
    users, and project boards, and parses that into OCaml records.
    *)

(** {1 HTTP requests}
    *)

(** HTTP methods. Only GET and POST are supported. *)
type http_method =
  | GET
  | POST

(** {1 People}
    *)

(** A [person] is a GitHub user: they are identified by their login username, a
    real name, and an email. The latter two are obtained from their public
    profile (not their commits) and may be absent. *)
type person =
  { login : string
  ; name : string option
  ; email : string option
  }
[@@deriving show]

(** {1 Issues}
    *)

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
  ; labels : string list
  }
[@@deriving show]

(** A GitHub issue, but with reactions. We have a separate type for this because
    fetching reactions is a separate request to the GitHub API, and we don't
    want to indiscriminately do this. *)
type issue_with_reactions =
  { issue : issue
  ; reactions : (string * person) list
  }

val get_issue_with_reactions : int -> issue_with_reactions

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
  -> string
  -> Yojson.Basic.t Lwt.t

(** Run a GitHub query, returning the body JSON directly. All arguments are the
    same as in {!run_github_query_async}. *)
val run_github_query
  :  ?http_method:http_method
  -> ?params:(string * string list) list
  -> ?body:string
  -> string
  -> Yojson.Basic.t

(** Get details and reactions for a GitHub issue.
    *)
val get_issue : int -> issue

val get_project : unit -> project

type column_numeric =
  { name : string
  ; id : int
  ; issue_numbers : int list
  }

type project_numeric =
  { id : int
  ; name : string
  ; columns_num : column_numeric list
  }

(** The same as a column, but issues additionally contain reactions.
    *)
type column_reactions =
  { name : string
  ; id : int
  ; issue_reactions : issue_with_reactions list
  }

(** The same as a project, but issues additionally contain reactions. *)
type project_reactions =
  { id : int
  ; name : string
  ; columns_rxn : column_reactions list
  }

val get_project_num : unit -> project_numeric
val get_project_reactions : unit -> project_reactions
