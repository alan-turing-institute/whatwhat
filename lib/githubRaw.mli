(** [GithubRaw] queries the GitHub GraphQL and REST APIs for data about issues,
    users, and project boards, and parses that into OCaml records.
    *)

(** {1 HTTP requests}
    *)

(** HTTP methods. Only [GET] and [POST] are supported within [whatwhat]. *)
type http_method =
  | GET
  | POST

(** Run a GitHub query, returning a promise for the body JSON.

    - [params] are request header parameters.
    - [body] is the request body (used only for [POST] requests).
    - The mandatory argument is the URI.
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

(** {1 People}
    *)

(** A [person] is a GitHub user: they are identified by their login username, a
    real name, and an email. The latter two are obtained from their public
    profile (not their commits) and may be absent.

    In general, we would like to have an external lookup of GitHub usernames to
    real names as this would allow for foolproof matching of Forecast and GitHub
    accounts. See
    {{: https://github.com/alan-turing-institute/whatwhat/issues/70 }#70}.
    *)
type person =
  { login : string
  ; name : string option
  ; email : string option
  }

(** [all_users] is a list of all users who can be assigned to issues in the
    repository, which essentially means anybody who can view the repository. *)
val all_users : person list

(** {1 Issues}

    Each issue on the GitHub project tracker nominally refers to a project
    within REG.
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

(** A GitHub issue, but with reactions. We have a separate type for this because
    fetching reactions is a separate request to the GitHub API, and we don't
    want to indiscriminately do this. *)
type issue_r =
  { issue : issue
  ; reactions : (string * person) list
  }

(** Fetch a numbered issue from GitHub. *)
val get_issue : int -> issue

(** Fetch a numbered issue from GitHub, together with its reactions. *)
val get_issue_r : int -> issue_r

(** {1 Columns}

    A [column] refers to a column within a project board on GitHub.
    
    There are several forms of columns, in order to deal with the fact that
    different parts of [whatwhat] require different requirements for data. There
    are at least three possibilities:
    
    + Only the numbers of issues in a column are required. (For example, to
      determine whether an issue is in a column.)
    + The issues themselves are needed, but not the reactions. (This is the most
      common.)
    + All issues and reactions are needed. (This is required if we want to
      collate all reactions from a specific person.)

    In order to ensure that functions can be called in a type-safe manner, these
    three classes of columns are respectively represented by the [column_n]
    ('numbers'), [column], and [column_r] ('reactions') record types.
    *)

type column_n =
  { name : string
  ; id : int
  ; issues_n : int list
  }

type column =
  { name : string
  ; id : int
  ; issues : issue list (* The [string] here is the column name. *)
  }

type column_r =
  { name : string
  ; id : int
  ; issues_r : issue_r list
  }

(** {1 Projects}

    A [project] refers to a project board on GitHub, which contains a list of
    columns. For similar reasons as described for columns, there are three
    record types: [project_n], [project], and [project_r], each with the same
    meaning as above.
    *)

type project_n =
  { id : int
  ; name : string
  ; columns_n : column_n list
  }

type project =
  { id : int
  ; name : string
  ; columns : column list
  }

type project_r =
  { id : int
  ; name : string
  ; columns_r : column_r list
  }

(** The following three functions retrieve entire [projects] from GitHub with
    the required level of detail. The exact project being fetched is determined
    by the variables in the [whatwhat] configuration file. *)

val get_project_n : unit -> project_n
val get_project : unit -> project
val get_project_r : unit -> project_r
