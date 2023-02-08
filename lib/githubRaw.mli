(** GithubRaw queries the Github GraphQL API for data about issues, users, and
    project boards, and parses that into OCaml records. It throws exceptions if the data
    is too broken for even this parsing, but doesn't otherwise check for consistency or
    quality of what it receives. *)

(** Return the list of issues in a project board, given the name of the board. *)
val get_project_issues : string -> GithubTypes.issue list

(** Return all the users in the Alan Turing Institute Github organisation. *)
val get_users : unit -> GithubTypes.person list
