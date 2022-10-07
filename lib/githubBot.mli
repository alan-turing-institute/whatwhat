(** Utilities for posting comments to GitHub issues *)


val github_post : string -> int -> string -> string
(** [github_post repo issue comment] posts [comment] to issue [issue] in repository [repo] *)
