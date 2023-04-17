(** Pretty-print a person. *)

(** Pretty-print a person. The following details are shown:

    - Their full name, email, and GitHub username
    - Current assignments
    - Emoji reactions on all GitHub issues
    *)
val print : use_color:bool -> Domain.person -> Domain.Assignment.t list -> unit
