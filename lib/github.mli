(** High-level interface to the Project Tracker in Github.

    GitHub is used to record the overall status of each project. Each project is
    represented as an issue and indeed the GitHub issue number (sometimes
    referred to as a "Hut23 code") is used as a unique identifier for projects,
    including those on Forecast.

    A GitHub issue consists of a title, a body (the main content of the issue),
    some metadata (such as the people assigned to the issue and the "column" to
    which the issue belongs) and then a list of additional comments.

    We use the body of the issue to hold project-level metadata encoded as a
    YAML block (described below). The additional comments are not returned by
    this module.
    
    {2 Validation and reporting}

    Errors are logged to the console when metadata is malformed, and the issue
    is dropped from the list of all issues.

    Errors currently occur when:
      - Metadata does not have 8 keys.
      - Cannot break a metadata line into a (key,value) pair, which means it is
        incorrect yaml.
      - A crucial key cannot be parsed (missing, or there is an error when dealing
        with the value). Currently all keys are marked as non-crucial, but this may
        change in the future.

    Warnings are given for other inconsisties (e.g. no one is
    assigned). Warnings are given due to:
      - There being additional information in the value entry.
      - A non-crucial entry is missing or null.

 *)

(** Errors and warnings for logging problems with the issue metadata. *)
type parseerror =
  | DateOutOfBoundsError
  | DateParsingError
  | ExtraFieldError
  | FieldTypeError
  | FTETimeUnderSpecifiedError
  | FTETimeOverSpecifiedError
  | MissingCompulsoryFieldError
  | MissingOptionalFieldError
  | NoMetadataError
  (* | NullCompulsoryFieldError *)
  | YamlError

(* We reexport the Raw.person type so that no other module ever has a need to import
   anything from GithubRaw. *)

type person = GithubRaw.person =
  { login : string
  ; name : string option
  ; email : string option
  }
val show_person : person -> string
(** A type for Github users. *)

(** Given a project board name, return a list of projects, one for each issue on the
    board. *)
val get_project_issues : string -> Domain.project list

(** Return all the users in the Alan Turing Institute Github organisation. *)
val all_hut23_users : person list
