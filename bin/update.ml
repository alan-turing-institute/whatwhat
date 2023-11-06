module ExitError = struct
  let exit (code, msg) =
    let open Whatwhat.Pretty in
    let open ANSITerminal in
    prerr ~use_color:true [ Bold; Foreground Red ] "Error: ";
    prerr ~use_color:true [] (msg ^ "\n");
    exit code
  ;;

  let git_describe_failed = 1, "Could not parse git version"

  let dirty_working_dir =
    ( 2
    , "There are uncommitted changes or untracked files present in the working directory.\n\
       Please commit or remove these before running update_whatwhat." )
  ;;
end

(** Information about how far away our current commit is from the last tag. *)
type git_commit =
  { commits_since_tag : int
  ; commit_hash : string
  }
[@@deriving show]

(** Information about the current version of the codebase, as returned by
    git describe --tags --dirty. *)
type git_version =
  { major : int
  ; minor : int
  ; patch : int
  ; commit : git_commit option
  ; dirty : bool
  }
[@@deriving show]

(** Parse the output of git describe --tags --dirty into a git_version. *)
let parse_version_from_git () =
  let git_describe = Unix.open_process_in "git describe --tags --dirty" |> input_line in
  let git_version_rgx =
    Tyre.compile
      Tyre.(
        str "v" *> pos_int
        <&> str "." *> pos_int
        <&> str "." *> pos_int
        <&> opt
              (str "-" *> pos_int
               <&> str "-g" *> regex (Re.rep1 (Re.set "0123456789abcdef")))
        <&> opt (str "-dirty"))
  in
  match Tyre.exec git_version_rgx git_describe with
  | Error _ -> ExitError.(exit git_describe_failed)
  | Ok ((((major, minor), patch), commit_info), maybe_dirty) ->
    let commit =
      match commit_info with
      | Some (a, b) -> Some { commits_since_tag = a; commit_hash = b }
      | None -> None
    in
    { major; minor; patch; commit; dirty = Option.is_some maybe_dirty }
;;

let () =
  let version = parse_version_from_git () in
  (if version.dirty then ExitError.(exit dirty_working_dir));
  version |> show_git_version |> print_endline
;;
