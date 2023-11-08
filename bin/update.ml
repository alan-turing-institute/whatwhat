(** Executable which updates whatwhat's version number and optionally prepares a
    new Homebrew release. *)

open Cmdliner
open Whatwhat.Pretty
open ANSITerminal

(** Information about how far away our current commit is from the last tag. *)
type git_commit =
  { _commits_since_tag : int
  ; _commit_hash : string
  }

(** Information about the current version of the codebase, as returned by
    `git describe --tags --dirty`. *)
type git_version =
  { major : int
  ; minor : int
  ; patch : int
  ; _commit : git_commit option
  ; dirty : bool
  }

(** Exit codes and error messages used by this executable.
    TODO: Document these in main_cmd *)
module ExitError = struct
  let not_in_repo =
    1, "Please run update_whatwhat from the top level of the whatwhat git repository."
  ;;

  let git_describe_failed = 2, "Could not parse git version"

  let dirty_working_dir =
    ( 3
    , "There are uncommitted changes or untracked files present in the working directory.\n\
       Please commit or remove these before running update_whatwhat." )
  ;;

  let git_commit_failed = 4, "Could not run git commit"
  let git_tag_failed = 5, "Could not run git tag"
  let git_push_failed = 6, "Could not run git push"

  let exit (code, msg) =
    prerr ~use_color:true [ Bold; Foreground Red ] "Error: ";
    prerr ~use_color:true [] (msg ^ "\n");
    exit code
  ;;
end

(** Parse the output of `git describe --tags --dirty` into a git_version. *)
let parse_git_version () =
  let git_describe = Unix.open_process_in "git describe --tags --dirty" |> input_line in
  let open Angstrom in
  let parse_int = take_while1 Whatwhat.Utils.is_digit >>| int_of_string in
  let parser =
    let+ _ = char 'v'
    and+ major = parse_int
    and+ _ = char '.'
    and+ minor = parse_int
    and+ _ = char '.'
    and+ patch = parse_int
    and+ _commit =
      option
        None
        (let+ _commits_since_tag = char '-' *> parse_int
         and+ _commit_hash = string "-g" *> take_while1 Whatwhat.Utils.is_hex_digit in
         Some { _commits_since_tag; _commit_hash })
    and+ dirty = option false (string "-dirty" *> return true) in
    { major; minor; patch; _commit; dirty }
  in
  match parse_string ~consume:All parser git_describe with
  | Ok version -> version
  | Error _ -> ExitError.exit ExitError.git_describe_failed
;;

(** Type of an option presented to the user for choosing. *)
type 'a cli_option =
  { user_input : string (* The string the user should type to select this option. *)
  ; value : 'a (* The value to return if the user selects this option. *)
  ; description : string (* A description of this option. *)
  }

(** Print a prompt text, then present the options to the user and ask for input.
    If default_option is given, it is used in case the user just presses Enter.
    Errors and exits if the user enters an invalid option. *)
let prompt_cli_options ?(default_option = None) options prompt_text =
  prout ~use_color:true [ Bold ] (prompt_text ^ "\n");
  let print_one_option { user_input; description; _ } =
    prout ~use_color:true [ Bold ] ("(" ^ user_input ^ ") ");
    prout ~use_color:true [] (description ^ "\n")
  in
  let all_options =
    match default_option with
    | None -> options
    | Some o -> o :: options
  in
  List.iter print_one_option all_options;
  let rec get_input () =
    let print_selection_prompt =
      match default_option with
      | None -> prout ~use_color:true [] " > "
      | Some o ->
        prout ~use_color:true [ Bold ] ("[default: " ^ o.user_input ^ "]");
        prout ~use_color:true [] " > "
    in
    print_selection_prompt;
    let input = read_line () |> String.trim in
    (* Check if it matches default option *)
    match input, default_option with
    | "", Some { value; _ } -> value
    | "", None -> get_input ()
    | other_input, _ ->
      (* Check if it matches other options. *)
      (match
         List.find_opt (fun { user_input; _ } -> user_input = other_input) all_options
       with
       | None ->
         prout ~use_color:true [ Foreground Red ] "Invalid option; please choose again.\n";
         get_input ()
       | Some { value; _ } -> value)
  in
  get_input ()
;;

(** Prompt the user for a new version number. *)
let get_new_version current_version =
  let default_option =
    { user_input = "1"
    ; value =
        { major = current_version.major
        ; minor = current_version.minor
        ; patch = current_version.patch + 1
        ; _commit = None
        ; dirty = false
        }
    ; description =
        Printf.sprintf
          "New patch: v%d.%d.%d"
          current_version.major
          current_version.minor
          (current_version.patch + 1)
    }
  in
  let options =
    [ { user_input = "2"
      ; value =
          { major = current_version.major
          ; minor = current_version.minor + 1
          ; patch = 0
          ; _commit = None
          ; dirty = false
          }
      ; description =
          Printf.sprintf
            "New patch: v%d.%d.0"
            current_version.major
            (current_version.minor + 1)
      }
    ; { user_input = "3"
      ; value =
          { major = current_version.major + 1
          ; minor = 0
          ; patch = 0
          ; _commit = None
          ; dirty = false
          }
      ; description = Printf.sprintf "New patch: v%d.0.0" (current_version.major + 1)
      }
    ]
  in
  prompt_cli_options
    ~default_option:(Some default_option)
    options
    "What kind of version bump do you want to perform?"
;;

let update_version_number_in_dune_project current_version new_version =
  let file_name = "dune-project" in
  let file_contents = In_channel.with_open_text file_name In_channel.input_all in
  let current_version_regexp =
    Str.regexp
    @@ Printf.sprintf
         "(version %d.%d.%d)"
         current_version.major
         current_version.minor
         current_version.patch
  in
  let new_version_string =
    Printf.sprintf
      "(version %d.%d.%d)"
      new_version.major
      new_version.minor
      new_version.patch
  in
  let new_file_contents =
    Str.global_replace current_version_regexp new_version_string file_contents
  in
  Out_channel.with_open_text file_name (fun t ->
    Out_channel.output_string t new_file_contents)
;;

(* COMMAND-LINE ARGUMENTS *)

let ignore_dirty_arg =
  Arg.(
    value
    & flag
    & info
        [ "D"; "ignore-dirty" ]
        ~doc:
          "Don't check if the working directory is dirty (i.e. if it contains \
           uncommitted changes.)")
;;

(* ENTRY POINT *)

let main ignore_dirty =
  (* Detect current working directory and make sure it looks like the top level
     of the repo. A bit hacky but this should suffice. Also check for .git
     because we do need to perform git operations. *)
  let dir_contents = Sys.getcwd () |> Sys.readdir |> Array.to_list in
  (if (not (List.mem "dune-project" dir_contents))
      || (not (List.mem ".git" dir_contents))
      || not (List.mem "whatwhat.opam" dir_contents)
   then ExitError.(exit not_in_repo));

  (* Detect and show current version. *)
  let current_version = parse_git_version () in
  (if current_version.dirty && not ignore_dirty then ExitError.(exit dirty_working_dir));
  prout
    ~use_color:true
    [ Bold ]
    (Printf.sprintf
       "Detected current version of whatwhat: %d.%d.%d\n"
       current_version.major
       current_version.minor
       current_version.patch);
  print_endline "";

  (* Prompt user for the new version. *)
  let new_version = get_new_version current_version in
  prout
    ~use_color:true
    [ Bold ]
    (Printf.sprintf
       "Updating whatwhat to version: %d.%d.%d\n"
       new_version.major
       new_version.minor
       new_version.patch);
  print_endline "";

  (* Replace the version number in the dune-project file. *)
  prout ~use_color:true [ Bold ] "Updating version number in dune-project file...\n";
  update_version_number_in_dune_project current_version new_version;
  print_endline "";

  (* TODO Check if we are on the main branch and if there are any merge
     conflicts with upstream. *)

  (* Commit changes *)
  prout ~use_color:true [ Bold ] "Committing changes in git...\n";
  let git_commit_exit_code =
    Unix.system
      (Printf.sprintf
         "git commit -am 'Update version number to %d.%d.%d'"
         new_version.major
         new_version.minor
         new_version.patch)
  in
  (if git_commit_exit_code <> Unix.WEXITED 0 then ExitError.(exit git_commit_failed));
  print_endline "";

  (* Add git tag *)
  prout ~use_color:true [ Bold ] "Adding git tag...\n";
  let git_tag_exit_code =
    Unix.system
      (Printf.sprintf
         "git tag -a v%d.%d.%d -m 'Version %d.%d.%d'"
         new_version.major
         new_version.minor
         new_version.patch
         new_version.major
         new_version.minor
         new_version.patch)
  in
  (if git_tag_exit_code <> Unix.WEXITED 0 then ExitError.(exit git_tag_failed));
  print_endline "";

  (* TODO Push to GitHub with git cli *)
  let push_now =
    prompt_cli_options
      ~default_option:(Some { user_input = "y"; value = false; description = "Yes" })
      [ { user_input = "n"; value = true; description = "No" } ]
      "Do you want to push this tag to GitHub?"
  in
  if push_now
  then (
    let git_push_exit_code = Unix.system "git push" in
    if git_push_exit_code <> Unix.WEXITED 0 then ExitError.(exit git_push_failed))
  else
    prout
      ~use_color:true
      [ Foreground Green; Bold ]
      "update_whatwhat exiting.\n\
       To resume from this point, use `update_whatwhat --step=4`\n";

  (* TODO Edit contents of homebrew-hut23/whatwhat.rb with GitHub API *)

  (* TODO Create Homebrew bottle *)

  (* TODO Create new release with GitHub API *)

  (* TODO Upload Homebrew bottle to release *)

  (* TODO Edit contents of homebrew-hut23/whatwhat.rb (again) to include bottle *)
  prout ~use_color:true [ Foreground Green; Bold ] "Success!\n"
;;

let main_cmd =
  Cmd.v
    (Cmd.info "update_whatwhat" ~doc:"Prepare and release a new version of whatwhat.")
    Term.(const main $ ignore_dirty_arg)
;;

let () = exit (Cmd.eval main_cmd)
