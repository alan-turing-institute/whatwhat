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

  let wrong_branch name =
    4, "Please checkout the '" ^ name ^ "' branch before running update_whatwhat."
  ;;

  let remote_does_not_exist name = 5, "The remote '" ^ name ^ "' does not exist."
  let git_commit_failed = 6, "Could not run git commit"
  let git_tag_failed = 7, "Could not run git tag"
  let git_push_failed = 8, "Could not run git push. Are you connected to the Internet?"
  let git_fetch_failed = 9, "Could not run git fetch. Are you connected to the Internet?"
  let git_merge_abort_failed = 10, "Could not abort git merge"

  let merge_conflicts_present =
    ( 11
    , "Encountered merge conflicts when attempting to automatically merge upstream \
       branch. Please manually run `git merge` before rerunning `update_whatwhat`." )
  ;;

  let homebrew_formula_update_failed s =
    12, "Failed to update Homebrew formula on GitHub. Error details: " ^ s
  ;;

  let homebrew_bottle_failed = 13, "Failed to create Homebrew bottle"

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

(** Replace the version number in the dune-project file. *)
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

(** Run a command and ensure that it succeeds. *)
let run_command exit_error command =
  let exit_code = Unix.system command in
  if exit_code <> Unix.WEXITED 0 then ExitError.exit exit_error
;;

(** Check for merge conflicts with remote main branch. *)
let check_merge_conflicts remote_name branch_name =
  run_command
    ExitError.git_fetch_failed
    (String.concat " " [ "git"; "fetch"; remote_name; branch_name ]);
  (* This bit is a bit more subtle, as we need to try to merge, but also abort
     the merge before exiting. *)
  let merge_exit_code =
    Unix.system (Printf.sprintf "git merge %s/%s --no-edit" remote_name branch_name)
  in
  if merge_exit_code <> Unix.WEXITED 0
  then (
    run_command ExitError.git_merge_abort_failed "git merge --abort";
    ExitError.(exit merge_conflicts_present))
;;

(** Update the 'tag' and 'revision' items in the whatwhat.rb formula in the
    GitHub alan-turing-institute/homebrew-hut23 repository. This enables
    Homebrew to build the new version when instructed to build from source.

    The GitHub API is used here; you must have the GitHub bot token in your
    whatwhat config file. It assumes that the bot (i.e. the NowWhatBot GitHub
    account) has push permissions for the repository (which it does).

    NOTE: This function will throw exceptions if something fails. *)
let update_homebrew_hut23_formula current_version new_version =
  (* Fetch the file contents from GitHub *)
  let gh_get_file_resp =
    Whatwhat.GithubRaw.run_github_query
      ~as_bot:true
      ~http_method:GET
      ~accept:Json
      "https://api.github.com/repos/yongrenjie/homebrew-hut23/contents/whatwhat.rb"
  in
  let current_rb_contents =
    gh_get_file_resp
    |> Yojson.Basic.Util.member "content"
    |> Yojson.Basic.Util.to_string
    |> Str.global_replace (Str.regexp "\n") ""
    |> Base64.decode_exn
  in
  let current_sha =
    gh_get_file_resp |> Yojson.Basic.Util.member "sha" |> Yojson.Basic.Util.to_string
  in
  (* Update the file contents *)
  let current_git_commit = Unix.open_process_in "git rev-parse HEAD" |> input_line in
  let old_commit_regexp = Str.regexp "revision: \"\\([0-9a-f]+\\)\"" in
  let new_commit_string = "revision: \"" ^ current_git_commit ^ "\"" in
  let current_version_regexp =
    Str.regexp
    @@ Printf.sprintf
         "tag: \"v%d.%d.%d\""
         current_version.major
         current_version.minor
         current_version.patch
  in
  let new_version_string =
    Printf.sprintf
      "tag: \"v%d.%d.%d\""
      new_version.major
      new_version.minor
      new_version.patch
  in
  let new_text =
    current_rb_contents
    |> Str.replace_first current_version_regexp new_version_string
    |> Str.replace_first old_commit_regexp new_commit_string
  in
  (* Send the updated file contents back to GitHub *)
  let body_string =
    Yojson.Basic.to_string
      (`Assoc
        [ ( "message"
          , `String
              (Printf.sprintf
                 "Update Git revision for whatwhat.rb to v%d.%d.%d\n\n\
                  Co-authored-by: %s <%s>"
                 new_version.major
                 new_version.minor
                 new_version.patch
                 (Unix.open_process_in "git config --get user.name" |> input_line)
                 (Unix.open_process_in "git config --get user.email" |> input_line)) )
        ; ( "committer"
          , `Assoc
              [ "name", `String "NowWhatBot"
              ; "email", `String "hut23-1206-nowwhat@turing.ac.uk"
              ] )
        ; "content", `String (Base64.encode_exn new_text)
        ; "sha", `String current_sha
        ])
  in
  ignore
  @@ Whatwhat.GithubRaw.run_github_query
       ~as_bot:true
       ~http_method:PUT
       ~accept:Raw
       ~body:body_string
       "https://api.github.com/repos/yongrenjie/homebrew-hut23/contents/whatwhat.rb"
;;

(* COMMAND-LINE ARGUMENTS *)

let branch_name_arg =
  Arg.(
    value
    & opt string "main"
    & info
        [ "b"; "branch-name" ]
        ~doc:"Name of the Git branch you are on. Defaults to 'main'."
        ~docv:"BRANCH_NAME")
;;

let remote_name_arg =
  Arg.(
    value
    & opt string "origin"
    & info
        [ "r"; "remote-name" ]
        ~doc:
          "Name of the Git remote to push to. Defaults to 'origin'. Assumes that a \
           branch with the same name as $(b,branch_name) exists on the remote."
        ~docv:"REMOTE_NAME")
;;

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

let main branch_name remote_name ignore_dirty =
  let announce s = prout ~use_color:true [ Bold ] (s ^ "\n\n") in

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
  announce
    (Printf.sprintf
       "Detected current version of whatwhat: %d.%d.%d"
       current_version.major
       current_version.minor
       current_version.patch);

  (* Prompt user for the new version. *)
  let new_version = get_new_version current_version in
  announce
    (Printf.sprintf
       "Updating whatwhat to version: %d.%d.%d"
       new_version.major
       new_version.minor
       new_version.patch);

  (* Check if we are on the main branch *)
  let current_git_branch =
    Unix.open_process_in "git rev-parse --abbrev-ref HEAD" |> input_line
  in
  announce ("Checking if we are on the '" ^ branch_name ^ "' branch...");
  (if current_git_branch <> branch_name then ExitError.(exit @@ wrong_branch branch_name));

  (* Check if the given remote name exists *)
  announce ("Checking if the remote '" ^ remote_name ^ "' exists...");
  let remote_names =
    Unix.open_process_in "git remote"
    |> In_channel.input_all
    |> String.split_on_char '\n'
    |> List.map String.trim
  in
  (if not (List.mem remote_name remote_names)
   then ExitError.(exit @@ remote_does_not_exist remote_name));

  (* Check if there are any merge conflicts with upstream *)
  announce "Checking if there are any changes on the remote...";
  check_merge_conflicts remote_name branch_name;

  (* TODO: If anything after this point fails, it has to be undone manually. It
     would be nice to have a way to undo everything automatically. *)

  (* Replace the version number in the dune-project file. *)
  prout ~use_color:true [ Bold ] "Updating version number in dune-project file...\n";
  update_version_number_in_dune_project current_version new_version;
  print_endline "";

  (* Commit changes *)
  announce "Committing changes in git...";
  run_command
    ExitError.git_commit_failed
    (Printf.sprintf
       "git commit -am 'Update version number to %d.%d.%d'"
       new_version.major
       new_version.minor
       new_version.patch);
  print_endline "";

  (* Add git tag *)
  announce "Adding git tag...";
  run_command
    ExitError.git_tag_failed
    (Printf.sprintf
       "git tag -a v%d.%d.%d -m 'Version %d.%d.%d'"
       new_version.major
       new_version.minor
       new_version.patch
       new_version.major
       new_version.minor
       new_version.patch);
  print_endline "";

  (* Push to GitHub *)
  let push_now =
    prompt_cli_options
      ~default_option:(Some { user_input = "y"; value = false; description = "Yes" })
      [ { user_input = "n"; value = true; description = "No" } ]
      "Do you want to push this tag to GitHub now?"
  in
  if push_now
  then
    run_command
      ExitError.git_push_failed
      (Printf.sprintf
         "git push %s %s:%s && git push %s --tags"
         remote_name
         branch_name
         branch_name
         remote_name)
  else prout ~use_color:true [ Foreground Green; Bold ] "update_whatwhat exiting.\n";

  (* Edit contents of homebrew-hut23/whatwhat.rb with GitHub API *)
  announce "Updating homebrew-hut23/whatwhat.rb...";
  (try update_homebrew_hut23_formula current_version new_version with
   | e -> ExitError.(exit @@ homebrew_formula_update_failed (Printexc.to_string e)));

  (* Create Homebrew bottle *)
  announce "Creating Homebrew bottle on local machine...";
  run_command ExitError.homebrew_bottle_failed "brew update";
  run_command ExitError.homebrew_bottle_failed "brew uninstall whatwhat || true";
  run_command ExitError.homebrew_bottle_failed "brew tap yongrenjie/hut23";
  run_command ExitError.homebrew_bottle_failed "brew install --build-bottle --verbose whatwhat";
  let bottle_do_block = Unix.open_process_in "brew bottle whatwhat --no-rebuild" |> In_channel.input_all in
  print_endline bottle_do_block;

  (* TODO Create new release with GitHub API *)

  (* TODO Upload Homebrew bottle to release *)

  (* TODO Edit contents of homebrew-hut23/whatwhat.rb (again) to include bottle *)
  prout ~use_color:true [ Foreground Green; Bold ] "Success!\n"
;;

let main_cmd =
  Cmd.v
    (Cmd.info "update_whatwhat" ~doc:"Prepare and release a new version of whatwhat.")
    Term.(const main $ branch_name_arg $ remote_name_arg $ ignore_dirty_arg)
;;

let () = if false then exit (Cmd.eval main_cmd)
