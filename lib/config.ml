(* We distinguish between secrets and configs, because we want to keep them in two
   separate files. They are otherwise treated identically though, and both are collected
   into the same record type `t`. *)
type t =
  { github_project_name : string option
  ; github_project_columns : string list option
  ; github_repo_name : string option
  ; github_repo_owner : string option
  ; github_token : string option
  ; githubbot_token : string option
  ; forecast_id : string option
  ; forecast_token : string option
  ; slack_bot_token : string option
  ; slack_app_token : string option
  }

let secrets_path = XDGBaseDir.default.config_home ^ "/whatwhat/secrets.json"
let config_path = XDGBaseDir.default.config_home ^ "/whatwhat/config.json"
let users_path = XDGBaseDir.default.config_home ^ "/whatwhat/users"
let ( >>= ) = Option.bind

type config_event =
  | EmptySecret of string
  | MissingSecret of string
  | MissingConfig of string
  | MissingSecretsFile
  | MissingConfigFile

let log_event (cfg_event : config_event) : unit =
  match cfg_event with
  | MissingSecretsFile ->
    Log.log'
      { level = Log.Error' 1
      ; entity = Log.Other
      ; message = "The secrets file could not be found in " ^ secrets_path ^ "."
      }
  | MissingConfigFile ->
    Log.log'
      { level = Log.Error' 2
      ; entity = Log.Other
      ; message = "The config file could not be found in " ^ config_path ^ "."
      }
  | MissingSecret secret ->
    Log.log'
      { level = Log.Error' 3
      ; entity = Log.Other
      ; message =
          "The secret '" ^ secret ^ "' could not be found in " ^ secrets_path ^ "."
      }
  | MissingConfig config ->
    Log.log'
      { level = Log.Error' 4
      ; entity = Log.Other
      ; message = "The config '" ^ config ^ "' could not be found in " ^ config_path ^ "."
      }
  | EmptySecret secret ->
    Log.log'
      { level = Log.Error' 5
      ; entity = Log.Other
      ; message =
          "An empty value was found for the secret '"
          ^ secret
          ^ "' in "
          ^ secrets_path
          ^ ". Please update it."
      }
;;

(* Constants which are going to be the same on every run *)
let github_url = "https://api.github.com"
let forecast_url = "https://api.forecastapp.com"

(* 'Time off' Forecast project which doesn't have a client *)
let forecast_ignored_projects = [ 1684536 ]

(* List of issue numbers for which it's okay to have duplicate projects in Forecast *)
let forecast_duplicates_okay = []

(* These JSON parsers are more lenient than the ones in Yojson.Basic.Util: They return
   [None] if the JSON is of the wrong type, rather than erroring. *)
let string_opt_of_json = function
  | `String value -> Some value
  | _ -> None
;;

let int_opt_of_json = function
  | `Int value -> Some value
  | _ -> None
;;

let string_list_opt_of_json = function
  | `List value ->
    let string_opts = Yojson.Basic.Util.convert_each string_opt_of_json (`List value) in
    (* Fold a list of options to an option of list. *)
    let folder x y =
      match x, y with
      | Some acc, Some value -> Some (value :: acc)
      | _ -> None
    in
    List.fold_left folder (Some []) string_opts
  | _ -> None
;;

let int_list_opt_of_json = function
  | `List value ->
    let int_opts = Yojson.Basic.Util.convert_each int_opt_of_json (`List value) in
    (* Fold a list of options to an option of list. *)
    let folder x y =
      match x, y with
      | Some acc, Some value -> Some (value :: acc)
      | _ -> None
    in
    List.fold_left folder (Some []) int_opts
  | _ -> None
;;

(* Find the value for a secret/config called [key].

   If the environment variable called [String.uppercase_ascii ("WHATWHAT_" ^ key)] exists,
   return its value. If it doesn't, return the corresponding key from the JSON object
   provided. If that doesn't exist either, return [None].

   Both the environment variable and the file value are read as JSON, and parsed using the
   function [json_parser], which takes JSON and returns an [`a option]. *)
let find_setting json_parser key file_json_opt =
  let file_value =
    file_json_opt |> Option.map (Yojson.Basic.Util.member key) >>= json_parser
  in
  let envvar_key = String.uppercase_ascii ("WHATWHAT_" ^ key) in
  let envvar_value =
    Sys.getenv_opt envvar_key |> Option.map Yojson.Basic.from_string >>= json_parser
  in
  match file_value, envvar_value with
  | _, Some value -> Some value
  | Some value, None -> Some value
  | None, None -> None
;;

let attempt_file (path : string) (message : string) (update_message : bool) =
  (* get the directory name *)
  let dir =
    String.split_on_char '/' path |> List.rev |> List.tl |> List.rev |> String.concat "/"
  in
  (* strip all before characters before last/ from string *)
  let file_type = String.split_on_char '/' path |> List.rev |> List.hd in

  try
    (* File exists *)
    let _ = open_in path in
    Pretty.prout ~use_color:true [ Bold; Foreground Yellow ] "Information: ";
    Printf.printf
      "The %s file exists. If you want to make any changes, please update the file \
       yourself in %s.\n"
      file_type
      path
  with
  | Sys_error _ ->
    (* File doesn't exist. Create directory first *)
    let _ = Unix.system ("mkdir -p " ^ dir) in
    (* Populate file with message *)
    let oc = open_out path in
    Printf.fprintf oc "%s" message;
    close_out oc;
    (* Notify *)
    Printf.printf "I have written a %s file to %s.\n" file_type path;
    if update_message
    then
      print_endline
        "Make sure you update the tokens there following the instructions in the comments.\n"
;;

let default_config_contents : string =
  {|{
  "githubRepoOwner": "alan-turing-institute",
  "githubRepoName": "Hut23",
  "githubProjectName": "Project Tracker",
  "githubProjectColumns": [
    "Active",
    "Awaiting start",
    "Finding people",
    "Awaiting go/no-go"
  ],
  "forecastId": "974183"
}|}
;;

let default_secrets_contents : string =
  {|{
    // githubToken (required)
    // Used for reading info from GitHub project boards, issues, etc. This can
    // be generated at https://github.com/settings/tokens. The token will need
    // to have the permissions: read:user, repo, and user:email.
    "githubToken": "",

    // forecastToken (required)
    // Used to fetch project allocations from Forecast. Get your own token at
    // https://id.getharvest.com/oauth2/access_tokens/new.
    "forecastToken": "",

    // githubBotToken (optional, leave blank if unused)
    // Used to post to GitHub from whatwhat, primarily for whatwhat admins).
    // Leave blank if unneeded. To get this, you need to be added to the
    // hut23-1206-nowwhat@turing.ac.uk group (ask someone else on the whatwhat
    // developer team to add you, e.g. the person who most recently committed to
    // main).
    "githubBotToken": "",

    // slackBotToken (optional, leave blank if unused)
    // Ostensibly used to run a whatwhat Slack bot. However, this hasn't yet
    // been launched on the main Turing Slack workspace, so this token is
    // currently unused. Note that this is a bot token, not an app token: it
    // should start with "xoxb-".
    "slackBotToken": "",

    // slackAppToken (optional, leave blank if unused)
    // Ostensibly used to annoy people on Slack by pinging them about projects.
    // As with the above, it hasn't been launched on the main Turing Slack
    // workspace, so this token is currently unused. Note that this is a an app
    // token, and should start with * "xapp-". There is an existing token that
    // can also be obtained from the hut23-1206-nowwhat@turing.ac.uk group.
    "slackAppToken": ""
}|}
;;

let load_settings () : t =
  try
    let secrets_required_setup, secrets_json_opt =
      try false, Some (Yojson.Basic.from_file secrets_path) with
      | Sys_error _ ->
        (* Ask user if its ok to set up new file *)
        Pretty.prout ~use_color:true [ Bold; Foreground Green ] "Attention!: ";
        print_string
          ("The file "
           ^ secrets_path
           ^ " was not found.\n\
              Are you happy for me to set up a secrets template for you? (yes/no): ");
        let answer = read_line () in

        (* If answer starts with Y or y *)
        if String.lowercase_ascii (String.sub answer 0 1) = "y"
        then (
          (* run attempt_file on the secrets_path *)
          let _ = attempt_file secrets_path default_secrets_contents true in

          true, Some (Yojson.Basic.from_file secrets_path) (* If no, exit *))
        else (
          Pretty.prout ~use_color:true [ Bold; Foreground Red ] "Exiting...\n";
          exit 0)
    in
    let _config_required_setup, config_json_opt =
      try false, Some (Yojson.Basic.from_file config_path) with
      | Sys_error _ ->
        (* Ask user if its ok to set up new file *)
        print_string
          ("The file "
           ^ config_path
           ^ " was not found.\n\
              Are you happy for me to set up a config template for you? (yes/no): ");
        let answer = read_line () in
        (* If answer starts with Y or y *)
        if String.lowercase_ascii (String.sub answer 0 1) = "y"
        then (
          (* run attempt_file on the config_path *)
          let _ = attempt_file config_path default_config_contents false in
          true, Some (Yojson.Basic.from_file config_path) (*  If no, exit *))
        else (
          Pretty.prout ~use_color:true [ Bold; Foreground Red ] "Exiting...\n";
          exit 0)
    in
    (* Don't continue if the secrets file was found to be missing, it'll just
       give an error anyway, because the tokens are not present. *)
    if secrets_required_setup then exit 0;
    (* Otherwise parse them and continue *)
    { github_project_name =
        find_setting string_opt_of_json "githubProjectName" config_json_opt
    ; github_repo_name = find_setting string_opt_of_json "githubRepoName" config_json_opt
    ; github_repo_owner =
        find_setting string_opt_of_json "githubRepoOwner" config_json_opt
    ; github_token = find_setting string_opt_of_json "githubToken" secrets_json_opt
    ; githubbot_token = find_setting string_opt_of_json "githubBotToken" secrets_json_opt
    ; github_project_columns =
        find_setting string_list_opt_of_json "githubProjectColumns" config_json_opt
    ; forecast_id = find_setting string_opt_of_json "forecastId" config_json_opt
    ; forecast_token = find_setting string_opt_of_json "forecastToken" secrets_json_opt
    ; slack_bot_token = find_setting string_opt_of_json "slackBotToken" secrets_json_opt
    ; slack_app_token = find_setting string_opt_of_json "slackAppToken" secrets_json_opt
    }
  with
  | _ -> exit 0
;;

let settings = load_settings ()

let get_github_project_name () =
  (* let* settings = load_settings () in *)
  match settings.github_project_name with
  | Some value -> value
  | None ->
    log_event (MissingConfig "githubProjectName");
    failwith "MissingConfig"
;;

let get_github_project_columns () =
  (* let* settings = load_settings () in *)
  settings.github_project_columns
;;

let get_github_repo_name () =
  (* let* settings = load_settings () in *)
  match settings.github_repo_name with
  | Some value -> value
  | None ->
    log_event (MissingConfig "githubRepoName");
    failwith "MissingConfig"
;;

let get_github_repo_owner () =
  (* let* settings = load_settings () in *)
  match settings.github_repo_owner with
  | Some value -> value
  | None ->
    log_event (MissingConfig "githubRepoOwner");
    failwith "MissingConfig"
;;

let get_github_token () =
  (* let* settings = load_settings () in *)
  match settings.github_token with
  | Some "" ->
    log_event (EmptySecret "githubToken");
    ""
  | Some nonEmptyValue -> nonEmptyValue
  | None ->
    log_event (MissingSecret "githubToken");
    failwith "MissingSecret"
;;

let get_githubbot_token () =
  (* let* settings = load_settings () in *)
  match settings.githubbot_token with
  | Some value -> value
  | None ->
    log_event (MissingSecret "githubBotToken");
    failwith "MissingSecret"
;;

let get_forecast_id () =
  (* let* settings = load_settings () in *)
  match settings.forecast_id with
  | Some value -> value
  | None ->
    log_event (MissingConfig "forecastId");
    failwith "MissingConfig"
;;

let get_forecast_token () =
  (* let* settings = load_settings () in *)
  match settings.forecast_token with
  | Some "" ->
    log_event (EmptySecret "forecastToken");
    ""
  | Some nonEmptyValue -> nonEmptyValue
  | None ->
    log_event (MissingSecret "forecastToken");
    failwith "MissingSecret"
;;

let get_slack_bot_token () =
  (* let* settings = load_settings () in *)
  match settings.slack_bot_token with
  | Some value -> value
  | None ->
    log_event (MissingSecret "slackBotToken");
    failwith "MissingSecret"
;;

let get_slack_app_token () =
  (* let* settings = load_settings () in *)
  match settings.slack_app_token with
  | Some value -> value
  | None ->
    log_event (MissingSecret "slackAppToken");
    failwith "MissingSecret"
;;

let get_extra_users () =
  match Utils.read_file users_path with
  | None -> []
  | Some contents ->
    let lines = String.split_on_char '\n' contents in
    List.filter_map
      (fun l ->
        match l |> String.trim |> String.split_on_char ':' with
        | [ name; login ] -> Some (name, login)
        | _ -> None)
      lines
;;
