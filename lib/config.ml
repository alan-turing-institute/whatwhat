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

exception MissingGHToken of string
exception MissingForecastToken of string
exception MissingSecret of string
exception MissingConfig of string
exception MissingSecretsFile of string
exception MissingConfigFile of string

let secrets_path = XDGBaseDir.default.config_home ^ "/whatwhat/secrets.json"
let config_path = XDGBaseDir.default.config_home ^ "/whatwhat/config.json"
let users_path = XDGBaseDir.default.config_home ^ "/whatwhat/users"
let ( >>= ) = Option.bind

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

  (* strip all before characters before last/ from string *)
  let file_type = String.split_on_char '/' path |> List.rev |> List.hd in 


  (* if the file exists do X, if not do Y *)
  try 
    let _ = open_in path in
    Pretty.prout ~use_color:true [ Bold; Foreground Yellow ] "\nInformation: ";
    Printf.printf "The %s file exists. If you want to make any changes, please update the file yourself in %s\n" file_type path
  with
    | Sys_error _ -> 
      (* Pretty.prout ~use_color:true [ Bold; Foreground Magenta ] "Please acknowledge!: "; *)
      Printf.printf "I have written a %s file to %s. " file_type path;

      if update_message then
        print_endline "Make sure you update the tokens there following the instructions in comments. ";
      let oc = open_out path in
      Printf.fprintf oc "%s\n" message;
      close_out oc
;;

let load_settings () : t =

  (* let use_color = true in *)
  let secrets_json_opt =
    try Some (Yojson.Basic.from_file secrets_path) with
    | Sys_error _ -> 
      Printf.printf "\n";
      Pretty.prout ~use_color:true [ Bold; Foreground Red ] "E0002 ";
      Printf.printf "Missing secrets file: %s\n" secrets_path; 

      (* Ask user if its ok to set up new file *)
      Pretty.prout ~use_color:true [ Bold; Foreground Green ] "Attention!: ";
      print_string "Are you happy for me to set up a secrets template for you? (yes/no): ";
      let answer = read_line () in

      (* If answer starts with Y or y *)
      if String.lowercase_ascii (String.sub answer 0 1) = "y" then
        let message_secrets = "{\n" ^
          "  /* githubToken: Required for project reactions. This can generate at https://github.com/settings/tokens. The token will need to have the permissions: read:user, repo, and user:email */\n" ^
          "  \"githubToken\"    : \"\", \n\n" ^
          "  /* githubBotToken: OPTIONAL (used to post to github from whatwhat- primarily for whatwhat admins). You need to be added to the hut23-1206-nowwhat@turing.ac.uk group (ask someone else on the whatwhat developer team to add you, e.g. the person who most recently committed to main) */\n" ^
          "  \"githubBotToken\" : \"\", \n\n" ^
          "  /* forecastToken : Required for project allocations. This can be obtained from https://id.getharvest.com/oauth2/access_tokens/new.  */\n" ^
          "  \"forecastToken\"  : \"\", \n\n" ^
          "  /* slackBotToken: OPTIONAL (used to post to slack from whatwhat - primarily for whatwhat admins). You need to be added to the hut23-1206-nowwhat@turing.ac.uk group (ask someone else on the whatwhat developer team to add you, e.g. the person who most recently committed to main) */\n" ^
          "  \"slackBotToken\"     : \"\" \n" ^
          "  /* slackAppToken: OPTIONAL (used to connect to slack from whatwhat - primarily for whatwhat admins). You need to be added to the hut23-1206-nowwhat@turing.ac.uk group (ask someone else on the whatwhat developer team to add you, e.g. the person who most recently committed to main) */\n" ^
          "  \"slackAppToken\"     : \"\" \n" ^
        "}" in
        
        (* run attempt_file on the secrets_path *)
        let _ = attempt_file secrets_path message_secrets true in
        
        Some (Yojson.Basic.from_file secrets_path)

      (*  If no, exit *)
      else
        (Pretty.prout ~use_color:true [ Bold; Foreground Red ] "Exiting...";
        raise (MissingSecretsFile secrets_path))
  in
  let config_json_opt =
    try Some (Yojson.Basic.from_file config_path) with
    | Sys_error _ -> 
      Printf.printf "\n";
      Pretty.prout ~use_color:true [ Bold; Foreground Red ] "E0002 ";
      Printf.printf "Missing config file: %s\n" config_path; 

      (* Ask user if its ok to set up new file *)
      Pretty.prout ~use_color:true [ Bold; Foreground Green ] "Attention!: ";
      print_string "Are you happy for me to set up a config template for you? (yes/no): ";
      let answer = read_line () in

      (* If answer starts with Y or y *)
      if String.lowercase_ascii (String.sub answer 0 1) = "y" then
          let message_config = "{\n" ^ 
          "  \"forecastId\": \"974183\",\n" ^ 
          "  \"forecastIgnoredProjects\": [\"1684536\"],\n" ^ 
          "  \"forecastUrl\": \"https://api.forecastapp.com\",\n" ^ 
          "  \"githubProjectName\": \"Project Tracker\",\n" ^ 
          "  \"githubProjectColumns\": [\"Finding people\", \"Awaiting start\", \"Active\"],\n" ^ 
          "  \"githubRepoOwner\": \"alan-turing-institute\",\n" ^ 
          "  \"githubRepoName\": \"Hut23\",\n" ^ 
          "  \"githubUrl\": \"https://api.github.com\",\n" ^ 
          "  \"userLookup\": \"/Users/kgoldmann/.config/nowwhat/user_lookup.csv\"\n" ^ 
        "}" in
        
        (* run attempt_file on the config_path *)
        let _ = attempt_file config_path message_config false in

        
        Some (Yojson.Basic.from_file config_path)
        

      (*  If no, exit *)
      else
        (Pretty.prout ~use_color:true [ Bold; Foreground Red ] "Exiting...";
        raise (MissingConfigFile config_path) )
  in

  (* if githubToken == '' throw error *)
  if Option.get (find_setting string_opt_of_json "githubToken" secrets_json_opt) = "" then
    (Pretty.prout ~use_color:true [ Bold; Foreground Yellow ] "\nW0001 ";
    Printf.printf "Your Github token is empty! Update in %s\n" secrets_path);
  
  if Option.get (find_setting string_opt_of_json "forecastToken" secrets_json_opt) = "" then
    (Pretty.prout ~use_color:true [ Bold; Foreground Yellow ] "\nW0001 ";
    Printf.printf "Your Forecast token is empty!\n");

  
  { 
    github_project_name = find_setting string_opt_of_json "githubProjectName" config_json_opt
  ; github_repo_name = find_setting string_opt_of_json "githubRepoName" config_json_opt
  ; github_repo_owner = find_setting string_opt_of_json "githubRepoOwner" config_json_opt
  ; github_token = find_setting string_opt_of_json "githubToken" secrets_json_opt
  ; githubbot_token = find_setting string_opt_of_json "githubBotToken" secrets_json_opt
  ; github_project_columns = find_setting string_list_opt_of_json "githubProjectColumns" config_json_opt
  ; forecast_id = find_setting string_opt_of_json "forecastId" config_json_opt
  ; forecast_token = find_setting string_opt_of_json "forecastToken" secrets_json_opt
  ; slack_bot_token = find_setting string_opt_of_json "slackBotToken" secrets_json_opt
  ; slack_app_token = find_setting string_opt_of_json "slackAppToken" secrets_json_opt
  }
;;

let settings = load_settings ()

let get_github_project_name () =
  (* let* settings = load_settings () in *)
  match settings.github_project_name with
  | Some value -> value
  | None -> raise (MissingConfig "githubProjectName")
;;

let get_github_project_columns () = 
  (* let* settings = load_settings () in *)
  settings.github_project_columns
;;

let get_github_repo_name () =
  (* let* settings = load_settings () in *)
  match settings.github_repo_name with
  | Some value -> value
  | None -> raise (MissingConfig "githubRepoName")
;;

let get_github_repo_owner () =
  (* let* settings = load_settings () in *)
  match settings.github_repo_owner with
  | Some value -> value
  | None -> raise (MissingConfig "githubRepoOwner")
;;

let get_github_token () =
  (* let* settings = load_settings () in *)
  match settings.github_token with
  | Some value -> value
  | None -> raise (MissingSecret "githubToken")
;;

let get_githubbot_token () =
  (* let* settings = load_settings () in *)
  match settings.githubbot_token with
  | Some value -> value
  | None -> raise (MissingSecret "githubbotToken")
;;

let get_forecast_id() =
  (* let* settings = load_settings () in *)
  match settings.forecast_id with
  | Some value -> value
  | None -> raise (MissingConfig "forecastId")
;;

let get_forecast_token () =
  (* let* settings = load_settings () in *)
  match settings.forecast_token with
  | Some value -> value
  | None -> raise (MissingSecret "forecastToken")
;;

let get_slack_bot_token () =
  (* let* settings = load_settings () in *)
  match settings.slack_bot_token with
  | Some value -> value
  | None -> raise (MissingSecret "slackBotToken")
;;

let get_slack_app_token () =
  (* let* settings = load_settings () in *)
  match settings.slack_app_token with
  | Some value -> value
  | None -> raise (MissingSecret "slackAppToken")
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
