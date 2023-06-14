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
  ; slack_token : string option
  }

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

let load_settings () : t =
  (* let use_color = true in *)
  let secrets_json_opt =
    try Some (Yojson.Basic.from_file secrets_path) with
    | Sys_error _ -> 
        Printf.printf "\n";
        Pretty.prout ~use_color:true [ Bold; Foreground Red ] "E0001 ";
        Printf.printf "Missing secrets file: %s\n" secrets_path ;
        Printf.printf "Run whathwhat populateconfig to create a %s file." secrets_path;
        raise (MissingSecretsFile secrets_path)
  in
  let config_json_opt =
    try Some (Yojson.Basic.from_file config_path) with
    | Sys_error _ -> 
      Printf.printf "\n";
      Pretty.prout ~use_color:true [ Bold; Foreground Red ] "E0002 ";
      Printf.printf "Missing config file: %s\n" config_path;
      Printf.printf "Run whathwhat populateconfig to create a %s file." config_path;
      raise (MissingConfigFile config_path)
  in
  { github_project_name =
      find_setting string_opt_of_json "githubProjectName" config_json_opt
  ; github_repo_name = find_setting string_opt_of_json "githubRepoName" config_json_opt
  ; github_repo_owner = find_setting string_opt_of_json "githubRepoOwner" config_json_opt
  ; github_token = find_setting string_opt_of_json "githubToken" secrets_json_opt
  ; githubbot_token = find_setting string_opt_of_json "githubBotToken" secrets_json_opt
  ; github_project_columns =
      find_setting string_list_opt_of_json "githubProjectColumns" config_json_opt
  ; forecast_id = find_setting string_opt_of_json "forecastId" config_json_opt
  ; forecast_token = find_setting string_opt_of_json "forecastToken" secrets_json_opt
  ; slack_token = find_setting string_opt_of_json "slackToken" secrets_json_opt
  }
;;

let settings = load_settings ()

let get_github_project_name () =
  match settings.github_project_name with
  | Some value -> value
  | None -> raise (MissingConfig "githubProjectName")
;;

let get_github_project_columns () = settings.github_project_columns

let get_github_repo_name () =
  match settings.github_repo_name with
  | Some value -> value
  | None -> raise (MissingConfig "githubRepoName")
;;

let get_github_repo_owner () =
  match settings.github_repo_owner with
  | Some value -> value
  | None -> raise (MissingConfig "githubRepoOwner")
;;

let get_github_token () =
  match settings.github_token with
  | Some value -> value
  | None -> raise (MissingSecret "githubToken")
;;

let get_githubbot_token () =
  match settings.githubbot_token with
  | Some value -> value
  | None -> raise (MissingSecret "githubbotToken")
;;

let get_forecast_id () =
  match settings.forecast_id with
  | Some value -> value
  | None -> raise (MissingConfig "forecastId")
;;

let get_forecast_token () =
  match settings.forecast_token with
  | Some value -> value
  | None -> raise (MissingSecret "forecastToken")
;;

let get_slack_token () =
  match settings.slack_token with
  | Some value -> value
  | None -> raise (MissingSecret "githubBotToken")
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
