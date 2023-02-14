(* We distinguish between secrets and configs, because we want to keep them in two
   separate files. They are otherwise treated identically though, and both are collected
   into the same record type `t`. *)
type t =
  { github_project_name : string option
  ; github_project_columns : string list option
  ; github_repo_name : string option
  ; github_repo_owner : string option
  ; github_token : string option
  ; github_url : string option
  ; githubbot_token : string option
  ; forecast_id : string option
  ; forecast_ignored_projects : int list option
  ; forecast_token : string option
  ; forecast_url : string option
  ; slack_token : string option
  }

exception MissingSecret of string
exception MissingConfig of string

let secrets_path = XDGBaseDir.default.config_home ^ "/nowwhat/secrets.json"
let config_path = XDGBaseDir.default.config_home ^ "/nowwhat/config.json"
let ( >>= ) = Option.bind

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
  let secrets_json_opt =
    try Some (Yojson.Basic.from_file secrets_path) with
    | Sys_error _ -> None
  in
  let config_json_opt =
    try Some (Yojson.Basic.from_file config_path) with
    | Sys_error _ -> None
  in
  { github_project_name =
      find_setting string_opt_of_json "githubProjectName" config_json_opt
  ; github_repo_name = find_setting string_opt_of_json "githubRepoName" config_json_opt
  ; github_repo_owner = find_setting string_opt_of_json "githubRepoOwner" config_json_opt
  ; github_token = find_setting string_opt_of_json "githubToken" secrets_json_opt
  ; github_url = find_setting string_opt_of_json "githubUrl" config_json_opt
  ; githubbot_token = find_setting string_opt_of_json "githubBotToken" secrets_json_opt
  ; github_project_columns = find_setting string_list_opt_of_json "githubProjectColumns" config_json_opt
  ; forecast_id = find_setting string_opt_of_json "forecastId" config_json_opt
  ; forecast_ignored_projects =
      find_setting int_list_opt_of_json "forecastIgnoredProjects" config_json_opt
  ; forecast_token = find_setting string_opt_of_json "forecastToken" secrets_json_opt
  ; forecast_url = find_setting string_opt_of_json "forecastUrl" config_json_opt
  ; slack_token = find_setting string_opt_of_json "slackToken" secrets_json_opt
  }
;;

let settings = load_settings ()

(* TODO Isn't there some metaprogramming way to autogenerate these? Preprocessing? *)
let get_github_project_name () =
  match settings.github_project_name with
  | Some value -> value
  | None -> raise (MissingConfig "githubProjectName")
;;

let get_github_project_columns () =
  match settings.github_project_columns with
  | Some value -> value
  | None -> raise (MissingConfig "githubProjectColumns")
;;

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

let get_github_url () =
  match settings.github_url with
  | Some value -> value
  | None -> raise (MissingConfig "githubUrl")
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

let get_forecast_ignored_projects () =
  match settings.forecast_ignored_projects with
  | Some value -> value
  | None -> raise (MissingConfig "forecastIgnoredProjects")
;;

let get_forecast_token () =
  match settings.forecast_token with
  | Some value -> value
  | None -> raise (MissingSecret "forecastToken")
;;

let get_forecast_url () =
  match settings.forecast_url with
  | Some value -> value
  | None -> raise (MissingConfig "forecastUrl")
;;

let get_slack_token () =
  match settings.slack_token with
  | Some value -> value
  | None -> raise (MissingSecret "githubBotToken")
;;
