type t =
  { github_token : string option
  ; githubbot_token : string option
  ; forecast_id : string option
  ; forecast_token : string option
  ; slack_token : string option
  }

exception MissingSecret of string

let secrets_path = XDGBaseDir.default.config_home ^ "/nowwhat/secrets.json"

(* Read the field called [key] from [json] as a [string option]. *)
let read_string_opt_field key json =
  Yojson.Basic.Util.member key json |> Yojson.Basic.Util.to_string_option
;;

(* Find the value for a secret called [key].

   If the environment variable called [String.uppercase_ascii ("WHATWHAT_" ^ key)]
   exists, return its value. If it doesn't, return the corresponding key from the secrets
   file. If that doesn't exist either, return [None]. *)
let find_secret key json_opt =
  let file_value = Option.bind json_opt @@ read_string_opt_field key in
  let envvar_key = String.uppercase_ascii ("WHATWHAT_" ^ key) in
  let envvar_value = Sys.getenv_opt envvar_key in
  match file_value, envvar_value with
  | _, Some value -> Some value
  | Some value, None -> Some value
  | None, None -> None
;;

let load_config () : t =
  let json_opt =
    try Some (Yojson.Basic.from_file secrets_path) with
    | Sys_error _ -> None
  in
  { github_token = find_secret "githubToken" json_opt
  ; githubbot_token = find_secret "githubBotToken" json_opt
  ; forecast_id = find_secret "forecastId" json_opt
  ; forecast_token = find_secret "forecastToken" json_opt
  ; slack_token = find_secret "slackToken" json_opt
  }
;;

let settings = load_config ()

(* TODO Isn't there some metaprogramming way to autogenerate these? Preprocessing? *)
let get_github_token () =
  match settings.github_token with
  | Some value -> value
  | None -> raise (MissingSecret "github_token")
;;

let get_githubbot_token () =
  match settings.githubbot_token with
  | Some value -> value
  | None -> raise (MissingSecret "githubbot_token")
;;

let get_forecast_id () =
  match settings.forecast_id with
  | Some value -> value
  | None -> raise (MissingSecret "forecast_id")
;;

let get_forecast_token () =
  match settings.forecast_token with
  | Some value -> value
  | None -> raise (MissingSecret "forecast_token")
;;

let get_slack_token () =
  match settings.slack_token with
  | Some value -> value
  | None -> raise (MissingSecret "github_bot_token")
;;
