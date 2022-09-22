(** Obtain API secrets from config file
    
    Looks first in the XDG config location*)

type t =
  { github_token : string
  ; forecast_id : string
  ; forecast_token : string
  }

let loadConfig () : t =
  let json =
    XDGBaseDir.default.config_home ^ "/nowwhat/secrets.json" |> Yojson.Basic.from_file
  in

  match json with
  | `Assoc
      [ ("githubToken", `String github_token)
      ; ("forecastId", `String forecast_id)
      ; ("forecastToken", `String forecast_token)
      ] -> { github_token; forecast_id; forecast_token }
  | _ -> failwith @@ "Could not decode config file\n" ^ Yojson.Basic.to_string json
;;

let settings = loadConfig ()
