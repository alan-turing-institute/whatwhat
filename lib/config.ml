(** Obtain API secrets from config file
    
    Looks first in the XDG config location*)

type t = { githubToken : string; forecastId : string; forecastToken : string }

let loadConfig () : t =
  let json =
    XDGBaseDir.default.config_home ^ "/nowwhat/secrets.json"
    |> Yojson.Basic.from_file
  in

  match json with
  | `Assoc
      [
        ("githubToken", `String githubToken);
        ("forecastId", `String forecastId);
        ("forecastToken", `String forecastToken);
      ] ->
      { githubToken; forecastId; forecastToken }
  | _ ->
      failwith @@ "Could not decode config file\n" ^ Yojson.Basic.to_string json

let settings = loadConfig ()
