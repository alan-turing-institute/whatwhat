open Cohttp
open Cohttp_lwt_unix
open Yojson
(* TODO: Change this to add the notifications to a stack *)

type log_type =
  | Error
  | Warning

let show_log_type = function
  | Error -> "Error"
  | Warning -> "Warning"
;;

exception SlackAPIError of string

(* Read the logger from an environment variable, assuming it's one of the valid options,
   otherwise use the default.*)
let default_logger = "stdout"
let valid_loggers = [ "stdout"; "slack"; "null" ]

let log_method =
  let envvar_opt = Sys.getenv_opt "WHATWHAT_LOG_METHOD" in
  let envvar =
    match envvar_opt with
    | Some envvar -> envvar
    | None -> default_logger
  in
  if List.mem envvar valid_loggers then envvar else default_logger
;;

let lvl_prefix lvl = show_log_type lvl ^ ":"
let null_logger _ _ = ()
let stdout_logger lvl msg = Printf.printf "%-8s %s\n" (lvl_prefix lvl) msg

(* Slack logging *)
let slack_post_message_url = "https://slack.com/api/chat.postMessage"
let slack_token = Config.settings.slack_token

let slack_logger lvl msg =
  let auth_cred = Auth.credential_of_string ("Bearer " ^ slack_token) in
  let header =
    Header.init ()
    |> (fun header -> Header.add_authorization header auth_cred)
    |> fun header -> Header.add header "Content-type" "application/json"
  in
  let body_string = Printf.sprintf "%-8s %s" (lvl_prefix lvl) msg in
  let request_body_obj = Cohttp_lwt.Body.of_string @@ "{'text': '" ^ body_string ^ "'}" in
  let uri = Uri.of_string slack_post_message_url in
  let response, response_body =
    Client.post ~headers:header ~body:request_body_obj uri |> Lwt_main.run
  in
  let () = Utils.check_http_response response in
  let response_body_json =
    response_body |> Cohttp_lwt.Body.to_string |> Lwt_main.run |> Basic.from_string
  in
  (* Error or warn if the response body reports something other than all-good. See
     https://api.slack.com/web#responses for what to expect of the response. *)
  let ok_opt = Basic.Util.member "ok" response_body_json |> Basic.Util.to_bool_option in
  let error_opt =
    Basic.Util.member "error" response_body_json |> Basic.Util.to_string_option
  in
  let warning_opt =
    Basic.Util.member "warning" response_body_json |> Basic.Util.to_string_option
  in
  match ok_opt with
  | Some true ->
    (match warning_opt with
     | Some x -> stdout_logger Warning ("Slack response warning: " ^ x)
     | None -> () (* Got ok: true and no warning, nothing to report. *))
  | _ ->
    let error =
      match error_opt with
      | Some x -> x
      | None -> ""
    in
    raise @@ SlackAPIError error
;;

let the_logger =
  ref
    (match log_method with
     | "stdout" -> stdout_logger
     | "slack" -> slack_logger
     | _ -> null_logger)
;;

(* "Every problem in computer science can be solved through another layer of indirection"
 *)
let log (lvl : log_type) (msg : string) : unit = !the_logger lvl msg
