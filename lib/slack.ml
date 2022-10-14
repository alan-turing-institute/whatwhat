open Cohttp
open Cohttp_lwt_unix
module Basic = Yojson.Basic

exception SlackAPIError of string

let slack_post_message_url = "https://slack.com/api/chat.postMessage"
let slack_token = Config.get_slack_token ()
let slack_channel = "C0465E3FEN4" (* The #hut23-whatwhat-bot-test channel *)

let header =
  let auth_cred = Auth.credential_of_string ("Bearer " ^ slack_token) in
  Header.init ()
  |> (fun header -> Header.add_authorization header auth_cred)
  |> fun header -> Header.add header "Content-type" "application/json"
;;

let slack_uri = Uri.of_string slack_post_message_url

let make_body msg =
  Cohttp_lwt.Body.of_string
  @@ "{\"channel\": \""
  ^ slack_channel
  ^ "\", \"text\": \""
  ^ String.escaped msg
  ^ "\"}"
;;

let check_response (response, body) =
  let () = Utils.check_http_response response in
  let body_json =
    body |> Cohttp_lwt.Body.to_string |> Lwt_main.run |> Basic.from_string
  in
  (* Error or warn if the response body reports something other than all-good. See
     https://api.slack.com/web#responses for what to expect of the response. *)
  let ok_opt = Basic.Util.member "ok" body_json |> Basic.Util.to_bool_option in
  let error_opt = Basic.Util.member "error" body_json |> Basic.Util.to_string_option in
  let warning_opt =
    Basic.Util.member "warning" body_json |> Basic.Util.to_string_option
  in
  match ok_opt with
  | Some true ->
    (match warning_opt with
     (* TODO We could try to use the Log module here to raise this Warning, but that runs
       the risk of an infinite loop. *)
     | Some x -> print_endline ("Slack response warning: " ^ x)
     | None -> () (* Got ok: true and no warning, nothing to report. *))
  | _ ->
    let error = Option.value error_opt ~default:"" in
    raise @@ SlackAPIError error
;;

(** Post a [msg] to Slack.

    If the HTTP POST request fails, raise a [HttpError]. If it passes, but the Slack API
    returns a response that indicates an error, raise a [SlackAPIError]. If the Slack API
    response indicates success with a warning, print the warning to [stdout].

    Return [()]. *)
let post msg =
  let body = make_body msg in
  Client.post ~headers:header ~body slack_uri |> Lwt_main.run |> check_response
;;
