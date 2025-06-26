open Printf
module Basic = Yojson.Basic
open Utils
open Lwt.Syntax
open Cohttp_lwt_unix

exception SlackAPIError of string
exception FrameParseFailed

(** TODO: Use this *)
let check_response (response, body) =
  let open Yojson.Basic in
  let () = Utils.check_http_response response in
  let body_json = body |> Cohttp_lwt.Body.to_string |> Lwt_main.run |> from_string in
  (* Error or warn if the response body reports something other than all-good. See
     https://api.slack.com/web#responses for what to expect of the response. *)
  let ok_opt = Util.member "ok" body_json |> Util.to_bool_option in
  let error_opt = Util.member "error" body_json |> Util.to_string_option in
  let warning_opt = Util.member "warning" body_json |> Util.to_string_option in
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

(** A message in a channel. *)
type message =
  { envelope_id : string
  ; text : string
  ; user : string
  ; channel : string
  ; timestamp : string
  }
[@@deriving show]

(** A slash-command run in a channel the bot is in. *)
type slash_command =
  { envelope_id : string
  ; channel_id : string
  ; user_id : string (* user who invoked the command *)
  ; command : string
  ; text : string
  }
[@@deriving show]

(** The types of Slack events that we know how to handle. *)
type slack_event =
  | Ping
    (* This is not an @ ping on Slack, but just a 'ping' from Slack's server to
  check whether we're still connected, part of the WebSocket protocol *)
  | Hello of int (* connection count *)
  | Message of message (* someone posted a message in a channel *)
  | PingMessage of message (* someone pinged the bot in a channel *)
  | SlashCommand of slash_command
[@@deriving show]

(** --- CONNECTION SETUP ------------------------
    In this library, we use the WebSocket protocol to connect to Slack using
    'socket mode'. This requires an app-level token (see the Config module). *)

(** Obtain the web socket URL from Slack, using an app-level token. This URL
    will be the one used for communication. *)
let get_websocket_url (app_token : string) : Uri.t Lwt.t =
  let authUri = Uri.of_string "https://slack.com/api/apps.connections.open" in
  let headers =
    Cohttp.Header.of_list
      [ "Content-type", "application/x-www-form-urlencoded"
      ; "Authorization", "Bearer " ^ app_token
      ]
  in
  let* _, respBody = Client.post ~headers authUri in
  let* body = Cohttp_lwt.Body.to_string respBody in
  match Yojson.Safe.from_string body with
  | `Assoc ps ->
    (match List.assoc_opt "url" ps with
     | Some (`String url) ->
       (* wss doesn't exist in Unix /etc/services, so we need to replace it
          with https, see https://github.com/mirage/ocaml-conduit/issues/79 *)
       Lwt.return
         (Uri.of_string (Str.replace_first (Str.regexp_string "wss") "https" url))
     | _ -> Lwt.fail (Failure "url was not found"))
  | _ -> Lwt.fail (Failure "response was not a json object")
;;

(** Connect to the URL and return an open websocket connection, which can then
    be used to send and receive 'frames' . *)
let connect_websocket (uri : Uri.t) : Websocket_lwt_unix.conn Lwt.t =
  let* (endp : Conduit.endp) = Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system in
  let ctx : Conduit_lwt_unix.ctx = Lazy.force Conduit_lwt_unix.default_ctx in
  let* (client : Conduit_lwt_unix.client) = Conduit_lwt_unix.endp_to_client ~ctx endp in
  Websocket_lwt_unix.connect client uri
;;

(** --- FRAME PARSERS ---------------------------
    Communication over the WebSocket protocol occurs via 'frames'. These are
    defined in RFC 6455: https://datatracker.ietf.org/doc/html/rfc6455#section-5
    The most important components of a frame are the 'opcode', which indicates
    the 'type' of frame that is being sent, and the 'payload', which is the
    actual contents that we are interested in.

    In OCaml, a frame is defined by the Websocket.Frame.t type:
    https://ocaml.org/p/websocket/2.2/doc/Websocket_lwt/Frame/index.html

    The functions in this section parse raw frames into known Slack events, so
    that they can subsequently be handled. *)

(** Parse frame as a slash command *)
let parse_as_slash_command (frame : Websocket.Frame.t) : slack_event =
  let open Yojson.Basic in
  try
    let contents = frame.content |> from_string in
    let message_type = contents |> Util.member "type" |> to_string |> dequote in
    let envelope_id = contents |> Util.member "envelope_id" |> to_string |> dequote in
    if message_type <> "slash_commands" then raise FrameParseFailed;
    let payload = contents |> Util.member "payload" in
    let command = payload |> Util.member "command" |> to_string |> dequote in
    let text = payload |> Util.member "text" |> to_string |> dequote in
    let user_id = payload |> Util.member "user_id" |> to_string |> dequote in
    let channel_id = payload |> Util.member "channel_id" |> to_string |> dequote in
    SlashCommand { envelope_id; channel_id; user_id; command; text }
  with
  | _ -> raise FrameParseFailed
;;

(** Parse frame as a message *)
let parse_as_message (frame : Websocket.Frame.t) : slack_event =
  let open Yojson.Basic in
  try
    let contents = frame.content |> from_string in
    let message_type = contents |> Util.member "type" |> to_string |> dequote in
    let envelope_id = contents |> Util.member "envelope_id" |> to_string |> dequote in
    if message_type <> "events_api" then raise FrameParseFailed;
    let event = contents |> Util.member "payload" |> Util.member "event" in
    let event_type = event |> Util.member "type" |> to_string |> dequote in
    if dequote event_type <> "message" then raise FrameParseFailed;
    let text = event |> Util.member "text" |> to_string |> dequote in
    let user = event |> Util.member "user" |> to_string |> dequote in
    let channel = event |> Util.member "channel" |> to_string |> dequote in
    let timestamp = event |> Util.member "ts" |> to_string |> dequote in
    Message { envelope_id; text; user; channel; timestamp }
  with
  | _ -> raise FrameParseFailed
;;

(** Parse frame as a ping-message *)
let parse_as_pingmessage (frame : Websocket.Frame.t) : slack_event =
  let open Yojson.Basic in
  try
    let contents = frame.content |> from_string in
    let message_type = contents |> Util.member "type" |> to_string in
    let envelope_id = contents |> Util.member "envelope_id" |> to_string |> dequote in
    if dequote message_type <> "events_api" then raise FrameParseFailed;
    let event =
      frame.content |> from_string |> Util.member "payload" |> Util.member "event"
    in
    let event_type = event |> Util.member "type" |> to_string |> dequote in
    if dequote event_type <> "app_mention" then raise FrameParseFailed;
    let text = event |> Util.member "text" |> to_string |> dequote in
    let user = event |> Util.member "user" |> to_string |> dequote in
    let channel = event |> Util.member "channel" |> to_string |> dequote in
    let timestamp = event |> Util.member "ts" |> to_string |> dequote in
    PingMessage { envelope_id; text; user; channel; timestamp }
  with
  | _ -> raise FrameParseFailed
;;

(** Parse frame as hello *)
let parse_as_hello (frame : Websocket.Frame.t) : slack_event =
  let open Yojson.Basic in
  try
    let contents = frame.content |> from_string in
    let message_type = contents |> Util.member "type" |> to_string in
    if message_type <> "\"hello\"" then raise FrameParseFailed;
    let conn_count = contents |> Util.member "num_connections" |> to_string in
    Hello (int_of_string conn_count)
  with
  | _ -> raise FrameParseFailed
;;

(** Parse frame as a ping *)
let parse_as_ping (frame : Websocket.Frame.t) : slack_event =
  if frame.opcode = Websocket.Frame.Opcode.Ping then Ping else raise FrameParseFailed
;;

(** Attempt to parse a WebSocket frame as a Slack event. *)
let parse_frame (frame : Websocket.Frame.t) : slack_event =
  let parsers =
    [ parse_as_slash_command
    ; parse_as_pingmessage
    ; parse_as_message
    ; parse_as_hello
    ; parse_as_ping
    ]
  in
  let rec getFirstParseSuccess ps =
    match ps with
    | [] -> raise FrameParseFailed
    | p :: ps' ->
      (try p frame with
       | FrameParseFailed -> getFirstParseSuccess ps')
  in
  getFirstParseSuccess parsers
;;

(** Print JSON contents of a frame to standard output. *)
let print_frame_json (frame : Websocket.Frame.t) : unit Lwt.t =
  print_endline frame.content;
  Lwt.return_unit
;;

(** --- EVENT HANDLERS ---------------------------
    Once a frame is received and parsed into an event, we must then perform some
    kind of action in response to it. In this library, actions are split up into
    'acknowledgement' and 'handling'.

    The former entails directly sending a response frame to Slack's servers,
    indicating that we have received the frame they sent. If this is not done,
    Slack starts retrying to send the frame, so it is quite important to send an
    acknowledgement first before actually performing any other actions.
    These are kept inside the SlackInternal module.

    The latter means some kind of high-level, visible, response to the event.
    For example, this could be posting a message to the channel, or reacting to
    a message. *)

(** Send a pong back to the server. This should be done in response to a ping. *)
let pong (conn : Websocket_lwt_unix.conn) : unit Lwt.t =
  let frame = Websocket.Frame.create ~opcode:Websocket.Frame.Opcode.Pong () in
  Websocket_lwt_unix.write conn frame
;;

(** Send a frame back to Slack's servers indicating that an envelope has been
    received. This is mandatory, or else Slack will start sending retries. *)
let return_envelope (envelope_id : string) (conn : Websocket_lwt_unix.conn) : unit Lwt.t =
  let content = sprintf "{ \"envelope_id\": \"%s\" }" envelope_id in
  let frame = Websocket.Frame.create ~content () in
  Websocket_lwt_unix.write conn frame
;;

(** Handlers for acknowledging all types of Slack events. This is purely to keep
    the WebSocket connection alive, and should not need to be modified unless new
    types of events are added. To specify how the bot should react to incoming
    events, see [handle_event]. *)
let acknowledge_event (conn : Websocket_lwt_unix.conn) (event : slack_event) : unit Lwt.t =
  match event with
  | Ping -> pong conn
  | Hello _ -> Lwt.return_unit
  | PingMessage m -> return_envelope m.envelope_id conn
  | Message m -> return_envelope m.envelope_id conn
  | SlashCommand m -> return_envelope m.envelope_id conn
;;

(** Post a message to Slack.

    This deliberately does NOT use the 'blocks' representation of Slack, because
    for some reason, blocks are wrapped to a certain width which annoyingly
    cannot be overridden. So the message is just passed as a (mrkdwn-formatted)
    string. *)
let post_message (bot_token : string) (channel : string) (message : string) : unit Lwt.t =
  let reply_content = sprintf {|{"channel": "%s", "text": "%s"}|} channel message in
  let body = Cohttp_lwt.Body.of_string reply_content in
  let postMessageUri = "https://slack.com/api/chat.postMessage" |> Uri.of_string in
  let headers =
    Cohttp.Header.of_list
      [ "Content-type", "application/json"; "Authorization", "Bearer " ^ bot_token ]
  in
  let* _ = Client.post ~headers ~body postMessageUri in
  Lwt.return_unit
;;

(** Add a reaction to a message.

    The reaction is specified as a string, without the surrounding colons. For
    example, if you want to react with :eyes:, pass "eyes" as this argument. *)
let add_reaction (bot_token : string) (message : message) (reaction : string) : unit Lwt.t
  =
  let addReactionUri =
    "https://slack.com/api/reactions.add"
    |> Uri.of_string
    |> fun u ->
    Uri.add_query_param' u ("channel", message.channel)
    |> fun u ->
    Uri.add_query_param' u ("timestamp", message.timestamp)
    |> fun u -> Uri.add_query_param' u ("name", reaction)
  in
  let headers =
    Cohttp.Header.of_list
      [ "Content-type", "application/x-www-form-urlencoded"
      ; "Authorization", "Bearer " ^ bot_token
      ]
  in
  let* _ = Client.post ~headers addReactionUri in
  Lwt.return_unit
;;

(** Get the user ID of the current user (i.e. the bot). *)
let get_current_user (bot_token : string) : string Lwt.t =
  let meUri = "https://slack.com/api/auth.test" |> Uri.of_string in
  let headers =
    Cohttp.Header.of_list
      [ "Content-type", "application/x-www-form-urlencoded"
      ; "Authorization", "Bearer " ^ bot_token
      ]
  in
  let* _, respBody = Client.get ~headers meUri in
  let* body = Cohttp_lwt.Body.to_string respBody in
  let open Yojson.Basic in
  let json = from_string body in
  Lwt.return (json |> Util.member "user_id" |> Util.to_string |> dequote)
;;

(** Get the username of a person with a given user_id. Not all user IDs
    correspond to real people (for example, bots don't), so those return None. *)
let get_username (bot_token : string) (user_id : string) : string option Lwt.t =
  let userUri =
    "https://slack.com/api/users.profile.get"
    |> Uri.of_string
    |> fun u -> Uri.add_query_param' u ("user", user_id)
  in
  let headers =
    Cohttp.Header.of_list
      [ "Content-type", "application/x-www-form-urlencoded"
      ; "Authorization", "Bearer " ^ bot_token
      ]
  in
  let* _, respBody = Client.get ~headers userUri in
  let* body = Cohttp_lwt.Body.to_string respBody in
  try
    let open Yojson.Basic in
    let json = from_string body in
    let real_name =
      json
      |> Util.member "profile"
      |> Util.member "real_name"
      |> Util.to_string
      |> dequote
    in
    Lwt.return (Some real_name)
  with
  | _ -> Lwt.return_none
;;
