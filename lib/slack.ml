open Printf
open Cohttp_lwt_unix
module Basic = Yojson.Basic
open Utils
open Lwt.Syntax

exception SlackAPIError of string
exception ParseFailed

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

type message =
  { envelope_id : string
  ; text : string
  ; user : string
  ; channel : string
  ; timestamp : string
  }
[@@deriving show]

type slash_command =
  { envelope_id : string
  ; channel_id : string
  ; user_id : string (* user who invoked the command *)
  ; command : string
  ; text : string
  }
[@@deriving show]

type slack_event =
  | Ping
    (* This is not an @ ping on Slack, but just a 'ping' from Slack's server to
  check whether we're still connected, part of the WebSocket protocol *)
  | Hello of int (* connection count *)
  | Message of message (* someone posted a message in a channel *)
  | PingMessage of message (* someone pinged the bot in a channel *)
  | SlashCommand of slash_command
[@@deriving show]

(** Parse frame as a slash command *)
let parse_as_slash_command (frame : Websocket.Frame.t) : slack_event =
  let open Yojson.Basic in
  try
    let contents = frame.content |> from_string in
    let message_type = contents |> Util.member "type" |> to_string |> dequote in
    let envelope_id = contents |> Util.member "envelope_id" |> to_string |> dequote in
    if message_type <> "slash_commands" then raise ParseFailed;
    let payload = contents |> Util.member "payload" in
    let command = payload |> Util.member "command" |> to_string |> dequote in
    let text = payload |> Util.member "text" |> to_string |> dequote in
    let user_id = payload |> Util.member "user_id" |> to_string |> dequote in
    let channel_id = payload |> Util.member "channel_id" |> to_string |> dequote in
    SlashCommand { envelope_id; channel_id; user_id; command; text }
  with
  | _ -> raise ParseFailed
;;

(** Parse frame as a message *)
let parse_as_message (frame : Websocket.Frame.t) : slack_event =
  let open Yojson.Basic in
  try
    let contents = frame.content |> from_string in
    let message_type = contents |> Util.member "type" |> to_string |> dequote in
    let envelope_id = contents |> Util.member "envelope_id" |> to_string |> dequote in
    if message_type <> "events_api" then raise ParseFailed;
    let event = contents |> Util.member "payload" |> Util.member "event" in
    let event_type = event |> Util.member "type" |> to_string |> dequote in
    if dequote event_type <> "message" then raise ParseFailed;
    let text = event |> Util.member "text" |> to_string |> dequote in
    let user = event |> Util.member "user" |> to_string |> dequote in
    let channel = event |> Util.member "channel" |> to_string |> dequote in
    let timestamp = event |> Util.member "ts" |> to_string |> dequote in
    Message { envelope_id; text; user; channel; timestamp }
  with
  | _ -> raise ParseFailed
;;

(** Parse frame as a ping-message *)
let parse_as_pingmessage (frame : Websocket.Frame.t) : slack_event =
  let open Yojson.Basic in
  try
    let contents = frame.content |> from_string in
    let message_type = contents |> Util.member "type" |> to_string in
    let envelope_id = contents |> Util.member "envelope_id" |> to_string |> dequote in
    if dequote message_type <> "events_api" then raise ParseFailed;
    let event =
      frame.content |> from_string |> Util.member "payload" |> Util.member "event"
    in
    let event_type = event |> Util.member "type" |> to_string |> dequote in
    if dequote event_type <> "app_mention" then raise ParseFailed;
    let text = event |> Util.member "text" |> to_string |> dequote in
    let user = event |> Util.member "user" |> to_string |> dequote in
    let channel = event |> Util.member "channel" |> to_string |> dequote in
    let timestamp = event |> Util.member "ts" |> to_string |> dequote in
    PingMessage { envelope_id; text; user; channel; timestamp }
  with
  | _ -> raise ParseFailed
;;

(** Parse frame as hello *)
let parse_as_hello (frame : Websocket.Frame.t) : slack_event =
  let open Yojson.Basic in
  try
    let contents = frame.content |> from_string in
    let message_type = contents |> Util.member "type" |> to_string in
    if message_type <> "\"hello\"" then raise ParseFailed;
    let conn_count = contents |> Util.member "num_connections" |> to_string in
    Hello (int_of_string conn_count)
  with
  | _ -> raise ParseFailed
;;

(** Parse frame as a ping *)
let parse_as_ping (frame : Websocket.Frame.t) : slack_event =
  if frame.opcode = Websocket.Frame.Opcode.Ping then Ping else raise ParseFailed
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
    | [] -> raise ParseFailed
    | p :: ps' ->
      (try p frame with
       | ParseFailed -> getFirstParseSuccess ps')
  in
  getFirstParseSuccess parsers
;;

(** Print JSON contents of a frame to standard output. *)
let print_frame_json (frame : Websocket.Frame.t) : unit Lwt.t =
  print_endline frame.content;
  Lwt.return_unit
;;

let pong (conn : Websocket_lwt_unix.conn) : unit Lwt.t =
  let frame = Websocket.Frame.create ~opcode:Websocket.Frame.Opcode.Pong () in
  Websocket_lwt_unix.write conn frame
;;

let post_message (bot_token : string) (channel : string) (message : string) : unit Lwt.t =
  let reply_content = sprintf {|{"channel": "%s", "text": "%s"}|} channel message in
  let body = Cohttp_lwt.Body.of_string reply_content in
  let postMessageUri =
    "https://slack.com/api/chat.postMessage"
    |> Uri.of_string
    |> fun u -> Uri.add_query_param' u ("channel", channel)
  in
  let headers =
    Cohttp.Header.of_list
      [ "Content-type", "application/json"; "Authorization", "Bearer " ^ bot_token ]
  in
  let* _ = Client.post ~headers ~body postMessageUri in
  Lwt.return_unit
;;

let add_reaction
  (bot_token : string)
  (channel : string)
  (timestamp : string)
  (reaction : string)
  : unit Lwt.t
  =
  let addReactionUri =
    "https://slack.com/api/reactions.add"
    |> Uri.of_string
    |> fun u ->
    Uri.add_query_param' u ("channel", channel)
    |> fun u ->
    Uri.add_query_param' u ("timestamp", timestamp)
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

let return_envelope (envelope_id : string) (conn : Websocket_lwt_unix.conn) : unit Lwt.t =
  let content = sprintf "{ \"envelope_id\": \"%s\" }" envelope_id in
  let frame = Websocket.Frame.create ~content () in
  Websocket_lwt_unix.write conn frame
;;

let acknowledge_event (conn : Websocket_lwt_unix.conn) (event : slack_event) : unit Lwt.t =
  match event with
  | Ping -> pong conn
  | Hello _ -> Lwt.return_unit
  | PingMessage m -> return_envelope m.envelope_id conn
  | Message m -> return_envelope m.envelope_id conn
  | SlashCommand m -> return_envelope m.envelope_id conn
;;

let handle_event
  (conn : Websocket_lwt_unix.conn)
  (bot_token : string)
  (userId : string)
  (event : slack_event)
  : unit Lwt.t
  =
  ignore conn;
  ignore userId;
  match event with
  | Ping -> Lwt.return_unit
  | Hello n ->
    printf " *** Received a hello. Current number of connections: %d\n" n;
    Lwt.return_unit
  | PingMessage m ->
    printf
      " *** Received a ping-message. Envelope ID: %s, text: %s, user: %s\n"
      m.envelope_id
      m.text
      m.user;
    Lwt.return_unit
  | Message m ->
    printf
      " *** Received a message. Envelope ID: %s, text: %s, user: %s\n"
      m.envelope_id
      m.text
      m.user;

    (* React with a randomly selected emoji *)
    let possible_reactions =
      [ "eyes"
      ; "heart_eyes"
      ; "kissing_closed_eyes"
      ; "face_with_rolling_eyes"
      ; "thinking_face"
      ; "face_with_monocle"
      ; "exploding_head"
      ]
    in
    let reaction =
      List.nth possible_reactions (Random.int (List.length possible_reactions))
    in
    add_reaction bot_token m.channel m.timestamp reaction
  | SlashCommand sc ->
    printf
      " *** Received a slash command. Envelope ID: %s, command: %s, text: %s, user: %s\n"
      sc.envelope_id
      sc.command
      sc.text
      sc.user_id;

    if sc.command = "/whatwhat-person"
    then
      (* Acknowledge in channel *)
      let* _ =
        post_message
          bot_token
          sc.channel_id
          (sprintf
             "<@%s>, you just ran: `/whatwhat-person %s`. I will respond shortly!"
             sc.user_id
             sc.text)
      in

      (* Run whatwhat *)
      let open CalendarLib.Date in
      let start_date = make 2016 1 1 in
      let end_date = add (today ()) (Period.year 1) in
      let* people, projects, assignments =
        Schedule.get_the_schedule_async ~start_date ~end_date
      in

      let matched_people =
        List.filter
          (fun (p : Domain.person) ->
            Utils.contains ~case_sensitive:false p.full_name sc.text
            || Utils.contains ~case_sensitive:false p.email sc.text
            ||
            match p.github_handle with
            | None -> false
            | Some s -> Utils.contains ~case_sensitive:false s sc.text)
          people
      in
      match matched_people with
      | [] ->
        let msg = Printf.sprintf "No person with the name '%s' was found." sc.text in
        post_message bot_token sc.channel_id ("```" ^ msg ^ "```")
      | [ p ] ->
        let* msg = Person.make_slack_output p projects assignments in
        post_message bot_token sc.channel_id ("```" ^ msg ^ "```")
      | ps ->
        let s =
          Printf.sprintf "Multiple people were found matching the string '%s':" sc.text
        in
        let possible_names =
          List.map
            (fun (p : Domain.person) ->
              match p.github_handle with
              | None -> p.full_name
              | Some s -> Printf.sprintf "%s (@%s)" p.full_name s)
            ps
        in
        let msg = String.concat "\n" (s :: possible_names) in
        post_message bot_token sc.channel_id ("```" ^ msg ^ "```")
    else Lwt.return_unit
;;

(** Obtain the web socket URL from Slack, using an app-level token. This URL
    will be the one used for communication. *)
let getWebsocketUrl (token : string) : Uri.t Lwt.t =
  let authUri = Uri.of_string "https://slack.com/api/apps.connections.open" in
  let headers =
    Cohttp.Header.of_list
      [ "Content-type", "application/x-www-form-urlencoded"
      ; "Authorization", "Bearer " ^ token
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

(** Connect to the websocket URL and return an open websocket connection. *)
let connect_websocket (uri : Uri.t) : Websocket_lwt_unix.conn Lwt.t =
  let* (endp : Conduit.endp) = Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system in
  let ctx : Conduit_lwt_unix.ctx = Lazy.force Conduit_lwt_unix.default_ctx in
  let* (client : Conduit_lwt_unix.client) = Conduit_lwt_unix.endp_to_client ~ctx endp in
  Websocket_lwt_unix.connect client uri
;;

let run_bot () =
  Random.self_init ();
  let rec loop (conn : Websocket_lwt_unix.conn) (bot_token : string) (userId : string)
    : unit Lwt.t
    =
    let* frame = Websocket_lwt_unix.read conn in
    print_endline "==================================";
    let right_now = CalendarLib.Calendar.now () in
    CalendarLib.Printer.Calendar.dprint right_now;
    print_endline "";
    try
      let event = parse_frame frame in
      (* Run all of these concurrently *)
      let* () =
        Lwt.join
          [ print_frame_json frame
          ; acknowledge_event conn event
          ; handle_event conn bot_token userId event
          ]
      in
      loop conn bot_token userId
    with
    | ParseFailed ->
      print_endline "Failed to parse frame.";
      print_endline "Its opcode is:";
      print_endline (Websocket.Frame.Opcode.to_string frame.opcode);
      print_endline "Its contents are:";
      let* _ = print_frame_json frame in
      exit 1
    | End_of_file ->
      print_endline "Connection was unexpectedly closed.";
      exit 2
  in
  let app_token = Config.get_slack_app_token () in
  let bot_token = Config.get_slack_bot_token () in
  let main =
    let* uri = getWebsocketUrl app_token in
    let* conn = connect_websocket uri in
    let* userId = get_current_user bot_token in
    loop conn bot_token userId
  in
  Lwt_main.run main
;;
