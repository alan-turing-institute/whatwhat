open Printf
module Basic = Yojson.Basic
open Lwt.Syntax
open SlackInternal

(** React to a message with a randomly selected emoji. *)
let react_randomly (bot_token : string) (message : message) : unit Lwt.t =
  (* Initialise pseudorandom number generator *)
  Random.self_init ();
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
  add_reaction bot_token message reaction
;;

(** Response when a /whatwhat-person slash command is received. *)
let handle_whatwhat_person (bot_token : string) (sc : slash_command) =
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
    let msg = Printf.sprintf ":x: *No person with the name '%s' was found.*" sc.text in
    post_message bot_token sc.channel_id msg
  | [ p ] ->
    let* msg = Person.make_slack_output p projects assignments in
    post_message bot_token sc.channel_id msg
  | ps ->
    let s =
      Printf.sprintf
        ":interrobang: *Multiple people were found matching the string '%s':*"
        sc.text
    in
    let possible_names =
      List.map
        (fun (p : Domain.person) ->
          match p.github_handle with
          | None -> p.full_name
          | Some s -> Printf.sprintf " ⏵ %s (`@%s`)" p.full_name s)
        ps
    in
    let msg = String.concat "\n" (s :: possible_names) in
    post_message bot_token sc.channel_id msg
;;

(** Define custom handlers for Slack events here. *)
let handle_event (bot_token : string) (userId : string) (event : slack_event) : unit Lwt.t
  =
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
    if m.user <> userId then react_randomly bot_token m else Lwt.return_unit
  | SlashCommand sc ->
    printf
      " *** Received a slash command. Envelope ID: %s, command: %s, text: %s, user: %s\n"
      sc.envelope_id
      sc.command
      sc.text
      sc.user_id;
    (match sc.command with
     | "/whatwhat-person" -> handle_whatwhat_person bot_token sc
     | _ -> Lwt.return_unit)
;;

(** Run the Slack bot. *)
let run_bot () =
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
      Lwt.join
        [ print_frame_json frame
        ; acknowledge_event conn event
        ; handle_event bot_token userId event
        ; loop conn bot_token userId
        ]
    with
    | FrameParseFailed ->
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
    let* uri = get_websocket_url app_token in
    let* conn = connect_websocket uri in
    let* userId = get_current_user bot_token in
    loop conn bot_token userId
  in
  Lwt_main.run main
;;
