(** Utilities for logging issues during the course of ingesting the data for 
    later summary and notification 
 *)

type level =
  | Fatal of int
  | Error of int
  | Warning of int
  | Info
  | Debug

type source =
  | Forecast
  | Github
  | GithubMetadata
  | Schedule

type entity =
  | RawForecastProject of string
  | Project of int
  | RawForecastPerson of string
  | Person of string
  | RawForecastAssignment
  | Assignment of (int * string)

type event =
  { level : level
  ; source : source
  ; entity : entity
  ; message : string
  }

(* ------------------------------------------------------------ *)

(* The log is a mutable stack of events *)
let the_log : event Stack.t = Stack.create ()

(* Interface -------------------------------------------------- *)

let log lvl src ent msg =
  Stack.push { level = lvl; source = src; entity = ent; message = msg } the_log;
  match lvl with
  | Fatal _ ->
    (* TODO: print the log instead of this cheesy message *)
    print_endline "Uh-oh";
    exit 2
  | _ -> ()
;;

let get_the_log () = Stack.to_seq the_log

let show_source = function
  | Forecast -> "Forecast"
  | Github -> "GitHub"
  | GithubMetadata -> "GitHub Metadata"
  | Schedule -> "Schedule"
;;

let show_level = function
  | Fatal n -> Printf.sprintf "F%d" n
  | Error n -> Printf.sprintf "E%d" n
  | Warning n -> Printf.sprintf "W%d" n
  | Info -> "Info"
  | Debug -> "Debug"
;;

let isFatal e = match e.level with
  | Fatal _ -> true
  | _ -> false
;;
let isError e = match e.level with
  | Error _ -> true
  | _ -> false
;;
let isWarning e = match e.level with
  | Warning _ -> true
  | _ -> false
;;
let isInfo e = match e.level with
  | Info -> true
  | _ -> false
;;
let isDebug e = match e.level with
  | Debug -> true
  | _ -> false
;;

(* Printing --------------------------------------------------- *)

(* TODO: restructure code so that info/debug don't need ints *)
let make_display_message ?(color = true) e =
  let open ANSITerminal in
  let style =
    if not color
    then []
    else (
      match e.level with
      | Fatal _ -> [ Bold; Foreground Red ]
      | Error _ -> [ Foreground Red ]
      | Warning _ -> [ Foreground Yellow ]
      | _ -> [])
  in
  match e.level with
  | Fatal n -> ANSITerminal.sprintf style "F%d " n ^ e.message
  | Error n -> ANSITerminal.sprintf style "E%d " n ^ e.message
  | Warning n -> ANSITerminal.sprintf style "W%d " n ^ e.message
  | Info -> ANSITerminal.sprintf style "Info  " ^ e.message
  | Debug -> ANSITerminal.sprintf style "Debug  " ^ e.message
;;
