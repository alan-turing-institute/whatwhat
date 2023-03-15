(** Utilities for logging issues during the course of ingesting the data for 
    later summary and notification 
 *)

type level =
  | Fatal
  | Error
  | Warning
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
  ; code : int
  ; source : source
  ; entity : entity
  ; message : string
  }

(* ------------------------------------------------------------ *)

(* The log is a mutable stack of events *)
let the_log : event Stack.t = Stack.create ()

let default_logger lvl code src ent msg =
  Stack.push { level = lvl; code; source = src; entity = ent; message = msg } the_log;
  (* TODO: kill the programme *)
  if lvl = Fatal then print_endline "Uh oh!"
;;

let the_logger = ref default_logger

(* Interface -------------------------------------------------- *)

let log (lvl : level) (code : int) (src : source) (ent : entity) (msg : string) : unit =
  !the_logger lvl code src ent msg
;;

let get_the_log () = Stack.to_seq the_log

let show_level = function
  | Fatal -> "Fatal"
  | Error -> "Error"
  | Warning -> "Warning"
  | Info -> "Info"
  | Debug -> "Debug"
;;

let show_source = function
  | Forecast -> "Forecast"
  | Github -> "GitHub"
  | GithubMetadata -> "GitHub Metadata"
  | Schedule -> "Schedule"
;;

(* TODO: restructure code so that info/debug don't need ints *)
let make_display_message ?(color=true) e = 
  let open ANSITerminal in
  let style = if not color then []
  else match e.level with
  | Fatal   -> [Bold; Foreground Red]
  | Error   -> [Foreground Red]
  | Warning -> [Foreground Yellow]
  | _ -> []
  in
  match e.level with
  | Fatal   -> (ANSITerminal.sprintf style "F%d " e.code) ^ e.message
  | Error   -> (ANSITerminal.sprintf style "E%d " e.code) ^ e.message
  | Warning -> (ANSITerminal.sprintf style "W%d " e.code) ^ e.message
  | Info    -> (ANSITerminal.sprintf style "I%d " e.code) ^ e.message
  | Debug   -> (ANSITerminal.sprintf style "D%d " e.code) ^ e.message
