(** Utilities for logging issues during the course of ingesting the data for 
    later summary and notification 
 *)

type level =
  | Error
  | Warning
  | Info   
  | Debug  

type source = 
  | Forecast
  | GitHub
  | GitHubMetadata 
  | Schedule

type entity =
  | RawForecastProject of string 
  | Project of int
  | Person of string
  | RawForecastPerson of string
  | Assignment of (int * string)

type event = {
    level : level;
    source : source;
    entity : entity;
    message : string
  }

(* ------------------------------------------------------------ *)

(* The log is a mutable stack of events *)
let the_log : event Stack.t = Stack.create()

let default_logger lvl src ent msg =
  Stack.push {level = lvl; source = src; entity = ent; message = msg} the_log

let the_logger = ref default_logger


(* Interface -------------------------------------------------- *)

let log (lvl : level) (src : source) (ent : entity)
      (msg : string) : unit =
  !the_logger lvl src ent msg

let get_the_log () =
  Stack.to_seq the_log

let show_level = function
  | Error -> "Error"
  | Warning -> "Warning"
  | Info -> "Info"
  | Debug -> "Debug"

let show_source = function
  | Forecast -> "Forecast"
  | GitHub -> "GitHub"
  | GitHubMetadata -> "GitHub Metadata"
  | Schedule -> "Schedule"

let show_origin = function

