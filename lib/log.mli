(** Logging: Record problems for later notification
    This logging system is very specific to WhatWhat
    
    A logged message consits of:
    - The level  -- how serious is the problem
    - The source -- which part of whatwhat were we executing
    - The origin -- What caused the problem
    - The entity -- which entity was affected 

 *)

(** Severity of logged problem *)
type level =
  | Fatal of int (** Prevents whatwhat from continuing at all *)
  | Error of int (** Prevents processing of some data  *)
  | Warning of int (** Likely to cause an error if not fixed *)
  | Info (** Information for end users *)
  | Debug (** Information for whatwhat developers *)

(** Which module where we processing when the problem arose *)
type source =
  | Forecast (** When ingesting Forecast data *)
  | Github (** When ingesting GitHub data *)
  | GithubMetadata
  | Schedule (** When merging Forecast and GitHub data *)

type entity =
  | RawForecastProject of string (** Project name *)
  | Project of int (** The project code *)
  | RawForecastPerson of string (** Person's name *)
  | Person of string (** email address *)
  | RawForecastAssignment of int (** Assignment ID in Forecast *)
  | Assignment of (int * string) (** Pair of a Project and a person *)

type event =
  { level : level
  ; source : source
  ; entity : entity
  ; message : string
  }

(** Take a log_type and a message to log, print it to stdout in the standard logging
    format. *)
val log : level -> source -> entity -> string -> unit

val log' : event -> unit
val get_the_log : unit -> event Seq.t
val show_level : level -> string
val show_source : source -> string
val isFatal : event -> bool
val isError : event -> bool
val isWarning : event -> bool
val isInfo : event -> bool
val isDebug : event -> bool
val make_display_message : ?color:bool -> event -> string
val dump_event : event -> unit
val dump_the_log : unit -> unit
