(** Logging: Record problems for later notification

    This logging system is very specific to WhatWhat
    
    A logged message consits of:
    - The level  -- how serious is the problem
    - The source -- which part of whatwhat were we executing
    - The origin -- What caused the problem
    - The entity -- which entity was affected 

 *) 
   
type level =
  | Error   (** Prevents processing of some data  *) 
  | Warning (** Likely to cause an error if not fixed *)
  | Info    (** Information for end users *)
  | Debug   (** Information for whatwhat developers *)
(** Severity of logged problem *)

type source = 
  | Forecast (** When ingesting Forecast data *)
  | GitHub   (** When ingesting GitHub data *) 
  | GitHubMetadata 
  | Schedule (** When merging Forecast and GitHub data *)
(** Which module where we processing when the problem arose *)

type entity =
  | RawForecastProject of string    (** Project name *)
  | Project of int                  (** The project code *)
  | RawForecastPerson of string     (** Person's name *)
  | Person of string                (** email address *)
  | RawForecastAssignment
  | Assignment of (int * string)    (** Pair of a Project and a person *)

type event = {
    level : level;
    source : source;
    entity : entity;
    message : string
  }


val log : level -> source -> entity -> string -> unit
(** Take a log_type and a message to log, print it to stdout in the standard logging
    format. *)

val get_the_log : unit -> event Seq.t
val show_level  : level -> string
val show_source : source -> string
