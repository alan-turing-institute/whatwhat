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
  | Error' of int (** Prevents processing of some data  *)
  | Warning of int (** Likely to cause an error if not fixed *)
  | Info (** Information for end users *)
  | Debug (** Information for whatwhat developers *)

(** Which module where we processing when the problem arose *)
type source =
  | Forecast (** When ingesting Forecast data *)
  | ForecastRaw
  | Github (** When ingesting GitHub data *)
  | GithubMetadata
  | Schedule (** When merging Forecast and GitHub data *)

type entity =
  | RawForecastProject of string (** Project name *)
  | ForecastProject of int (** The project number *)
  | Project of int (** The project number *)
  | RawForecastPerson of string (** Person's name *)
  | ForecastPerson of string (** Email address *)
  | Person of string (** email address *)
  | RawForecastAssignment of int (** Assignment ID in Forecast *)
  | Assignment of (int * string) (** Pair of a Project and a person *)
  | Other

type event =
  { level : level
  ; source : source
  ; entity : entity
  ; message : string
  }

val log' : event -> unit
val get_the_log : unit -> event Seq.t
val show_source : source -> string
val isError : event -> bool
val isWarning : event -> bool
val isInfo : event -> bool
val isDebug : event -> bool

val pretty_print
  :  use_color:bool
  -> verbose:int
  -> suppressed_codes:level list
  -> restrict_issues:int list option
  -> unit
