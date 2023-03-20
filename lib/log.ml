(** Utilities for logging issues during the course of ingesting the data for 
    later summary and notification 
 *)

exception FatalErrorRaised

type level =
  | Fatal of int
  | Error of int
  | Warning of int
  | Info
  | Debug
[@@deriving ord]

type source =
  | Forecast
  | ForecastRaw
  | Github
  | GithubMetadata
  | Schedule

type entity =
  | RawForecastProject of string
  | Project of int
  | RawForecastPerson of string
  | Person of string
  | RawForecastAssignment of int
  | Assignment of (int * string)
  | Other

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

let log' ev =
  Stack.push ev the_log;
  match ev.level with
  | Fatal _ -> raise FatalErrorRaised
  | _ -> ()
;;

let get_the_log () = Stack.to_seq the_log

let show_source = function
  | Forecast -> "Forecast"
  | ForecastRaw -> "ForecastRaw"
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

let isFatal e =
  match e.level with
  | Fatal _ -> true
  | _ -> false
;;

let isError e =
  match e.level with
  | Error _ -> true
  | _ -> false
;;

let isWarning e =
  match e.level with
  | Warning _ -> true
  | _ -> false
;;

let isInfo e =
  match e.level with
  | Info -> true
  | _ -> false
;;

let isDebug e =
  match e.level with
  | Debug -> true
  | _ -> false
;;

(* Printing --------------------------------------------------- *)

(* TODO implement this function properly *)
let extract_issue_number event =
  match event.entity with
  | RawForecastProject _ -> None
  | Project n -> Some n
  | RawForecastPerson _ -> None
  | Person _ -> None
  | RawForecastAssignment _ -> None
  | Assignment _ -> None
  | Other -> None
;;

let pretty_print_event ~use_color e =
  let open ANSITerminal in
  let color styles = if use_color then styles else [] in
  let error_code =
    match e.level with
    | Fatal n -> sprintf (color [ Bold; Foreground Red ]) "F%d" n
    | Error n -> sprintf (color [ Foreground Red ]) "E%d" n
    | Warning n -> sprintf (color [ Foreground Yellow ]) "W%d" n
    | Info -> "I"
    | Debug -> "D"
  in
  let header =
    match extract_issue_number e with
    | Some i -> sprintf (color [ Bold ]) "Issue %-5d" i
    | None -> sprintf (color [ Bold ]) "Something else"
  in
  Printf.printf "%s %s %s\n" header error_code e.message
;;

let pretty_print ~use_color =
  let color styles = if use_color then styles else [] in

  let compare_events e1 e2 =
    (* Compare on issue number first, then error code *)
    match
      match extract_issue_number e1, extract_issue_number e2 with
      | None, None -> 0
      | None, Some _ -> -1
      | Some _, None -> 1
      | Some x, Some y -> Stdlib.compare x y
    with
    | 0 -> compare_level e1.level e2.level
    | n -> n
  in
  let fatal, nonfatal = List.partition isFatal (the_log |> Stack.to_seq |> List.of_seq) in

  (* Print errors, warnings, etc. *)
  nonfatal |> List.stable_sort compare_events |> List.iter (pretty_print_event ~use_color);

  (* Print fatal errors *)
  let open ANSITerminal in
  match fatal with
  | [] -> ()
  | fs ->
    printf (color [ Bold ]) "\nwhatwhat encountered the following fatal error(s):\n";
    List.iter (pretty_print_event ~use_color) fs
;;
