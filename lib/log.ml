(** Utilities for logging issues during the course of ingesting the data for 
    later summary and notification 
 *)

(* We need [Error'] here instead of just [Error], because of an unfortunate bug
   in ppx_deriving: https://github.com/ocaml-ppx/ppx_deriving/issues/254 *)
type level =
  | Error' of int
  | Warning of int
  | Info
  | Debug
[@@deriving ord]

let show_level = function
  | Error' x -> "E" ^ string_of_int x
  | Warning x -> "W" ^ string_of_int x
  | Info -> "INFO"
  | Debug -> "DEBUG"
;;

type entity =
  | RawForecastProject of string
  | ForecastProject of int
  | Project of int
  | RawForecastPlaceholder of string
  | RawForecastPerson of string
  | ForecastPerson of string
  | Person of string
  | RawForecastAssignment of int
  | Assignment of (int * string)
  | Other

type event =
  { level : level
  ; entity : entity
  ; message : string
  }

(* ------------------------------------------------------------ *)

(* The log is a mutable stack of events *)
let the_log : event Stack.t = Stack.create ()

(* Interface -------------------------------------------------- *)

let log' ev = Stack.push ev the_log
let get_the_log () = Stack.to_seq the_log

let isError e =
  match e.level with
  | Error' _ -> true
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
  | ForecastProject n -> Some n
  | Project n -> Some n
  | _ -> None
;;

(* TODO implement this function properly *)
let extract_source event =
  match event.entity with
  | RawForecastProject _ -> "FCRaw:Project"
  | ForecastProject n -> Printf.sprintf "Issue %-5d" n
  | Project n -> Printf.sprintf "Issue %-5d" n
  | RawForecastPlaceholder _ -> "FCRaw:Placeholder"
  | RawForecastPerson _ -> "FCRaw:Person"
  | ForecastPerson _ -> "Person"
  | Person _ -> "Person"
  | RawForecastAssignment _ -> "FCRaw:Assignment"
  | Assignment _ -> "Assignment"
  | Other -> "Other"
;;

(* A type which determines whether specific error codes are to be suppressed or
   filtered for. *)
type code_spec =
  | Without of level list
  | Only of level list
  | All

let should_be_shown ~verbose ~restrict_codes ~restrict_issues (issue_number, event) =
  (* Check if the issue number is in restrict_issues *)
  let pred1 =
    match restrict_issues, issue_number with
    | None, _ -> true
    | Some _, None -> false
    | Some numbers, Some n -> List.mem n numbers
  in
  (* Check that the error code is consistent with restrict_codes *)
  let pred2 =
    match restrict_codes with
    | All -> true
    | Only cds -> List.mem event.level cds
    | Without cds -> not (List.mem event.level cds)
  in
  (* Check against the desired level of verbosity *)
  let pred3 =
    match event.level with
    | Error' _ -> true
    | Warning _ -> true
    | Info -> verbose >= 1
    | Debug -> verbose >= 2
  in
  pred1 && pred2 && pred3
;;

let pretty_print_event ~use_color (_, e) =
  let open ANSITerminal in
  let header = Printf.sprintf "%-20s" (extract_source e) in
  let error_code, error_style =
    match e.level with
    | Error' n -> Printf.sprintf "E%d" n, [ Foreground Red ]
    | Warning n -> Printf.sprintf "W%d" n, [ Foreground Yellow ]
    | Info -> "INFO ", []
    | Debug -> "DEBUG", []
  in

  Utils.prcol ~use_color [ Bold ] header;
  Printf.printf " ";
  Utils.prcol ~use_color error_style error_code;
  Printf.printf " ";
  Printf.printf "%s\n" e.message
;;

let gather_events ~verbose ~restrict_codes ~restrict_issues : (int option * event) list =
  let compare_events (n1, e1) (n2, e2) =
    (* Compare on issue number first, then error code *)
    let issue_number_cmp =
      match n1, n2 with
      | None, None -> 0
      | None, Some _ -> -1
      | Some _, None -> 1
      | Some x, Some y -> Stdlib.compare x y
    in
    match issue_number_cmp with
    | 0 -> compare_level e1.level e2.level
    | n -> n
  in
  the_log
  |> Stack.to_seq
  |> Seq.map (fun e -> extract_issue_number e, e)
  |> Seq.filter (should_be_shown ~verbose ~restrict_codes ~restrict_issues)
  |> List.of_seq
  |> List.stable_sort compare_events
;;

let gather_events' ~verbose ~restrict_codes ~restrict_issues
  : (int option * event list) list
  =
  gather_events ~verbose ~restrict_codes ~restrict_issues
  |> Utils.group_by (fun (i1, _) (i2, _) -> i1 = i2)
  |> List.map (fun pairs -> fst (List.hd pairs), List.map snd pairs)
;;

let pretty_print ~use_color ~verbose ~restrict_codes ~restrict_issues =
  gather_events ~verbose ~restrict_codes ~restrict_issues
  |> List.iter (pretty_print_event ~use_color)
;;
