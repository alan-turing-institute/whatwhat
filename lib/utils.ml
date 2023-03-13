open Cohttp

exception HttpError of string

(** Parse a string as a date in the format year-month-day. If the string is not in this
    format, return [Error ()], else [Ok date]. *)
let date_of_string (str : string) =
  let datelist = Str.split (Str.regexp {|-|}) str in
  match datelist with
  | [ year; month; day ] ->
    (try
       Ok
         (CalendarLib.Date.make
            (int_of_string year)
            (int_of_string month)
            (int_of_string day))
     with
     | _ -> Error ())
  | _ -> Error ()
;;

(** Raise a HttpError if the API request response indicates failure. *)
let check_http_response response =
  if Response.status response |> Code.code_of_status |> Code.is_success |> not
  then (
    let code_string = Response.status response |> Code.string_of_status in
    raise @@ HttpError code_string)
;;

(** Print an option type. *)
let show_opt str_opt =
  match str_opt with
  | None -> "None"
  | Some s -> "Some " ^ s
;;

(** Check if a character is a digit, i.e. [0-9] **)
let is_digit c =
  match c with
  | '0' .. '9' -> true
  | _ -> false
;;

(** Parse a string as a date. The string must contain either 8 digits, in which
 case it is treated as YYYYMMDD, or 6, in which case it is treated as YYMMDD.
 Non-digit characters are ignored. *)
let parse_date s =
  let digits = s |> String.to_seq |> Seq.filter is_digit |> String.of_seq in
  let yyyy, mm, dd =
    match String.length digits with
    | 8 ->
      ( int_of_string (String.sub digits 0 4)
      , int_of_string (String.sub digits 4 2)
      , int_of_string (String.sub digits 6 2) )
    | 6 ->
      ( 2000 + int_of_string (String.sub digits 0 2)
      , int_of_string (String.sub digits 2 2)
      , int_of_string (String.sub digits 4 2) )
    | _ -> failwith ("Unrecognised date: " ^ s)
  in
  let open CalendarLib.Date in
  if is_valid_date yyyy mm dd
  then make yyyy mm dd
  else failwith ("Date '" ^ s ^ "' is not valid!")
;;

(** Calculate the default start date for Forecast export, which is the 1st of the
    previous month (relative to today's date). The [relative_to] argument can be
    specified to allow for unit testing. *)
let default_start_date ?relative_to () =
  let open CalendarLib.Date in
  let d =
    match relative_to with
    | Some dt -> dt
    | None -> today ()
  in
  match int_of_month (month d) with
  | 1 -> make (year d - 1) 12 1
  | n -> make (year d) (n - 1) 1
;;

(** Calculate the default end date for Forecast export, which is the last day of
    the next month (relative to today's date). The [relative_to] argument can be
    specified to allow for unit testing. *)
let default_end_date ?relative_to () =
  let open CalendarLib.Date in
  let d =
    match relative_to with
    | Some dt -> dt
    | None -> today ()
  in
  let one_day_over =
    match int_of_month (month d) with
    | 11 -> make (year d + 1) 1 1
    | 12 -> make (year d + 1) 2 1
    | n -> make (year d) (n + 2) 1
  in
  rem one_day_over (Period.day 1)
;;
