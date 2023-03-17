open Cohttp

exception HttpError of string

(** Redefine date type (and provide a pretty-printing function in order to
    cleanly derive [show]. *)
type date = CalendarLib.Date.t
let pp_date pp date = CalendarLib.Printer.Date.fprint "%i" pp date

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

(** Parse a string containing 8 digits as a date (in YYYYMMDD format).
    Non-digit characters are ignored. *)
let parse_date s =
  let digits = s |> String.to_seq |> Seq.filter is_digit |> String.of_seq in
  let yyyy, mm, dd =
    match String.length digits with
    | 8 ->
      ( int_of_string (String.sub digits 0 4)
      , int_of_string (String.sub digits 4 2)
      , int_of_string (String.sub digits 6 2) )
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

(** Group adjacent elements of a list together using a predicate *)
let group_by (p : 'a -> 'a -> bool) (xs : 'a list) : 'a list list =
  let acc x grps =
    match grps with
    | [] -> [ [ x ] ]
    | grp :: rest ->
      if p x (List.hd grp) then (x :: grp) :: rest else [ x ] :: grp :: rest
  in
  List.fold_right acc xs []
;;

(** Sum a list *)
let rec sum (xs : float list) : float =
  match xs with
  | [] -> 0.
  | y :: ys -> y +. sum ys
;;

(** Roll a date back to a Monday. Leaves Mondays untouched. *)
let rollback_week (d : CalendarLib.Date.t) : CalendarLib.Date.t =
  let open CalendarLib.Date in
  match day_of_week d with
  | Mon -> d
  | Tue -> rem d (Period.day 1)
  | Wed -> rem d (Period.day 2)
  | Thu -> rem d (Period.day 3)
  | Fri -> rem d (Period.day 4)
  | Sat -> rem d (Period.day 5)
  | Sun -> rem d (Period.day 6)
;;

(** Roll a date forward to a Friday. Leaves Fridays untouched.
    If [with_weekend] is [true], then rolls forward to Sundays. *)
let rollforward_week ?(with_weekend = false) (d : CalendarLib.Date.t) : CalendarLib.Date.t =
  let open CalendarLib.Date in
  let days_to_friday, days_to_sunday =
    match day_of_week d with
    | Mon -> 4, 6
    | Tue -> 3, 5
    | Wed -> 2, 4
    | Thu -> 1, 3
    | Fri -> 0, 2
    | Sat -> 6, 1
    | Sun -> 5, 0
  in
  let days_to_add = if with_weekend then days_to_sunday else days_to_friday in
  add d (Period.day days_to_add)
