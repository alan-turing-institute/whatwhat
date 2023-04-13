open Cohttp

exception HttpError of string
exception GithubRateLimitError of string

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

(** Raise an appropriate error if the API request response indicates failure. *)
let check_http_response response =
  let show_time (t : Unix.tm) : string =
    Printf.sprintf "%02d:%02d:%02d" t.Unix.tm_hour t.Unix.tm_min t.Unix.tm_sec
  in
  let headers = Response.headers response in
  (* Check GitHub rate limit *)
  (match
     Header.get headers "x-ratelimit-remaining", Header.get headers "x-ratelimit-reset"
   with
   | Some "0", Some s ->
     let time = Unix.localtime (float_of_string s) in
     raise @@ GithubRateLimitError (show_time time)
   | _ -> ());
  (* Check other HTTP problems *)
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
  match String.length digits with
  | 8 ->
    let yyyy, mm, dd =
      ( int_of_string (String.sub digits 0 4)
      , int_of_string (String.sub digits 4 2)
      , int_of_string (String.sub digits 6 2) )
    in
    let open CalendarLib.Date in
    if is_valid_date yyyy mm dd
    then Ok (make yyyy mm dd)
    else Error (`Msg ("Date '" ^ s ^ "' is not valid!"))
  | _ -> Error (`Msg ("Unrecognised date: " ^ s))
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

(** Sort and then group elements of a list according to a key *)
let sort_and_group_by (f : 'a -> 'key) (xs : 'a list) : ('key * 'a list) list =
  match xs with
  | [] -> []
  | _ ->
    xs
    |> List.map (fun x -> f x, x)
    |> List.sort (fun x1 x2 -> compare (fst x1) (fst x2))
    |> group_by (fun x1 x2 -> fst x1 = fst x2)
    |> List.map (fun ps ->
         match ps with
         | p :: _ -> fst p, List.map snd ps
         | _ -> failwith "group_by on nonempty input should not give empty lists")
;;

(** [splitAt n xs] returns [(take n xs, drop n xs)] (i.e. the first [n] elements
    and the rest). *)
let rec splitAt n xs =
  match n, xs with
  | _, [] -> [], []
  | 1, x :: x' -> [ x ], x'
  | n, x :: x' ->
    let y1, y2 = splitAt (n - 1) x' in
    x :: y1, y2
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
let rollforward_week ?(with_weekend = false) (d : CalendarLib.Date.t) : CalendarLib.Date.t
  =
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
;;

(** [all_throttled] is like [Lwt.all] but only resolves [max_concurrent]
    promises at a time. *)
let rec all_throttled ?(max_concurrent = 150) (reqs : 'a Lwt.t list) =
  let open Lwt.Syntax in
  match splitAt max_concurrent reqs with
  | [], [] -> Lwt.return []
  | xs, [] -> Lwt.all xs
  | xs, ys ->
    let* first_batch = Lwt.all xs in
    let* the_rest = all_throttled ~max_concurrent ys in
    Lwt.return (first_batch @ the_rest)
;;

(** Escape a string such that characters show up correctly in (GitHub-Flavoured)
    Markdown. Note: this means that formatting etc. will be lost in the
    resulting post, and it'll purely appear as a string. *)
let gfm_escape s =
  let b = Buffer.create 80 in
  String.iter
    (fun c ->
      match c with
      (* This list of replacements is probably not exhaustive. *)
      | '<' -> Buffer.add_string b {|\<|}
      | '>' -> Buffer.add_string b {|\>|}
      | '*' -> Buffer.add_string b {|\*|}
      | '_' -> Buffer.add_string b {|\_|}
      | '#' -> Buffer.add_string b {|\#|}
      | '`' -> Buffer.add_string b {|\`|}
      | '\\' -> Buffer.add_string b {|\\|}
      | _ -> Buffer.add_char b c)
    s;
  Buffer.contents b
;;

(** Get the maximum of a list using a key function *)
let rec max_by ~(default : 'b) (f : 'a -> 'b) (xs : 'a list) : 'b =
  match xs with
  | [] -> default
  | x :: xs -> max (f x) (max_by ~default f xs)
;;

(** Check if s2 is a substring of s1 *)
let contains ?(case_sensitive = true) s1 s2 =
  let re =
    if case_sensitive then Str.regexp_string s2 else Str.regexp_string_case_fold s2
  in
  try
    ignore (Str.search_forward re s1 0);
    true
  with
  | Not_found -> false
;;

(** Take the first n elements of a list *)
let rec take n xs =
  match n, xs with
  | _, [] -> []
  | 1, x :: _ -> [ x ]
  | n, x :: xs -> x :: take (n - 1) xs
;;

(** Drop the first n elements of a list *)
let rec drop n xs =
  match n, xs with
  | _, [] -> []
  | 1, _ :: xs -> xs
  | n, _ :: xs -> drop (n - 1) xs
;;

(** Split a list after n elements. Effectively, [split_at n xs] returns [(take n
    xs, drop n xs)]. *)
let rec split_at (n : int) (xs : 'a list) : 'a list * 'a list =
  match n, xs with
  | _, [] -> [], []
  | 1, x :: xs -> [ x ], xs
  | n, x :: xs ->
    let ys, zs = split_at (n - 1) xs in
    x :: ys, zs
;;

(** Transpose a list of lists *)
let transpose (ls : 'a list list) : 'a list list =
  let rec transpose_rec acc = function
    (* Finished going through lists. Just reverse the accumulator *)
    | [] -> List.rev acc
    | [] :: _ -> List.rev acc
    (* Cons the first element of each list onto the accumulator *)
    | ls -> transpose_rec (List.map List.hd ls :: acc) (List.map List.tl ls)
  in
  transpose_rec [] ls
;;
