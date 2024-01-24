open Cohttp
open CalendarLib

(** --- Stdlib shims ------------ *)

let is_digit c =
  match c with
  | '0' .. '9' -> true
  | _ -> false
;;

let is_hex_digit c =
  match c with
  | '0' .. '9' -> true
  | 'a' .. 'f' -> true
  | _ -> false
;;

let range i j = List.init (j - i) (( + ) i)

(* This is not a great implementation ... *) 
let group_by (p : 'a -> 'a -> bool) (xs : 'a list) : 'a list list =
  let rec acc x grps =
    match grps with
    | [] -> [ [ x ] ]
    | grp :: rest ->
       if p x (List.hd grp) then
         (x :: grp) :: rest else grp :: (acc x rest)
  in
  List.fold_right acc xs []
;;

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

let rec take n xs =
  match n, xs with
  | _, [] -> []
  | 1, x :: _ -> [ x ]
  | n, x :: xs -> x :: take (n - 1) xs
;;

let rec drop n xs =
  match n, xs with
  | _, [] -> []
  | 1, _ :: xs -> xs
  | n, _ :: xs -> drop (n - 1) xs
;;

let rec split_at n xs =
  match n, xs with
  | _, [] -> [], []
  | 1, x :: x' -> [ x ], x'
  | n, x :: x' ->
    let y1, y2 = split_at (n - 1) x' in
    x :: y1, y2
;;

let rec sum (xs : float list) : float =
  match xs with
  | [] -> 0.
  | y :: ys -> y +. sum ys
;;

let rec max_by ~(default : 'b) (f : 'a -> 'b) (xs : 'a list) : 'b =
  match xs with
  | [] -> default
  | x :: xs -> max (f x) (max_by ~default f xs)
;;

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

(** Attempt to read text from a file. Returns [None] for various errors (e.g.
    if the file doesn't exist). *)
let read_file (filename : string) : string option =
  try Some (In_channel.with_open_bin filename In_channel.input_all) with
  | Sys_error _ -> None
;;

(** Print an option type. *)
let show_opt str_opt =
  match str_opt with
  | None -> "None"
  | Some s -> "Some " ^ s
;;

(** --- HTTP exceptions --------- *)

exception HttpError of string
exception GithubRateLimitError of string

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

(** --- Dates ------------------- *)

type date = Date.t

let pp_date pp date = Printer.Date.fprint "%i" pp date
let show_date date = Printer.Date.sprint "%i" date

let date_of_string ?(lax = false) (str : string) : (date, [> `Msg of string ]) result =
  if lax
  then (
    let digits = str |> String.to_seq |> Seq.filter is_digit |> String.of_seq in
    match String.length digits with
    | 8 ->
      let yyyy, mm, dd =
        ( int_of_string (String.sub digits 0 4)
        , int_of_string (String.sub digits 4 2)
        , int_of_string (String.sub digits 6 2) )
      in
      let open Date in
      if is_valid_date yyyy mm dd
      then Ok (make yyyy mm dd)
      else Error (`Msg ("Date '" ^ str ^ "' is not valid!"))
    | _ -> Error (`Msg ("Unrecognised date: " ^ str)))
  else (
    let datelist = Str.split (Str.regexp {|-|}) str in
    match datelist with
    | [ year; month; day ] ->
      (try
         Ok (Date.make (int_of_string year) (int_of_string month) (int_of_string day))
       with
       | _ -> Error (`Msg ("Date '" ^ str ^ "' is not valid!")))
    | _ -> Error (`Msg ("Unrecognised date: " ^ str)))
;;

let default_start_date ?relative_to () =
  let open Date in
  let d =
    match relative_to with
    | Some dt -> dt
    | None -> today ()
  in
  match int_of_month (month d) with
  | 1 -> make (year d - 1) 12 1
  | n -> make (year d) (n - 1) 1
;;

let default_end_date ?relative_to () =
  let open Date in
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

let rollback_week (d : Date.t) : Date.t =
  let open Date in
  match day_of_week d with
  | Mon -> d
  | Tue -> rem d (Period.day 1)
  | Wed -> rem d (Period.day 2)
  | Thu -> rem d (Period.day 3)
  | Fri -> rem d (Period.day 4)
  | Sat -> rem d (Period.day 5)
  | Sun -> rem d (Period.day 6)
;;

let rollforward_week ?(with_weekend = false) (d : Date.t) : Date.t =
  let open Date in
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

let get_xdays_between ~(day : Date.day) ~(start_date : Date.t) ~(end_date : Date.t)
  : Date.t list
  =
  let rec loop acc d =
    if d > end_date
    then acc
    else if Date.day_of_week d = day
    then loop (d :: acc) (Date.next d `Week)
    else loop acc (Date.next d `Day)
  in
  loop [] start_date |> List.rev
;;

let get_turing_weeks_in_month (month : [> `Year | `Month ] Date.date) =
  let first_of_month =
    Date.make (Date.year month) (Date.month month |> Date.int_of_month) 1
  in
  let last_of_month = Date.prev (Date.next first_of_month `Month) `Day in
  get_xdays_between ~day:Thu ~start_date:first_of_month ~end_date:last_of_month
  |> List.map rollback_week
;;

let get_weekdays_in_week (d : Date.t) : Date.t list =
  let monday = rollback_week d in
  List.init 5 (fun i -> Date.add monday (Date.Period.day i))
;;

let show_month = function
  | 1 -> "Jan"
  | 2 -> "Feb"
  | 3 -> "Mar"
  | 4 -> "Apr"
  | 5 -> "May"
  | 6 -> "Jun"
  | 7 -> "Jul"
  | 8 -> "Aug"
  | 9 -> "Sep"
  | 10 -> "Oct"
  | 11 -> "Nov"
  | 12 -> "Dec"
  | _ -> failwith "Invalid month."
;;

(** --- Miscellaneous ----------- *)

let rec all_throttled ?(max_concurrent = 150) (reqs : 'a Lwt.t list) =
  let open Lwt.Syntax in
  match split_at max_concurrent reqs with
  | [], [] -> Lwt.return []
  | xs, [] -> Lwt.all xs
  | xs, ys ->
    let* first_batch = Lwt.all xs in
    let* the_rest = all_throttled ~max_concurrent ys in
    Lwt.return (first_batch @ the_rest)
;;

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

let dequote (s : string) : string =
  let len = String.length s in
  if len < 2
  then s
  else if s.[0] = '"' && s.[len - 1] = '"'
  then String.sub s 1 (len - 2)
  else s
;;
