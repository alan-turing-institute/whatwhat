module Raw = GithubRaw

(* Re-exporting for convenience of modules that import this one. *)
let get_users = Raw.get_users

(* ---------------------------------------------------------------------- *)
(* METADATA PARSING ERROR LOGGING *)
type parseerror =
  | FieldError
  | FieldWarning
  | LengthWarning
  | LineError

(** Log an error given the error type, Github issue number, and explanatory message.*)
let log_parseerror (what : parseerror) (number : int) msg =
  let prefix = "Metadata Parsing (num: " ^ string_of_int number ^ "): " in
  let log_lvl =
    match what with
    | LengthWarning -> Log.Error
    | LineError -> Log.Error
    | FieldError -> Log.Error
    | FieldWarning -> Log.Warning
  in
  let error_description =
    match what with
    | LengthWarning -> " "
    | LineError -> "Unable to break line into (key, value) - "
    | FieldError -> "Unable to parse key "
    | FieldWarning -> "key "
  in
  Log.log log_lvl @@ prefix ^ error_description ^ msg
;;

(* ---------------------------------------------------------------------- *)
(* TYPES *)

type metadata =
  { turing_project_code : string option
  ; earliest_start_date : CalendarLib.Date.t option
       [@printer DatePrinter.pp_print_date_opt]
  ; latest_start_date : CalendarLib.Date.t option [@printer DatePrinter.pp_print_date_opt]
  ; latest_end_date : CalendarLib.Date.t option [@printer DatePrinter.pp_print_date_opt]
  ; fte_months : float option
  ; nominal_fte_percent : float option
  ; max_fte_percent : float option
  ; min_fte_percent : float option
  }
[@@deriving show]

type person = GithubRaw.person =
  { login : string
  ; name : string option
  ; email : string option
  }
[@@deriving show]

type project =
  { number : int
  ; title : string
  ; body : string
  ; state : string
  ; assignees : person list
  ; reactions : (string * person) list
  ; column : string option
  ; metadata : metadata
  }
[@@deriving show]

type field =
  { name : string
  ; optional : bool
      (* if False, the issue will not become a NowWhat project if this field does not exist *)
  }

(* This is useful for knowing what is a key field.  *)
let metadata_fields =
  [ { name = "turing-project-code"; optional = true }
  ; { name = "earliest-start-date"; optional = true }
  ; { name = "latest-start-date"; optional = true }
  ; { name = "latest-end-date"; optional = true }
  ; { name = "FTE-months"; optional = true }
  ; { name = "nominal-FTE-percent"; optional = true }
  ; { name = "max-FTE-percent"; optional = true }
  ; { name = "min-FTE-percent"; optional = true }
  ]
;;

(* ---------------------------------------------------------------------- *)
(* METADATA PARSING & VALIDATION *)

(* --- field parsing functions ---- *)

let date_from_string (n : int) (str : string) =
  let catch_date_exn year month day n =
    try Some (CalendarLib.Date.make year month day) with
    | CalendarLib.Date.Out_of_bounds ->
      log_parseerror FieldError n "date out of bounds";
      None
  in
  let datelist = Str.split (Str.regexp {|-|}) str in
  match datelist with
  | [ year; month; day ] ->
    catch_date_exn (int_of_string year) (int_of_string month) (int_of_string day) n
  | _ ->
    log_parseerror FieldError n ("unable to make date from " ^ str);
    None
;;

let date_from_string_opt (n : int) (str : string option) =
  match str with
  | None -> None
  | Some x -> date_from_string n x
;;

let float_opt_of_string_opt (x : string option) =
  match x with
  | Some "null" ->
    None
    (* not sure why there are still nulls at this stage...should be converted to empty strings in check_value? *)
  | Some "" -> None
  | None -> None
  | Some y -> float_of_string_opt y
;;

(* the value should be of the form " <val>". Any other spaces then content indicates a violation*)
(* at this stage an empty string to indicate no value*)
let check_value (n : int) (key : string) (value : string) =
  let x = Str.bounded_split (Str.regexp " +") value 2 in
  match x with
  | [] ->
    log_parseerror FieldWarning n (key ^ ", empty value");
    ""
  | "null" :: [] ->
    log_parseerror FieldWarning n (key ^ ", null value");
    ""
  | y :: [] -> y
  | y :: z ->
    let info = String.concat "" z in
    let () = log_parseerror FieldWarning n (key ^ ", additional info - " ^ info) in
    y
;;

let list_to_pair (n : int) (x : string list) =
  match x with
  | [ key; value ] -> Some (key, check_value n key value)
  | [ key ] ->
    log_parseerror FieldWarning n (key ^ ", empty value");
    Some (key, "")
  | y ->
    log_parseerror LineError n (String.concat ";" y);
    None
;;

(* ---  *)

let key_exists n yaml_fields mfield =
  if mfield.optional
  then true
  else (
    match List.assoc_opt mfield.name yaml_fields with
    | None ->
      log_parseerror FieldWarning n ("missing key " ^ mfield.name);
      false (* either it doesn't exist or the value is "" *)
    | Some "" -> false
    | Some _ -> true)
;;

let parse_fields (n : int) (lines : string list) =
  (* we now know if the value exists *)
  let fields =
    List.filter_map (fun x -> Str.split (Str.regexp {|:|}) x |> list_to_pair n) lines
  in

  (* we now know if the essential keys exist *)
  let contains_key_fields = List.for_all (key_exists n fields) metadata_fields in
  if contains_key_fields
  then
    (* parse data *)
    Some
      { turing_project_code = fields |> List.assoc_opt "turing-project-code"
      ; earliest_start_date =
          fields |> List.assoc_opt "earliest-start-date" |> date_from_string_opt n
      ; latest_start_date =
          fields |> List.assoc_opt "latest-start-date" |> date_from_string_opt n
      ; latest_end_date =
          fields |> List.assoc_opt "latest-end-date" |> date_from_string_opt n
      ; fte_months = fields |> List.assoc_opt "FTE-months" |> float_opt_of_string_opt
      ; nominal_fte_percent =
          fields |> List.assoc_opt "nominal-FTE-percent" |> float_opt_of_string_opt
      ; max_fte_percent =
          fields |> List.assoc_opt "max-FTE-percent" |> float_opt_of_string_opt
      ; min_fte_percent =
          fields |> List.assoc_opt "min-FTE-percent" |> float_opt_of_string_opt
      }
  else (
    let () = log_parseerror FieldError n "Essential fields missing" in
    None)
;;

let metadata_of_yaml (n : int) (y : string) =
  let lines = Str.split (Str.regexp "\r\n") y in
  let lenlines = List.length lines in
  let lenfields = List.length metadata_fields in
  if lenlines != lenfields
  then
    log_parseerror
      LengthWarning
      n
      ("length of yaml is "
      ^ string_of_int lenlines
      ^ " but length of expected metadata fields is "
      ^ string_of_int lenfields);
  parse_fields n lines
;;

let parse_metadata (n : int) (body : string) =
  let x = Str.split (Str.regexp {|\+\+\+|}) body in
  match x with
  | [ top; rest ] ->
    let mdata = metadata_of_yaml n top in
    mdata, rest
  | _ -> None, body
;;

let validate_issue (issue : Raw.issue) =
  let metadata, body = issue.body |> parse_metadata issue.number in
  match metadata with
  | None -> None
  | Some x ->
    Some
      { number = issue.number
      ; title = issue.title
      ; body
      ; state = issue.state
      ; assignees = issue.assignees
      ; reactions = issue.reactions
      ; column = issue.column
      ; metadata = x
      }
;;

let get_project_issues (project_name : string) =
  let issues = Raw.get_project_issues project_name in
  Printf.printf "Obtained %d Github issues\n" (List.length issues);
  List.filter_map validate_issue issues
;;
