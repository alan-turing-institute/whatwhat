module Raw = GithubRaw

(* Re-exporting for convenience of modules that import this one. *)
let get_users = Raw.get_users

(* ---------------------------------------------------------------------- *)
(* METADATA PARSING ERROR LOGGING *)
type parseerror =
  | DateOutOfBoundsError
  | DateParsingError
  | ExtraFieldError
  | FieldTypeError
  | MissingCompulsoryFieldError
  | MissingOptionalFieldError
  | NoMetadataError
  | NullCompulsoryFieldError
  | NullOptionalFieldError
  | YamlError

(** Log an error given the error type, Github issue number, and explanatory message.*)
let log_parseerror (what : parseerror) (number : int) msg =
  let prefix = "Metadata Parsing (num: " ^ string_of_int number ^ "): " in
  let log_lvl =
    match what with
    | DateOutOfBoundsError -> Log.Error
    | DateParsingError -> Log.Error
    | ExtraFieldError -> Log.Error
    | FieldTypeError -> Log.Error
    | MissingCompulsoryFieldError -> Log.Error
    | MissingOptionalFieldError -> Log.Warning
    | NoMetadataError -> Log.Error
    | NullCompulsoryFieldError -> Log.Error
    | NullOptionalFieldError -> Log.Warning
    | YamlError -> Log.Error
  in
  let error_description =
    match what with
    | DateOutOfBoundsError -> "Date out of bounds: "
    | DateParsingError -> "Unparseable date field: "
    | ExtraFieldError -> "Unexpected field in metadata: "
    | FieldTypeError -> "Wrong YAML type for field: "
    | MissingCompulsoryFieldError -> "Missing compulsory field: "
    | MissingOptionalFieldError -> "Missing optional field: "
    | NoMetadataError -> "No metadata block found in issue body."
    | NullCompulsoryFieldError -> "Null or empty compulsory field: "
    | NullOptionalFieldError -> "Null or empty optional field: "
    | YamlError -> "Unable to parse metadata block as YAML: "
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

(* [field] represents fields in the metadata block. It only stores metadata about the
   metadata (teehee), related to how each should be parsed. *)
type field =
  { name : string
  ; optional : bool
      (* if False, the issue will not become a NowWhat project if this field does not exist *)
  ; checker_function : Yaml.value -> bool
      (* Function that takes the YAML field and return a boolean for
      whether it is of the correct YAML type. *)
  }

(* ---------------------------------------------------------------------- *)
(* METADATA PARSING & VALIDATION *)

let yaml_is_float (yaml : Yaml.value) =
  match yaml with
  | `Float _ -> true
  | _ -> false
;;

let yaml_is_string (yaml : Yaml.value) =
  match yaml with
  | `String _ -> true
  | _ -> false
;;

let metadata_fields =
  [ { name = "turing-project-code"; optional = true; checker_function = yaml_is_string }
  ; { name = "earliest-start-date"; optional = true; checker_function = yaml_is_string }
  ; { name = "latest-start-date"; optional = true; checker_function = yaml_is_string }
  ; { name = "latest-end-date"; optional = true; checker_function = yaml_is_string }
  ; { name = "FTE-months"; optional = true; checker_function = yaml_is_float }
  ; { name = "nominal-FTE-percent"; optional = true; checker_function = yaml_is_float }
  ; { name = "max-FTE-percent"; optional = true; checker_function = yaml_is_float }
  ; { name = "min-FTE-percent"; optional = true; checker_function = yaml_is_float }
  ]
;;

module StringSet = Set.Make (String)

let is_field_optional key =
  let field_data = List.find (fun x -> x.name = key) metadata_fields in
  field_data.optional
;;

let valid_keys = List.map (fun x -> x.name) metadata_fields |> StringSet.of_list

let compulsory_keys =
  List.filter_map (fun x -> if x.optional then None else Some x.name) metadata_fields
  |> StringSet.of_list
;;

let checker_function key =
  let field_data = List.find (fun x -> x.name = key) metadata_fields in
  field_data.checker_function
;;

(* -- *)

let parse_date_field_opt (n : int) (str : string) =
  let date_opt =
    try Utils.date_opt_of_string str with
    | CalendarLib.Date.Out_of_bounds ->
      log_parseerror DateOutOfBoundsError n str;
      None
  in
  let () = if date_opt == None then log_parseerror DateParsingError n str in
  date_opt
;;

(* Get the value of [key] from [yaml]. Log an error or warning if the field is missing,
   depending on whether this is a optional field, and return None. *)
let read_yaml_log_missing n yaml key optional =
  let value_opt = Yaml.Util.find_exn key yaml in
  let () =
    if value_opt = None
    then (
      let log_type =
        if optional then MissingOptionalFieldError else MissingCompulsoryFieldError
      in
      log_parseerror log_type n key)
  in
  value_opt
;;

(* Get a [Yaml.value] that is known to be of type [`Float]. Log errors or
   warnings and return [None] if the field is missing.

   This function assumes that we've already checked that the yaml object is of a
   dictionary type and that _if_ the field in question exists, it is a [`Float]. *)
let float_opt_of_yaml n yaml key =
  let optional = is_field_optional key in
  let value_opt = read_yaml_log_missing n yaml key optional in
  match value_opt with
  | None -> None
  | Some value_yaml -> Some (Yaml.Util.to_float_exn value_yaml)
;;

(* Parse a [Yaml.value] that is known to be of type [`String]. Log errors or
   warnings and return [None] if the field is missing.

   This function assumes that we've already checked that the yaml object is of a
   dictionary type and that _if_ the field in question exists, it is a [`String]. *)
let string_opt_of_yaml n yaml key =
  let optional = is_field_optional key in
  let value_opt = read_yaml_log_missing n yaml key optional in
  match value_opt with
  | None -> None
  | Some value_yaml ->
    let value = Yaml.Util.to_string_exn value_yaml in
    (match value with
     | "" | "null" | "N/A" ->
       let log_type =
         if optional then NullOptionalFieldError else NullCompulsoryFieldError
       in
       let () = log_parseerror log_type n key in
       None
     | _ -> Some value)
;;

(* Parse a [Yaml.value] that is known to be of type [`String] as a date. Log errors or
   warnings and return [None] if this can't be done.

   This function assumes that we've already checked that the yaml object is of a
   dictionary type and that _if_ the field in question exists, it is a [`String]. *)
let date_opt_of_yaml n yaml key =
  let value_opt = string_opt_of_yaml n yaml key in
  match value_opt with
  | None -> None
  | Some value -> parse_date_field_opt n value
;;

(* Check if a given yaml block has all the expected keys, and only the expected keys. If
   not, log errors and return false, otherwise return true. *)
let check_keys n yaml =
  (* We can use Yaml.Util.keys_exn because we've previously checked that this block is of
     dictionary type. *)
  let yaml_keys = Yaml.Util.keys_exn yaml |> StringSet.of_list in
  let extra_keys = StringSet.diff yaml_keys valid_keys in
  let missing_keys = StringSet.diff compulsory_keys yaml_keys in
  let () = StringSet.iter (log_parseerror ExtraFieldError n) extra_keys in
  let () = StringSet.iter (log_parseerror MissingCompulsoryFieldError n) missing_keys in
  StringSet.is_empty extra_keys && StringSet.is_empty missing_keys
;;

let check_value_type n yaml key =
  let checker = checker_function key in
  (* We know this key exists, so we can safely just Option.get the value. *)
  let value = Yaml.Util.find_exn key yaml |> Option.get in
  let result = checker value in
  let key_value_string = key ^ ", " ^ Yaml.to_string_exn value in
  let () = log_parseerror FieldTypeError n key_value_string in
  result
;;

(* Check that all the fields in [yaml] are of the expected type. Log errors if they are
   not, and return a boolean. This function assumes that we've already checked that there
   are no unwelcome extra keys in [yaml]. *)
let check_value_types n yaml =
  Yaml.Util.keys_exn yaml
  |> List.map (check_value_type n yaml)
  |> List.fold_left ( && ) true
;;

let parse_fields (n : int) (yaml : Yaml.value) =
  let keys_ok = check_keys n yaml in
  let value_types_ok = check_value_types n yaml in

  if keys_ok && value_types_ok
  then
    (* We now know that all compulsory keys are in the data, there are no extra keys, and
       all keys that are there are of the expected type. We can safely proceed to parse.
       *)
    Some
      { turing_project_code = string_opt_of_yaml n yaml "turing-project-code"
      ; earliest_start_date = date_opt_of_yaml n yaml "earliest-start-date"
      ; latest_start_date = date_opt_of_yaml n yaml "latest-start-date"
      ; latest_end_date = date_opt_of_yaml n yaml "latest-end-date"
      ; fte_months = float_opt_of_yaml n yaml "FTE-months"
      ; nominal_fte_percent = float_opt_of_yaml n yaml "nominal-FTE-percent"
      ; max_fte_percent = float_opt_of_yaml n yaml "max-FTE-percent"
      ; min_fte_percent = float_opt_of_yaml n yaml "min-FTE-percent"
      }
  else None
;;

let metadata_of_yaml (n : int) (y : string) =
  let mdata_result = Yaml.of_string y in
  match mdata_result with
  | Ok yaml ->
    (* Check that this yaml block is of the dictionary type, rather than a single value
     or a list. *)
    (match yaml with
     | `O _ -> parse_fields n yaml
     | _ ->
       let () = log_parseerror YamlError n "YAML block is not a dictionary" in
       None)
  | Error (`Msg err) ->
    let () = log_parseerror YamlError n err in
    None
;;

let parse_metadata (n : int) (body : string) =
  let x = Str.split (Str.regexp {|\+\+\+|}) body in
  match x with
  | [ top; rest ] ->
    let mdata = metadata_of_yaml n top in
    mdata, rest
  | _ ->
    let () = log_parseerror NoMetadataError n "" in
    None, body
;;

let validate_issue (issue : Raw.issue) =
  let metadata, body = parse_metadata issue.number issue.body in
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
