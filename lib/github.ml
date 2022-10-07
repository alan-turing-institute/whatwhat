module Raw = GithubRaw

(* Whatwhat doesn't care about all projects: only those in the folowing stages:
   - Finding people
   - Awaiting start
   - Active
 *)
let is_valid_column = function
  | Some "Finding people" -> true
  | Some "Awaiting start" -> true
  | Some "Active" -> true
  | _ -> false
;;

(* Re-exporting for convenience of modules that import this one. *)
let get_users = Raw.get_users

(* ---------------------------------------------------------------------- *)
(* METADATA PARSING ERROR LOGGING *)
type parseerror =
  | DateOutOfBoundsError
  | DateParsingError
  | ExtraFieldError
  | FieldTypeError
  | FTETimeUnderSpecifiedError
  | FTETimeOverSpecifiedError
  | MissingCompulsoryFieldError
  | MissingOptionalFieldError
  | NoMetadataError
  (* | NullCompulsoryFieldError *)
  (* | NullOptionalFieldError *)
  | YamlError

(** Log an error given the error type, Github issue number, and explanatory message.*)
let log_parseerror (what : parseerror) (number : int) msg =
  let log_lvl =
    match what with
    | DateOutOfBoundsError -> Log.Error
    | DateParsingError -> Log.Error
    | ExtraFieldError -> Log.Error
    | FieldTypeError -> Log.Error
    | FTETimeUnderSpecifiedError -> Log.Error
    | FTETimeOverSpecifiedError -> Log.Error
    | MissingCompulsoryFieldError -> Log.Error
    | MissingOptionalFieldError -> Log.Warning
    | NoMetadataError -> Log.Error
    (* | NullCompulsoryFieldError -> Log.Error *)
    (* | NullOptionalFieldError -> Log.Warning *)
    | YamlError -> Log.Error
  in
  let error_description =
    match what with
    | DateOutOfBoundsError -> "Date out of bounds: "
    | DateParsingError -> "Unparseable date field: "
    | ExtraFieldError -> "Unexpected field in metadata: "
    | FieldTypeError -> "Wrong YAML type for field: "
    | FTETimeUnderSpecifiedError -> "Neither FTE-months nor FTE-weeks specified"
    | FTETimeOverSpecifiedError -> "Both FTE-months and FTE-weeks specified"
    | MissingCompulsoryFieldError -> "Missing field: "
    | MissingOptionalFieldError -> "Missing optional field (assuming null): "
    | NoMetadataError -> "No metadata block found in issue body."
    (* | NullCompulsoryFieldError -> "Null or empty compulsory field: " *)
    (* | NullOptionalFieldError -> "Null or empty optional field: " *)
    | YamlError -> "Unable to parse metadata block as YAML: "
  in
  Log.log log_lvl Log.GithubMetadata (Log.Project number) @@ error_description ^ msg
;;

(* ---------------------------------------------------------------------- *)
(* TYPES *)

type metadata =
  { turing_project_code : string list option
  ; earliest_start_date : CalendarLib.Date.t option
       [@printer DatePrinter.pp_print_date_opt]
  ; latest_start_date : CalendarLib.Date.t option [@printer DatePrinter.pp_print_date_opt]
  ; latest_end_date : CalendarLib.Date.t option [@printer DatePrinter.pp_print_date_opt]
  ; max_fte_percent : float option
  ; min_fte_percent : float option
  ; nominal_fte_percent : float option
  ; fte_months : float option
  ; fte_weeks : float option
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
  }

(* ---------------------------------------------------------------------- *)
(* METADATA PARSING & VALIDATION *)

(* This list defines which keys are valid in the metadata block, and whether they are
   optional or compulsory. *)
let metadata_fields =
  [ { name = "turing-project-code"; optional = true }
  ; { name = "earliest-start-date"; optional = true }
  ; { name = "latest-start-date"; optional = false }
  ; { name = "latest-end-date"; optional = true }
  ; { name = "max-FTE-percent"; optional = true }
  ; { name = "min-FTE-percent"; optional = true }
  ; { name = "nominal-FTE-percent"; optional = false }
  ; { name = "FTE-months"; optional = true }
  ; { name = "FTE-weeks"; optional = true }
  ]
;;

module StringSet = Set.Make (String)

let is_field_optional key =
  let field_data = List.find (fun x -> x.name = key) metadata_fields in
  field_data.optional
;;

let valid_keys = List.map (fun x -> x.name) metadata_fields |> StringSet.of_list

(* -- *)

let log_missing_field n key optional =
  if not optional then log_parseerror MissingCompulsoryFieldError n key
;;

(* Get a [Yaml.value] that is expected to be of type [`Float]. Return [Ok Some float] if
   it is found, [Ok None] if it is missing/null and the field is optional, or [Error ()]
   if it is missing/null and the field is compulsory or the value is malformed.

   If the field is not found or is null or malformed, either a warning or an error is
   logged explaining the issue.
 
   The [yaml] block is assumed to be of dictionary type. *)
let read_float_field n yaml key =
  let optional = is_field_optional key in
  let value_opt = Yaml.Util.find_exn key yaml in
  match value_opt with
  | None | Some `Null | Some (`String "") | Some (`String "N/A") ->
    let () = log_missing_field n key optional in
    if optional then Ok None else Error ()
  | Some (`Float value) -> Ok (Some value)
  | Some value ->
    let key_value_string = key ^ ", " ^ Yaml.to_string_exn value in
    let () = log_parseerror FieldTypeError n key_value_string in
    Error ()
;;

(* Get a [Yaml.value] that is expected to be of type [`String]. Return [Ok Some string] if
   it is found, [Ok None] if it is missing/null/empty and the field is optional, or [Error
   ()] if it is missing/null/empty and the field is compulsory or the value is malformed.

   If the field is not found or is null or malformed, either a warning or an error is
   logged explaining the issue.
 
   The [yaml] block is assumed to be of dictionary type. *)
let read_string_field n yaml key =
  let optional = is_field_optional key in
  let value_opt = Yaml.Util.find_exn key yaml in
  match value_opt with
  | None | Some `Null | Some (`String "") | Some (`String "N/A") ->
    let () = log_missing_field n key optional in
    if optional then Ok None else Error ()
  | Some (`String value) -> Ok (Some value)
  | Some value ->
    let key_value_string = key ^ ", " ^ Yaml.to_string_exn value in
    let () = log_parseerror FieldTypeError n key_value_string in
    Error ()
;;

(* Get a [Yaml.value] that is expected to be of type [`String] representing a date in the
   YYYY-MM-DD format. Return value is as with [read_string_field], except the string is
   parsed into a [CalendarLib.Date] object, or failing this, [Error ()] is returned.
 
   The [yaml] block is assumed to be of dictionary type. *)
let read_date_field n yaml key =
  let string_value = read_string_field n yaml key in
  match string_value with
  | Ok (Some str) ->
    let date = Utils.date_of_string str in
    (match date with
     | Ok date -> Ok (Some date)
     | Error _ ->
       let () = log_parseerror DateParsingError n (key ^ ", " ^ str) in
       Error ())
  | Ok None -> Ok None
  | Error _ -> Error ()
;;

(* Get a [Yaml.value] that is expected to be of type [`String] or [`A `String list].
   Return [Ok Some (string list)] if it is found (making a singleton list if only a single
   [`String] was found), [Ok None] if it is missing/null/empty and the field is optional,
   or [Error ()] if it is missing/null/empty and the field is compulsory or the value is
   malformed.

   If the field is not found or is null or malformed, either a warning or an error is
   logged explaining the issue.

   The [yaml] block is assumed to be of dictionary type. *)
let read_string_list_field n yaml key =
  let optional = is_field_optional key in
  let value_opt = Yaml.Util.find_exn key yaml in
  match value_opt with
  | None | Some `Null | Some (`String "") | Some (`String "N/A") ->
    let () = log_missing_field n key optional in
    if optional then Ok None else Error ()
  | Some (`String value) -> Ok (Some [ value ])
  (* `A is how the Yaml library marks YAML lists. *)
  | Some (`A yaml_list) ->
    List.fold_left
      (fun acc_result x ->
        match acc_result, x with
        (* If we have a live accumulator and a new string value in the list, append it. *)
        | Ok (Some acc), `String value -> Ok (Some (value :: acc))
        (* In any other case, kill the accumulator, turning it into an Error. *)
        | _ -> Error ())
      (Ok (Some []))
      yaml_list
  | Some value ->
    let key_value_string = key ^ ", " ^ Yaml.to_string_exn value in
    let () = log_parseerror FieldTypeError n key_value_string in
    Error ()
;;

(* Check if a given yaml block has any unexpected extra keys. If yes, log errors noting
   them. Return [true] if extra keys are found, [false] otherwise.)

   The [yaml] block is assumed to be of dictionary type. *)
let check_extra_keys n yaml =
  let yaml_keys = Yaml.Util.keys_exn yaml |> StringSet.of_list in
  let extra_keys = StringSet.diff yaml_keys valid_keys in
  let () = StringSet.iter (log_parseerror ExtraFieldError n) extra_keys in
  not @@ StringSet.is_empty extra_keys
;;

(* Given the [yaml] block, parse its fields and construct the metadata object. Return
   [Error ()] if something goes wrong, and log errors and warnings as needed.

   The [yaml] block is assumed to be of dictionary type. *)
let metadata_of_yaml (n : int) (yaml : Yaml.value) =
  let extra_keys = check_extra_keys n yaml in
  let turing_project_code_res = read_string_list_field n yaml "turing-project-code" in
  let earliest_start_date_res = read_date_field n yaml "earliest-start-date" in
  let latest_start_date_res = read_date_field n yaml "latest-start-date" in
  let latest_end_date_res = read_date_field n yaml "latest-end-date" in
  let max_fte_percent_res = read_float_field n yaml "max-FTE-percent" in
  let min_fte_percent_res = read_float_field n yaml "min-FTE-percent" in
  let nominal_fte_percent_res = read_float_field n yaml "nominal-FTE-percent" in
  let fte_months_res = read_float_field n yaml "FTE-months" in
  let fte_weeks_res = read_float_field n yaml "FTE-weeks" in
  (* Check if parsing of all fields went okay, and that only the expected fields are
     present. If yes, construct the metadata object, if not, return [Error ()]. *)
  match
    ( extra_keys
    , turing_project_code_res
    , earliest_start_date_res
    , latest_start_date_res
    , latest_end_date_res
    , max_fte_percent_res
    , min_fte_percent_res
    , nominal_fte_percent_res
    , fte_months_res
    , fte_weeks_res )
  with
  | ( false
    , Ok turing_project_code
    , Ok earliest_start_date
    , Ok latest_start_date
    , Ok latest_end_date
    , Ok max_fte_percent
    , Ok min_fte_percent
    , Ok nominal_fte_percent
    , Ok fte_months
    , Ok fte_weeks ) ->
    (match fte_weeks, fte_months with
     | None, None ->
       let () = log_parseerror FTETimeUnderSpecifiedError n "" in
       Error ()
     | Some _, Some _ ->
       let () = log_parseerror FTETimeOverSpecifiedError n "" in
       Error ()
     | _ ->
       Ok
         { turing_project_code
         ; earliest_start_date
         ; latest_start_date
         ; latest_end_date
         ; max_fte_percent
         ; min_fte_percent
         ; nominal_fte_percent
         ; fte_months
         ; fte_weeks
         })
  | _ -> Error ()
;;

let metadata_of_yaml_string (n : int) (y : string) =
  let mdata_result = Yaml.of_string y in
  match mdata_result with
  | Ok yaml ->
    (* Check that this yaml block is of the dictionary type, rather than a single value
     or a list. *)
    (match yaml with
     | `O _ -> metadata_of_yaml n yaml
     | _ ->
       let () = log_parseerror YamlError n "YAML block is not a dictionary" in
       Error ())
  | Error (`Msg err) ->
    let () = log_parseerror YamlError n err in
    Error ()
;;

let parse_metadata (n : int) (body : string) =
  let x = Str.split (Str.regexp {|\+\+\+|}) body in
  match x with
  | [ top; rest ] ->
    let mdata_res = metadata_of_yaml_string n top in
    (match mdata_res with
     | Ok mdata -> Some mdata, rest
     | Error () -> None, body)
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
  let issues =
    Raw.get_project_issues project_name
    |> List.filter (fun (issue : Raw.issue) -> is_valid_column issue.column)
  in
  Printf.printf "Obtained %d Github issues\n" (List.length issues);
  List.filter_map validate_issue issues
;;
