module Raw = GithubRaw
open Domain

(* Re-exporting for convenience of modules that import this one. *)
let all_users = Raw.all_users

type github_event =
  | NoMetadataError of Raw.issue (* E2001 *)
  | YamlError of Raw.issue (* E2002 *)
  | InvalidFieldError of Raw.issue * string (* E2004 *)
  | FTETimeUnderSpecifiedError of Raw.issue (* E2006 *)
  | FTETimeOverSpecifiedError of Raw.issue (* E2007 *)
  | MissingCompulsoryFieldError of Raw.issue * string (* E2008 *)
  | ExtraFieldError of Raw.issue * string (* E2009 *)

(** Log an error given the error type, Github issue number, and explanatory message.*)
let log_event (gh_event : github_event) : unit =
  match gh_event with
  | NoMetadataError raw_issue ->
    Log.log'
      { level = Log.Error' 2001
      ; source = Log.GithubMetadata
      ; entity = Log.Project raw_issue.number
      ; message = "YAML metadata not found in issue body."
      }
  | YamlError raw_issue ->
    Log.log'
      { level = Log.Error' 2002
      ; source = Log.GithubMetadata
      ; entity = Log.Project raw_issue.number
      ; message = "Unable to parse metadata block as valid YAML."
      }
  | InvalidFieldError (raw_issue, fld) ->
    Log.log'
      { level = Log.Error' 2004
      ; source = Log.GithubMetadata
      ; entity = Log.Project raw_issue.number
      ; message = Printf.sprintf "Field <%s> had an invalid value." fld
      }
  | ExtraFieldError (raw_issue, fld) ->
    Log.log'
      { level = Log.Error' 2009
      ; source = Log.GithubMetadata
      ; entity = Log.Project raw_issue.number
      ; message = Printf.sprintf "Unexpected field <%s> in metadata" fld
      }
  | FTETimeUnderSpecifiedError raw_issue ->
    Log.log'
      { level = Log.Error' 2006
      ; source = Log.GithubMetadata
      ; entity = Log.Project raw_issue.number
      ; message = "Neither FTE-months nor FTE-weeks were specified."
      }
  | FTETimeOverSpecifiedError raw_issue ->
    Log.log'
      { level = Log.Error' 2007
      ; source = Log.GithubMetadata
      ; entity = Log.Project raw_issue.number
      ; message = "Both FTE-months and FTE-weeks were specified."
      }
  | MissingCompulsoryFieldError (raw_issue, fld) ->
    Log.log'
      { level = Log.Error' 2008
      ; source = Log.GithubMetadata
      ; entity = Log.Project raw_issue.number
      ; message = Printf.sprintf "Missing field: <%s>" fld
      }
;;

type person = GithubRaw.person =
  { login : string
  ; name : string option
  ; email : string option
  }
[@@deriving show]

(* We recognise only floats, strings, and lists of strings as valid types in the metadata.
   We parse the YAML into this variant type first, and then pick the correct types for
   each field later. *)
type metadata_value =
  | Missing
  | Null
  | Float of float
  | String of string
  | StringList of string list

let ( let* ) = Result.bind
let ( >>= ) = Result.bind

(* Parse a YAML medata value into an instance of [metadata_value result], returning an
   [Error] if the field is not of one of the recognised types. *)
let read_metadata_value prj yaml key =
  let value_opt = Yaml.Util.find_exn key yaml in
  match value_opt with
  | None -> Ok Missing
  | Some `Null | Some (`String "") | Some (`String "N/A") -> Ok Null
  | Some (`Float value) -> Ok (Float value)
  | Some (`String value) -> Ok (String value)
  (* `A is how the Yaml library marks YAML lists. *)
  | Some (`A yaml_list) ->
    let acc_string_list acc_result x =
      match acc_result, x with
      (* If we have a live accumulator and a new string value in the list, append it. *)
      | Ok (StringList acc), `String value -> Ok (StringList (value :: acc))
      (* In any other case, kill the accumulator, turning it into an Error. *)
      | _ -> Error ()
    in
    let result = List.fold_left acc_string_list (Ok (StringList [])) yaml_list in
    if Result.is_error result then log_event (InvalidFieldError (prj, key));
    result
  | Some value ->
    let key_value_string = key ^ ", " ^ Yaml.to_string_exn value in
    log_event (InvalidFieldError (prj, key_value_string));
    Error ()
;;

(* Get a [Yaml.value] that is expected to be of type [`Float]. Return [Ok Some float] if
   it is found, [Ok None] if it is missing/null and the field is optional, or [Error ()]
   if it is missing/null and the field is compulsory or the value is malformed.

   If the field is not found or is null or malformed, either a warning or an error is
   logged explaining the issue.

   The [yaml] block is assumed to be of dictionary type. *)
let read_float_field n yaml key =
  let* value_result = read_metadata_value n yaml key in
  match value_result with
  | Float value -> Ok (Some value)
  | Null | Missing -> Ok None
  | _ ->
    log_event (InvalidFieldError (n, key));
    Error ()
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
  let* value_result = read_metadata_value n yaml key in
  match value_result with
  | String value -> Ok (Some [ value ])
  | StringList value -> Ok (Some value)
  | Null | Missing -> Ok None
  | _ ->
    log_event (InvalidFieldError (n, key));
    Error ()
;;

(* Get a [Yaml.value] that is expected to be of type [`String] representing a date in the
   YYYY-MM-DD format. Return value is as with [read_string_field], except the string is
   parsed into a [CalendarLib.Date] object, or failing this, [Error ()] is returned.
 
   The [yaml] block is assumed to be of dictionary type. *)
let read_date_field n yaml key =
  let* value_result = read_metadata_value n yaml key in
  match value_result with
  | String value ->
    let date = Utils.date_of_string value in
    (match date with
     | Ok date -> Ok (Some date)
     | Error _ ->
       log_event (InvalidFieldError (n, key));
       Error ())
  | Null | Missing -> Ok None
  | _ ->
    log_event (InvalidFieldError (n, key));
    Error ()
;;

(* Take an [option] value for a compulsory field, return [Ok value] if the option is
   [Some], or [Error ()] if it's [None]. In the latter case, log also a
   [MissingCompulsoryFieldError]. *)
let enforce_compulsory_field n key value_opt =
  match value_opt with
  | Some value -> Ok value
  | None ->
    log_event (MissingCompulsoryFieldError (n, key));
    Error ()
;;

module StringSet = Set.Make (String)

(* This set defines which keys are valid in the metadata block. *)
let valid_keys =
  StringSet.of_list
    [ "turing-project-code"
    ; "earliest-start-date"
    ; "latest-start-date"
    ; "latest-end-date"
    ; "max-FTE-percent"
    ; "min-FTE-percent"
    ; "nominal-FTE-percent"
    ; "FTE-months"
    ; "FTE-weeks"
    ]
;;

(* Check if a given yaml block has any unexpected extra keys. If yes, log errors noting
   them. Return [Error ()] if extra keys are found, [Ok ()] otherwise.)

   The [yaml] block is assumed to be of dictionary type. *)
let check_extra_keys n yaml =
  let yaml_keys = Yaml.Util.keys_exn yaml |> StringSet.of_list in
  let extra_keys = StringSet.diff yaml_keys valid_keys in
  StringSet.iter (fun k -> log_event (ExtraFieldError (n, k))) extra_keys;
  if StringSet.is_empty extra_keys then Ok () else Error ()
;;

(* Given the [yaml] block, parse its fields and construct the metadata object. Return
   [Error ()] if something goes wrong, and log errors and warnings as needed.

   The [yaml] block is assumed to be of dictionary type. *)
let metadata_of_yaml issue (yaml : Yaml.value) =
  let* () = check_extra_keys issue yaml in
  let* finance_codes =
    read_string_list_field issue yaml "turing-project-code"
    >>= enforce_compulsory_field issue "turing-project-code"
  in
  let* earliest_start_date = read_date_field issue yaml "earliest-start-date" in
  let* latest_start_date =
    read_date_field issue yaml "latest-start-date"
    >>= enforce_compulsory_field issue "turing-project-code"
  in
  let* latest_end_date = read_date_field issue yaml "latest-end-date" in
  let* max_fte_percent_opt = read_float_field issue yaml "max-FTE-percent" in
  let* min_fte_percent_opt = read_float_field issue yaml "min-FTE-percent" in
  let* nominal_fte_percent =
    read_float_field issue yaml "nominal-FTE-percent"
    >>= enforce_compulsory_field issue "nominal-FTE-percent"
  in
  let max_fte_percent = Option.value max_fte_percent_opt ~default:nominal_fte_percent in
  let min_fte_percent = Option.value min_fte_percent_opt ~default:nominal_fte_percent in
  let* fte_months = read_float_field issue yaml "FTE-months" in
  let* fte_weeks = read_float_field issue yaml "FTE-weeks" in
  let* budget =
    match fte_weeks, fte_months with
    | Some weeks, None -> Ok (FTE_weeks weeks)
    | None, Some months -> Ok (FTE_months months)
    | None, None ->
      log_event (FTETimeUnderSpecifiedError issue);
      Error ()
    | Some _, Some _ ->
      log_event (FTETimeOverSpecifiedError issue);
      Error ()
  in
  Ok
    { budget
    ; finance_codes
    ; earliest_start_date
    ; latest_start_date
    ; latest_end_date
    ; max_fte_percent
    ; min_fte_percent
    ; nominal_fte_percent
    }
;;

let metadata_of_yaml_string issue (y : string) =
  let mdata_result = Yaml.of_string y in
  match mdata_result with
  | Ok yaml ->
    (* Check that this yaml block is of the dictionary type, rather than a single value
     or a list. *)
    (match yaml with
     | `O _ -> metadata_of_yaml issue yaml
     | _ ->
       log_event (YamlError issue);
       Error ())
  | Error (`Msg _) ->
    log_event (YamlError issue);
    Error ()
;;

let parse_metadata issue (body : string) =
  let x = Str.split (Str.regexp {|\+\+\+|}) body in
  match x with
  | [ top; rest ] ->
    let mdata_res = metadata_of_yaml_string issue top in
    (match mdata_res with
     | Ok mdata -> Some mdata, rest
     | Error () -> None, body)
  | _ ->
    log_event (NoMetadataError issue);
    None, body
;;

let programme_regex =
  Re.(seq [ start; str "Programme: "; group (rep1 any); stop ]) |> Re.compile
;;

let find_programme (issue : Raw.issue) =
  let parse_label (label : string) =
    let re_group = Re.exec_opt programme_regex label in
    Option.bind re_group (fun g -> Re.Group.get_opt g 1)
  in
  List.find_map parse_label issue.labels
;;

let validate_issue (col_name : string) (issue : Raw.issue) =
  (* GithubRaw returns the issue body as well, but we ignore for now *)
  let metadata, _ = parse_metadata issue issue.body in
  match metadata with
  | None -> None
  | Some plan ->
    Some
      { nmbr = issue.number
      ; name = issue.title
      ; state = state_of_column col_name
      ; programme = find_programme issue
      ; plan
      }
;;

let get_project_issues () =
  let project = Raw.get_project () in
  let pair_issues (col : Raw.column) = List.map (fun i -> col.name, i) col.issues in
  project.columns
  |> List.concat_map pair_issues
  |> List.filter_map (fun (c, i) -> validate_issue c i)
;;
