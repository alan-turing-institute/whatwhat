module Raw = GithubRaw
open Domain

type github_event =
  | NoMetadataError of Raw.issue (* E2001 *)
  | YamlError of Raw.issue (* E2002 *)
  | ExtraFieldError of Raw.issue * string (* E2003 *)
  | InvalidFieldError of Raw.issue * string (* E2004 *)
  | DuplicateFieldError of Raw.issue * string (* E2003 *)
  | FTETimeUnderSpecifiedError of Raw.issue (* E2006 *)
  | FTETimeOverSpecifiedError of Raw.issue (* E2007 *)
  | NullFieldError of Raw.issue * string (* E2008 *)
  | MissingCompulsoryFieldError of Raw.issue * string (* E2009 *)
  | NoFinanceCodesError of Raw.issue (* E2010 *)

(** Re-exporting for convenience *)
let get_all_users_async = Raw.get_all_users_async

(** Log an error given the error type, Github issue number, and explanatory message.*)
let log_event (gh_event : github_event) : unit =
  match gh_event with
  | NoMetadataError raw_issue ->
    Log.log'
      { level = Log.Error' 2001
      ; entity = Log.Project raw_issue.number
      ; message = "YAML metadata not found in issue body."
      }
  | YamlError raw_issue ->
    Log.log'
      { level = Log.Error' 2002
      ; entity = Log.Project raw_issue.number
      ; message = "Unable to parse metadata block as valid YAML."
      }
  | ExtraFieldError (raw_issue, fld) ->
    Log.log'
      { level = Log.Error' 2003
      ; entity = Log.Project raw_issue.number
      ; message = Printf.sprintf "Unexpected field <%s> in metadata." fld
      }
  | InvalidFieldError (raw_issue, fld) ->
    Log.log'
      { level = Log.Error' 2004
      ; entity = Log.Project raw_issue.number
      ; message = Printf.sprintf "Field <%s> had an invalid value." fld
      }
  | DuplicateFieldError (raw_issue, fld) ->
    Log.log'
      { level = Log.Error' 2005
      ; entity = Log.Project raw_issue.number
      ; message = Printf.sprintf "Field <%s> was specified more than once." fld
      }
  | FTETimeUnderSpecifiedError raw_issue ->
    Log.log'
      { level = Log.Error' 2006
      ; entity = Log.Project raw_issue.number
      ; message = "Neither FTE-months nor FTE-weeks were specified."
      }
  | FTETimeOverSpecifiedError raw_issue ->
    Log.log'
      { level = Log.Error' 2007
      ; entity = Log.Project raw_issue.number
      ; message = "Both FTE-months and FTE-weeks were specified."
      }
  | NullFieldError (raw_issue, fld) ->
    Log.log'
      { level = Log.Error' 2008
      ; entity = Log.Project raw_issue.number
      ; message = Printf.sprintf "Field <%s> is null or empty." fld
      }
  | MissingCompulsoryFieldError (raw_issue, fld) ->
    Log.log'
      { level = Log.Error' 2009
      ; entity = Log.Project raw_issue.number
      ; message = Printf.sprintf "Field <%s> is not present." fld
      }
  | NoFinanceCodesError raw_issue ->
    Log.log'
      { level = Log.Error' 2010
      ; entity = Log.Project raw_issue.number
      ; message = "Finance codes in GitHub metadata were empty."
      }
;;

type person = GithubRaw.person =
  { login : string
  ; name : string option
  ; email : string option
  }
[@@deriving show, ord]

type issue =
  { number : int (** The issue number from GitHub *)
  ; name : string
  ; state : Domain.State.t
  ; programme : string option
  ; plan : Domain.project_plan option
  ; assignees : string list
  }

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

(* Parse a YAML metadata value into an instance of [metadata_value result],
   returning an [Error] if the field is not of one of the recognised types. *)
let read_metadata_value ~compulsory prj (pairs : (string * Yaml.value) list) key =
  let values = pairs |> List.filter (fun (k, _) -> k = key) |> List.map snd in
  match values with
  | [] ->
    if compulsory
    then (
      log_event (MissingCompulsoryFieldError (prj, key));
      Error ())
    else Ok Missing
  | _ :: _ :: _ ->
    log_event (DuplicateFieldError (prj, key));
    Error ()
  | [ `Null ] | [ `String "" ] | [ `String "N/A" ] -> Ok Null
  | [ `Float value ] -> Ok (Float value)
  | [ `String value ] -> Ok (String value)
  (* `A is how the Yaml library marks YAML lists. *)
  | [ `A yaml_list ] ->
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
  | _ ->
    log_event (InvalidFieldError (prj, key));
    Error ()
;;

(* Get a [Yaml.value] that is expected to be of type [`Float]. Return [Ok Some
   float] if it is found, [Ok None] if it is missing/null and the field is
   optional, or [Error ()] if it is missing/null and the field is compulsory or
   the value is malformed.

   If the field is not found or is null or malformed, either a warning or an
   error is logged explaining the issue.

   The [yaml] block is assumed to be of dictionary type. *)
let read_nonneg_float_field ?(compulsory = true) n yaml key =
  let* value_result = read_metadata_value ~compulsory n yaml key in
  match value_result with
  | Float f when f >= 0. -> Ok (Some f)
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
let read_string_list_field ?(compulsory = true) n yaml key =
  let* value_result = read_metadata_value ~compulsory n yaml key in
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
let read_date_field ?(compulsory = true) n yaml key =
  let* value_result = read_metadata_value ~compulsory n yaml key in
  match value_result with
  | String value ->
    (match Utils.date_of_string value with
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
   [NullFieldError]. *)
let enforce_non_null_field n key value_opt =
  match value_opt with
  | Some value -> Ok value
  | None ->
    log_event (NullFieldError (n, key));
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
   them. Return [Error ()] if extra keys are found, [Ok ()] otherwise.) *)
let check_extra_keys n (pairs : (string * Yaml.value) list) =
  let yaml_keys = pairs |> List.map fst |> StringSet.of_list in
  let extra_keys = StringSet.diff yaml_keys valid_keys in
  StringSet.iter (fun k -> log_event (ExtraFieldError (n, k))) extra_keys;
  if StringSet.is_empty extra_keys then Ok () else Error ()
;;

(* Given the [yaml] block, parse its fields and construct the metadata object. Return
   [Error ()] if something goes wrong, and log errors and warnings as needed.

   The [yaml] block is assumed to be of dictionary type. *)
let metadata_of_yaml issue (pairs : (string * Yaml.value) list) =
  let* () = check_extra_keys issue pairs in
  let* finance_codes =
    let* finance_codes_opt = read_string_list_field issue pairs "turing-project-code" in
    Ok
      (match finance_codes_opt with
       | Some xs -> xs
       | None -> [])
  in
  let* earliest_start_date = read_date_field issue pairs "earliest-start-date" in
  let* latest_start_date =
    read_date_field issue pairs "latest-start-date"
    >>= enforce_non_null_field issue "latest-start-date"
  in
  let* latest_end_date = read_date_field issue pairs "latest-end-date" in
  let* max_fte_percent_opt = read_nonneg_float_field issue pairs "max-FTE-percent" in
  let* min_fte_percent_opt = read_nonneg_float_field issue pairs "min-FTE-percent" in
  let* nominal_fte_percent =
    read_nonneg_float_field issue pairs "nominal-FTE-percent"
    >>= enforce_non_null_field issue "nominal-FTE-percent"
  in
  let max_fte_percent = Option.value max_fte_percent_opt ~default:nominal_fte_percent in
  let min_fte_percent = Option.value min_fte_percent_opt ~default:nominal_fte_percent in
  let* fte_months = read_nonneg_float_field ~compulsory:false issue pairs "FTE-months" in
  let* fte_weeks = read_nonneg_float_field ~compulsory:false issue pairs "FTE-weeks" in
  let* budget =
    match fte_weeks, fte_months with
    | Some weeks, None -> Ok (FTE.Weeks weeks)
    | None, Some months -> Ok (FTE.Months months)
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
     | `O pairs -> metadata_of_yaml issue pairs
     | _ ->
       log_event (YamlError issue);
       Error ())
  | Error (`Msg _) ->
    log_event (YamlError issue);
    Error ()
;;

let parse_metadata (issue : Raw.issue) =
  let x = Str.split (Str.regexp {|\+\+\+|}) issue.body in
  match x with
  | [ top; _ ] ->
    let mdata_res = metadata_of_yaml_string issue top in
    (match mdata_res with
     | Ok mdata ->
       (*Domain.show_project_plan mdata;*)
       Some mdata
     | Error () -> None)
  | _ ->
    log_event (NoMetadataError issue);
    None
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

let make_issue (col_name : string) (issue : Raw.issue) : issue =
  let plan =
    match parse_metadata issue with
    | None -> None
    | Some plan ->
      let state = state_of_column col_name in
      if state >= State.FindingPeople && List.length plan.finance_codes = 0
      then log_event (NoFinanceCodesError issue);
      Some plan
  in
  { number = issue.number
  ; name = issue.title
  ; state = state_of_column col_name
  ; programme = find_programme issue
  ; plan
  ; assignees = issue.assignees
  }
;;

let get_project_issues_async () =
  let open Lwt.Syntax in
  let* project_board = Raw.get_project_async () in
  let pair_issues (col : Raw.column) = List.map (fun i -> col.name, i) col.issues in
  let pairs = project_board.columns |> List.concat_map pair_issues in
  let projects = pairs |> List.map (fun (c, i) -> make_issue c i) in
  Lwt.return projects
;;

let get_project_issues () = Lwt_main.run (get_project_issues_async ())
