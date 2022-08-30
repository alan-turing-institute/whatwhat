(* High-level interface to the Github API

  Performs validation for Github issues. A valid project issue has: 
   - Metadata, with at least the entries:
        - earliest_start_date
        - latest_start_date
        - fte_months  
        - nominal_fte_percent
   
  Errors are logged to the console when these are missing, and the issue is removed.contents

  Warnings are given for other inconsisties (e.g. no one is assigned.)
  s

 *)


open CalendarLib
module Raw = GithubRaw


(* ---------------------------------------------------------------------- *)
(* METADATA LOGGING *)
type parseerror =
| FieldError
| FieldWarning
| LengthError
| LineError

let log_parseerror (what: parseerror) (number: int) msg =
  match what with
  | LengthError -> print_endline @@ "Error: Metadata Parsing (num: " ^ (string_of_int number) ^ "): Expected 8 metadata keys, got " ^ msg 
  | LineError -> print_endline @@ "Error: Metadata Parsing (num: " ^ (string_of_int number) ^ "): Unable to break line into (key, value) - " ^ msg
  | FieldError -> print_endline @@ "Error: Metadata Parsing (num: " ^ (string_of_int number) ^ "): Unable to parse key " ^ msg
  | FieldWarning -> print_endline @@ "Warning: Metadata Parsing (num: " ^ (string_of_int number) ^ "): key " ^ msg

(* ---------------------------------------------------------------------- *)
(* TYPES *)

let dateprinter fmt (dt: Date.t) = Format.pp_print_string fmt (Printer.Date.to_string dt)
let dateprint_opt (dt: Date.t option) = 
  match dt with
  | Some x -> Printer.Date.to_string x
  | None -> "None"


type metadata = 
  { turing_project_code : string option
  ; earliest_start_date : Date.t option [@printer dateprinter]
  ; latest_start_date : Date.t option [@printer dateprinter]
  ; latest_end_date : Date.t option [@printer fun fmt x -> Format.pp_print_string fmt (dateprint_opt x)]
  ; fte_months : float option
  ; nominal_fte_percent : float option
  ; max_fte_percent : float option
  ; min_fte_percent : float option
  }

type project =
  { number : int
  ; title : string
  ; body : string
  ; state : string
  ; assignees : Raw.person list
  ; reactions : (string * Raw.person) list
  ; column : string option
  ; metadata : metadata
  }


(* ---------------------------------------------------------------------- *)
(* METADATA PARSING & VALIDATION *)

let maybe_null ~(f: string -> 'a) (x: string option) =
  match x with
  | None -> None
  | Some y -> match y with 
              | "" -> None
              | "null" -> None
              | _ -> Some (f y)

let maybe_null_string s = maybe_null ~f:(fun x -> x) s
let maybe_null_float s = maybe_null ~f:float_of_string s 

let catch_date_exn year month day n =
  try Some (Date.make year month day) with
    Date.Out_of_bounds -> log_parseerror FieldError n "date out of bounds"; None


let make_date (n: int) (str: string option) = 
  match str with
  | None -> log_parseerror  FieldError n "key does not exist"; None
  | Some x -> let datelist = Str.split (Str.regexp {|-|}) x in
    match datelist with
    | year :: month :: day :: [] -> catch_date_exn (int_of_string year) (int_of_string month) (int_of_string day) n   
    | _ -> log_parseerror  FieldError n x; None

  
(* the value should be of the form " <val>". Any other spaces then content indicates a violation*)
let check_value (n: int) (k: string) (v: string) =
  let x = Str.split (Str.regexp " ") v in
  match x with
  | [] -> log_parseerror  FieldWarning n (k ^ ", empty value"); ""
  | "null"::[] -> log_parseerror  FieldWarning n (k ^ ", null value"); ""
  | y::[] -> y
  | y::z -> log_parseerror FieldWarning n (k ^ ", additional info - " ^ (String.concat "" z)); y


  let list_to_pair (n: int) (x: string list) =
    match x with
    | [k;v] -> (k, (check_value n k v))
    | y -> log_parseerror LineError n (String.concat ";"  y); ("", "")
    
  
  let parse_fields (n: int) (lines: string list) =
    
    let fields = List.map (fun x -> Str.split (Str.regexp {|:|}) x |> list_to_pair n) lines in
    Some { turing_project_code = fields |> List.assoc_opt "turing-project-code" |> maybe_null_string
      ; earliest_start_date = fields |> List.assoc_opt "earliest-start-date" |> make_date n
      ; latest_start_date = fields |> List.assoc_opt "latest-start-date" |> make_date n
      ; latest_end_date = fields |> List.assoc_opt "latest-end-date" |> make_date n
      ; fte_months = fields |> List.assoc_opt "FTE-months" |> maybe_null_float
      ; nominal_fte_percent = fields |> List.assoc_opt "nominal-FTE-percent" |> maybe_null_float
      ; max_fte_percent = fields |> List.assoc_opt "max-FTE-percent" |> maybe_null_float
      ; min_fte_percent = fields |> List.assoc_opt "min-FTE-percent" |> maybe_null_float
    } 

  

let parse_lines (n: int) (lines: string list) =
  let len = List.length lines in 
  match len with 
  | 8 -> parse_fields n lines 
  | _ -> log_parseerror LengthError n (string_of_int len); None

let metadata_of_yaml (n: int) (y: string) = 
  let lines = Str.split (Str.regexp "\r\n") y in
  parse_lines n lines

let parse_metadata (number: int) (body: string) =
  let x = Str.split (Str.regexp {|+++|}) body in
  match x with
  | top :: rest :: [] -> let mdata = top |> metadata_of_yaml number in (mdata, Some rest) 
  | _ -> (None, None) 


let validate_issue (issue: Raw.issue) = 
  let (metadata, body) = issue.body |> parse_metadata issue.number in
  match metadata with
  | None -> None
  | Some x -> 
    Some { number = issue.number
    ; title = issue.title
    ; body = Option.get body
    ; state = issue.state
    ; assignees = issue.assignees
    ; reactions = issue.reactions
    ; column = issue.column
    ; metadata = x
    } 


let get_project_issues (project_name: string) = 
  let issues = Raw.get_project_issues project_name in
  List.filter_map validate_issue issues
  (* List.iter (fun c -> print_endline @@ GithubRaw.show_issue c) issues *)