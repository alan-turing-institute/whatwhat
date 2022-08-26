open Batteries
open Cohttp
open Cohttp_lwt_unix
open Yojson
open CalendarLib

(* ---------------------------------------------------------------------- *)
(* TYPES *)

exception HttpError of string
exception QueryError of string

type person =
  { login : string
  ; name : string option
  ; email : string option
  }
[@@deriving show]

let date_opt_to_string (dt: Date.t option) =
    match dt with
    | Some x -> Printer.Date.to_string x
    | None -> "None"

type metadata =  
{ turing_project_code : string option
  ; earliest_start_date : Date.t option [@printer fun fmt dt -> Format.pp_print_string fmt (date_opt_to_string dt)]
  ; latest_start_date : Date.t option [@printer fun fmt dt -> Format.pp_print_string fmt (date_opt_to_string dt)]
  ; latest_end_date : Date.t option [@printer fun fmt dt -> Format.pp_print_string fmt (date_opt_to_string dt)]
  ; fte_months : float option
  ; nominal_fte_percent : float option
  ; max_fte_percent : float option
  ; min_fte_percent : float option
  }
[@@deriving show]

type issue =
  { number : int
  ; title : string
  ; metadata : metadata option
  ; body : string option
  ; state : string
  ; assignees : person list
  ; reactions : (string * person) list
  ; column : string option
  }
[@@deriving show]

type column =
  { name : string
  ; cards : (issue * string) list
      (* The string is a cursor, would be nice to have a type alias for it. *)
  }
[@@deriving show]

type project =
  { number : int
  ; name : string
  ; columns : column list
  }
[@@deriving show]

type project_root = { projects : project list } [@@deriving show]


(* ---------------------------------------------------------------------- *)
(* METADATA LOGGING *)
type parseerror =
| FieldError
| FieldWarning
| LengthError
| LineError

let log_warning (what: parseerror) (number: int) msg =
  match what with
  | LengthError -> print_endline @@ "Metadata Parse Error (num: " ^ (string_of_int number) ^ "): Expected 8 metadata keys, got " ^ msg 
  | LineError -> print_endline @@ "Metadata Parse Error (num: " ^ (string_of_int number) ^ "): Unable to break line into (key, value) - " ^ msg
  | FieldError -> print_endline @@ "Metadata Parse Error (num: " ^ (string_of_int number) ^ "): Unable to parse key " ^ msg
  | FieldWarning -> print_endline @@ "Metadata Parse Warning (num: " ^ (string_of_int number) ^ "): key " ^ msg
  


(* ---------------------------------------------------------------------- *)
(* METADATA PARSING *)


let maybe_null ~(f: string -> 'a) (x: string option) =
  match x with
  | None -> None
  | Some y -> match y with 
              | "" -> None
              | "null" -> None
              | _ -> Some (f y)

let maybe_null_string s = maybe_null ~f:(fun x -> x) s
let maybe_null_float s = maybe_null ~f:float_of_string s 

let make_date (n: int) (str: string option) = 
  match str with
  | None -> log_warning  FieldError n "key does not exist"; None
  | Some x -> let datelist = Str.split (Str.regexp {|-|}) x in
    match datelist with
    | year :: month :: day :: [] -> Some (Date.make (int_of_string year) (int_of_string month) (int_of_string day))
    | _ -> log_warning  FieldError n x; None
  

(* the value should be of the form " <val>". Any other spaces then content indicates a violation*)
let check_value (n: int) (k: string) (v: string) =
  let x = Str.split (Str.regexp " ") v in
  match x with
  | [] -> log_warning  FieldWarning n (k ^ ", empty value"); ""
  | "null"::[] -> log_warning  FieldWarning n (k ^ ", null value"); ""
  | y::[] -> y
  | y::z -> log_warning FieldWarning n (k ^ ", additional info - " ^ (String.concat "" z)); y

let list_to_pair (n: int) (x: string list) =
  match x with
  | [k;v] -> (k, (check_value n k v))
  | y -> log_warning LineError n (String.concat ";"  y); ("", "")
  

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
  | _ -> log_warning LengthError n (string_of_int len); None

let metadata_of_yaml (n: int) (y: string) = 
  let lines = Str.split (Str.regexp "\r\n") y in
  parse_lines n lines

  let parse_metadata (number: int) (body: string) =
    let x = Str.split (Str.regexp {|+++|}) body in
    match x with
    | top :: rest :: [] -> let mdata = top |> metadata_of_yaml number in (mdata, Some rest) 
    | _ -> (None, None) 

    
(* ---------------------------------------------------------------------- *)
(* PARSERS *)



let member = Basic.Util.member
let member_to_string (str: string) json = json |> member str |> Basic.Util.to_string

let member_to_int (str: string) json = member str json |> Basic.Util.to_int

  

let person_of_json json =
  { login = json |> member_to_string "login"
  ; name = json |> member "name" |> Basic.Util.to_string_option
  ; email = json |> member "email" |> Basic.Util.to_string_option
  }
;;

let issue_of_json json =
  json
  |> member "node"
  |> member "content"
  |> fun x ->
    let number = x |> member_to_int "number" in
    let (metadata, body) = x |> member_to_string "body" |> parse_metadata number in
  { number
  ; title = x |> member_to_string "title"
  ; metadata
  ; body
  ; state = x |> member_to_string "state"
  ; column =
      x
      |> member "projectCards"
      |> member "edges"
      |> Basic.Util.convert_each (fun y ->
           y
           |> member "node"
           |> member "column"
           |> member "name"
           |> Basic.Util.to_string_option)
      |> List.first
  ; assignees =
      x
      |> member "assignees"
      |> member "edges"
      |> Basic.Util.convert_each (fun y -> y |> member "node" |> person_of_json)
  ; reactions =
      x
      |> member "reactions"
      |> member "edges"
      |> Basic.Util.convert_each (fun y ->
           ( y |> member "node" |> member_to_string "content"
           , y |> member "node" |> member "user" |> person_of_json ))
  }
;;

let column_of_json json =
  json
  |> member "node"
  |> fun x ->
  { name = x |> member_to_string "name"
  ; cards =
      x
      |> member "cards"
      |> member "edges"
      |> Basic.Util.convert_each (fun y ->
           issue_of_json y, y |> member_to_string "cursor")
  }
;;

let project_of_json json =
  json
  |> member "node"
  |> fun x ->
  { number = x |> member_to_int "number"
  ; name = x |> member_to_string "name"
  ; columns =
      x |> member "columns" |> member "edges" |> Basic.Util.convert_each column_of_json
  }
;;

let project_root_of_json json =
  { projects =
      json
      |> member "data"
      |> member "repository"
      |> member "projects"
      |> member "edges"
      |> Basic.Util.convert_each project_of_json
  }
;;




(* ---------------------------------------------------------------------- *)
(* QUERYING GITHUB *)
(* See
   https://docs.github.com/en/graphql/guides/forming-calls-with-graphql#communicating-with-graphql
*)

let github_graph_ql_endpoint = "https://api.github.com/graphql"

let check_http_response response =
  if Response.status response |> Code.code_of_status |> Code.is_success |> not
  then (
    let code_string = Response.status response |> Code.string_of_status in
    raise @@ HttpError code_string)
;;

let run_github_query (git_hub_token : string) body =
  let auth_cred = Auth.credential_of_string ("Bearer " ^ git_hub_token) in
  let header =
    Header.init ()
    |> fun header ->
    Header.add_authorization header auth_cred
    |> fun header -> Header.prepend_user_agent header "NowWhat"
  in
  let body_obj = Cohttp_lwt.Body.of_string body in
  let uri = Uri.of_string github_graph_ql_endpoint in
  let response, body = Client.post ~headers:header ~body:body_obj uri |> Lwt_main.run in
  let () = check_http_response response in
  let body_json =
    body |> Cohttp_lwt.Body.to_string |> Lwt_main.run |> Basic.from_string
  in
  let errors = Basic.Util.member "errors" body_json in
  match errors with
  | `Null -> body_json
  | _ -> raise @@ QueryError (Basic.to_string errors)
;;

(* For formatting the JSON query to enable correct parsing on Github's side *)
let remove_line_breaks (q : string) = Str.global_replace (Str.regexp "\n") "" q

(* ---------------------------------------------------------------------- *)

let issue_query_template_path = "./queries/issues-by-project-graphql.graphql"
let user_query_template_path = "./queries/users.graphql"

let read_file_as_string filepath =
  let channel = filepath |> open_in in
  let return_string = channel |> BatIO.lines_of |> BatEnum.fold ( ^ ) "" in
  let () = close_in channel in
  return_string
;;

(* The Github API query's JSON body is built by taking a template and replacing
   some placeholders with the project board name and cursor for paging. *)
let build_issue_query project_name cursor =
  let query_template = read_file_as_string issue_query_template_path in
  let cursor_query =
    let cursor_exp = Str.regexp "CURSOR" in
    let replace_with =
      match cursor with
      | None -> "null" (* Get first batch *)
      | Some crs -> "\\\"" ^ crs ^ "\\\"" (* Get subsequent batches *)
    in
    Str.global_replace cursor_exp replace_with query_template
  in

  let to_replace = Str.regexp "PROJECTNAME" in
  let replace_with = "\\\"" ^ project_name ^ "\\\"" in
  Str.global_replace to_replace replace_with cursor_query |> remove_line_breaks
;;

(* These are the main functions of this module, that would be called
   externally. *)

let get_users () =
  let github_token = Config.settings.githubToken in
  let user_query = read_file_as_string user_query_template_path in
  let body_json = run_github_query github_token user_query in
  let users =
    body_json
    |> Basic.Util.member "data"
    |> Basic.Util.member "repository"
    |> Basic.Util.member "assignableUsers"
    |> Basic.Util.member "edges"
    |> Basic.Util.convert_each (fun json ->
         json |> Basic.Util.member "node" |> person_of_json)
  in
  users
;;

let get_project_issues (project_name : string) =
  let github_token = Config.settings.githubToken in

  let rec get_project_issues_page
    (project_name : string)
    cursor
    (acc : (issue * string) list)
    =
    let issue_query = build_issue_query project_name cursor in
    let body_json = run_github_query github_token issue_query in

    (* Parse the response into a list of issues. *)
    let issues = body_json |> project_root_of_json in

    (* TODO Why only the head? Might there be more projects? *)
    let issue_data =
      issues.projects
      |> List.hd
      |> (fun x -> x.columns)
      |> List.map (fun column -> column.cards)
      |> List.concat
    in

    (* Cursor points to the last item returned, used for paging of the requests
     *)
    let next_cursor =
      if List.length issue_data = 0
      then None
      else issue_data |> List.last |> fun (_, c) -> Some c
    in

    let new_acc = acc @ issue_data in

    match next_cursor with
    | Some _ -> get_project_issues_page project_name next_cursor new_acc
    | None -> new_acc
  in

  let all_issues = get_project_issues_page project_name None [] in
  List.map fst all_issues
;;
