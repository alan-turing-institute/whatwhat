(* Queries Github API and traverse_of_interests the GraphQL results. Validation is done in github.ml
 *)

open Batteries
open Yojson
open Lwt.Syntax

(* exception QueryError of string *)

type person =
  { login : string
  ; name : string option
  ; email : string option
  }
[@@deriving show]

type issue =
  { number : int
  ; title : string
  ; body : string
  ; state : string
  ; assignees : person list
  ; reactions : (string * person) list
  ; labels : string list
  ; column : string option
  }
[@@deriving show]

(* This is from the graphQL API *)
type column =
  { name : string
  ; cards : (issue * string) list
      (* The string is a cursor, would be nice to have a type alias for it. *)
  }
[@@deriving show]

(* This is from the REST API *)
type rest_column =
  { name : string
  ; id : int
  ; issues : (issue * string) list
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
(* PARSERS *)
(* Parsers for the JSON returned by the Github API *)

(* Convenient bindings for functions we use repeatedly when parsing the JSON returned by
   the API. *)
let member_to_string (str : string) json =
  json |> Basic.Util.member str |> Basic.Util.to_string
;;

let person_of_json json =
  { login = json |> member_to_string "login"
  ; name = json |> Basic.Util.member "name" |> Basic.Util.to_string_option
  ; email = json |> Basic.Util.member "email" |> Basic.Util.to_string_option
  }
;;

(* ------- Generic functions for querying GitHub APIs ------- *)

(* unfortunately method is an OCaml keyword *)
(* TODO: Check for errors in body JSON before returning; member returns `Null if
   the field isn't found. The code below only works if an object is returned; it
   fails if an array of objects is returned (e.g. when getting a list of issues
   in a column *)
(* let errors = Basic.Util.member "errors" body_json in *)
(* match errors with *)
(* | `Null -> Lwt.return body_json *)
(* | _ -> raise @@ QueryError (Basic.to_string errors) *)
let run_github_query_async
  ?(methd = "GET")
  ?(params = [])
  ?(body = "")
  (github_token : string)
  uri
  =
  let open Cohttp in
  let open Cohttp_lwt_unix in
  let auth_cred = Auth.credential_of_string ("Bearer " ^ github_token) in
  let header_obj =
    Header.init ()
    |> (fun header -> Header.add_authorization header auth_cred)
    |> fun header -> Header.prepend_user_agent header "Whatwhat"
  in
  let uri = Uri.add_query_params (Uri.of_string uri) params in
  let* response, body =
    match methd with
    | "GET" -> Client.get ~headers:header_obj uri
    | "POST" ->
      let body_obj = Cohttp_lwt.Body.of_string body in
      Client.post ~headers:header_obj ~body:body_obj uri
    | _ -> failwith ("unsupported method " ^ methd)
  in
  let () = Utils.check_http_response response in
  let* body_string = Cohttp_lwt.Body.to_string body in
  let body_json = Basic.from_string body_string in
  Lwt.return body_json
;;

let run_github_query
  ?(methd = "GET")
  ?(params = [])
  ?(body = "")
  (github_token : string)
  uri
  =
  run_github_query_async ~methd ~params ~body github_token uri |> Lwt_main.run
;;

let read_file_as_string filepath =
  let channel = open_in filepath in
  let return_string = channel |> BatIO.lines_of |> BatEnum.fold ( ^ ) "" in
  let () = close_in channel in
  return_string
;;

let all_hut23_users =
  let github_graph_ql_endpoint = "https://api.github.com/graphql" in
  let user_query_template_path = "./queries/users.graphql" in
  let github_token = Config.get_github_token () in
  let user_query = read_file_as_string user_query_template_path in
  let body_json =
    run_github_query ~methd:"POST" ~body:user_query github_token github_graph_ql_endpoint
  in
  let open Yojson.Basic.Util in
  let users =
    body_json
    |> member "data"
    |> member "repository"
    |> member "assignableUsers"
    |> member "edges"
    |> convert_each (fun json -> json |> member "node" |> person_of_json)
  in
  users
;;

let find_person_by_login login = List.find_opt (fun p -> p.login = login) all_hut23_users

(* ------- Queries using REST API ------- *)

let get_issue_async ?col_name id =
  let ( let* ) = Lwt.bind in
  let open Yojson.Basic.Util in
  let issue_uri =
    "https://api.github.com/repos/alan-turing-institute/Hut23/issues/" ^ string_of_int id
  in
  let reactions_uri =
    "https://api.github.com/repos/alan-turing-institute/Hut23/issues/"
    ^ string_of_int id
    ^ "/reactions"
  in
  let github_token = Config.get_github_token () in
  let* issue_json = run_github_query_async github_token issue_uri in
  let* reactions_json = run_github_query_async github_token reactions_uri in
  let parse_reaction r =
    let rxn = r |> member "content" |> to_string in
    let psn = r |> member "user" |> member "login" |> to_string |> find_person_by_login in
    match rxn, psn with
    | p, Some q -> Some (p, q)
    | _, _ -> None
  in
  let reactions =
    reactions_json |> Basic.Util.to_list |> List.filter_map parse_reaction
  in
  Lwt.return
    { number = id
    ; title = issue_json |> member "title" |> to_string
    ; body = issue_json |> member "body" |> to_string
    ; state = issue_json |> member "state" |> to_string
    ; assignees =
        issue_json
        |> member "assignees"
        |> to_list
        |> List.filter_map (fun a ->
             a |> member "login" |> to_string |> find_person_by_login)
    ; reactions
    ; labels =
        issue_json
        |> member "labels"
        |> to_list
        |> List.map (fun j -> j |> member "name" |> to_string)
    ; column = col_name
    }
;;

let get_issue ?col_name id = get_issue_async ?col_name id |> Lwt_main.run

let get_issues_async (idnames : (int * string) list) =
  idnames |> List.map (fun (i, col) -> get_issue_async ~col_name:col i) |> Lwt.all
;;

(* Get a batch of issues, but with some throttling such that only
   max_concurrent_issues requests are fired off at any one time.
   Turns out that if you ask GitHub for >300 issues at a go, it complains
   Fatal error: exception Failure("TLS to non-TCP currently unsupported:
     host=api.github.com endp=(Unknown \"name resolution failed\")" *)
let rec get_issues_throttled (idnames : (int * string) list) =
  let rec splitAt n xs =
    match n, xs with
    | _, [] -> [], []
    | 1, x :: x' -> [ x ], x'
    | n, x :: x' ->
      let y1, y2 = splitAt (n - 1) x' in
      x :: y1, y2
  in
  let max_concurrent_issues = 150 in
  match splitAt max_concurrent_issues idnames with
  | [], [] -> Lwt.return []
  | xs, [] -> get_issues_async xs
  | xs, ys ->
    let* first_batch = get_issues_async xs in
    let* the_rest = get_issues_throttled ys in
    Lwt.return (first_batch @ the_rest)
;;

(* GitHub only allows for 100 items to be read from a column in a single
   request. *)
let rec get_issue_numbers_in_column_async ?(page = 1) col =
  let batch_get page col =
    let get_issue_number card_json : int option =
      let open Yojson.Basic.Util in
      (* TODO: could do with more thorough error checking *)
      match card_json |> member "content_url" |> to_string_option with
      | Some s -> Some (String.split_on_char '/' s |> List.rev |> List.hd |> int_of_string)
      | None -> None
    in
    let uri =
      "https://api.github.com/projects/columns/" ^ string_of_int col.id ^ "/cards"
    in
    let params = [ "per_page", [ "100" ]; "page", [ string_of_int page ] ] in
    let github_token = Config.get_github_token () in
    let* cards = run_github_query_async ~params github_token uri in
    let issue_numbers =
      cards
      |> Basic.Util.to_list
      |> List.filter_map get_issue_number
      |> List.map (fun i -> i, col.name)
    in
    Lwt.return issue_numbers
  in
  let* first_batch = batch_get page col in
  if List.length first_batch = 100
  then
    let* next_batch = get_issue_numbers_in_column_async ~page:(page + 1) col in
    Lwt.return (first_batch @ next_batch)
  else Lwt.return first_batch
;;

(* column -> (int, string) list *)
(* the string is the name of the column, i.e. project status *)
let get_issue_numbers_in_column col =
  get_issue_numbers_in_column_async col |> Lwt_main.run
;;

let parse_column (column_json : Basic.t) : rest_column =
  let open Yojson.Basic.Util in
  let name = column_json |> member "name" |> to_string in
  let id = column_json |> member "id" |> to_int in
  { name; id; issues = [] }
;;

let default_columns : string list =
  (* ; "Rejected" *)
  (* ; "Cancelled" *)
  (* ; "Done" *)
  (* ; "Completion review" *)
  [ "Active"
  ; "Awaiting start"
  ; "Finding people"
  ; "Awaiting go/no-go"
  ]
  (* ; "Project appraisal" *)
  (* ; "Extra info needed" *)
  (* ; "Proposal" *)
  (* ; "Suggested" *)
;;

(* TODO: don't hardcode the project id; they can be gotten via
    https://api.github.com/repos/alan-turing-institute/Hut23/projects *)
(* string -> () *)
let get_project_issues_async ?(column_names = default_columns) (project_name : string) =
  let github_token = Config.get_github_token () in
  let project_id =
    match project_name with
    | "Project Tracker" -> "621998"
    | "NowWhat Test Project" -> "14539393"
    | _ -> failwith "unknown project name"
  in
  let uri = "https://api.github.com/projects/" ^ project_id ^ "/columns" in
  let* columns = run_github_query_async github_token uri in
  let columns =
    columns
    |> Basic.Util.to_list
    |> List.map parse_column
    |> List.filter (fun (c : rest_column) -> List.mem c.name column_names)
  in
  (* print_endline ("found " ^ string_of_int (List.length columns) ^ " columns"); *)
  let* issue_numbers' =
    columns |> List.map get_issue_numbers_in_column_async |> Lwt.all
  in
  let issue_numbers = List.flatten issue_numbers' in
  (* print_endline ("found " ^ string_of_int (List.length issue_numbers) ^ " issues"); *)
  get_issues_throttled issue_numbers
;;

(* This almost completely gives the same output as GithubRaw.get_project_issues,
   but in a rather shorter time.
   There are a couple of differences:
     1. The issue status (open/closed) is in caps in GithubRaw but small letters
        here. This has to do with GitHub's REST and GraphQL APIs returning
        different values.
     2. The emoji string identifiers are also slightly different (for example,
        GraphQL gives "THUMBS_UP", REST gives "+1").
     3. This function removes issue assignments and reactions for people who are
        no longer in the Hut23 repo.
   *)
let get_project_issues (project_name : string) =
  get_project_issues_async project_name |> Lwt_main.run
;;
