(* Queries Github API and traverse_of_interests the GraphQL results. Validation
   is done in github.ml *)

open Yojson
open Lwt.Syntax

type http_method =
  | GET
  | POST

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

type column =
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
let run_github_query_async ?(methd = GET) ?(params = []) ?(body = "") uri =
  let open Cohttp in
  let open Cohttp_lwt_unix in
  let github_token = Config.get_github_token () in
  let auth_cred = Auth.credential_of_string ("Bearer " ^ github_token) in
  let header_obj =
    Header.init ()
    |> (fun header -> Header.add_authorization header auth_cred)
    |> fun header -> Header.prepend_user_agent header "Whatwhat"
  in
  let uri = Uri.add_query_params (Uri.of_string uri) params in
  let* response, body =
    match methd with
    | GET -> Client.get ~headers:header_obj uri
    | POST ->
      let body_obj = Cohttp_lwt.Body.of_string body in
      Client.post ~headers:header_obj ~body:body_obj uri
  in
  let () = Utils.check_http_response response in
  let* body_string = Cohttp_lwt.Body.to_string body in
  let body_json = Basic.from_string body_string in
  Lwt.return body_json
;;

let run_github_query ?(methd = GET) ?(params = []) ?(body = "") uri =
  run_github_query_async ~methd ~params ~body uri |> Lwt_main.run
;;

let all_users =
  let query_template = {|{ "query": "query { repository(owner: REPO_OWNER, name: REPO_NAME) { assignableUsers(first: 100) { edges { node { login name email } } } } }" } |} in
  let replacements =
    [ Str.regexp "REPO_NAME", "\\\"" ^ Config.get_github_repo_name () ^ "\\\""
    ; Str.regexp "REPO_OWNER", "\\\"" ^ Config.get_github_repo_owner () ^ "\\\""
    ]
  in
  let replace_multiple replacements str =
    let folder query (to_replace, replace_with) =
      Str.global_replace to_replace replace_with query
    in
    List.fold_left folder str replacements
  in
  let query = replace_multiple replacements query_template in

  let github_graph_ql_endpoint = Config.get_github_url () ^ "/graphql" in
  let body_json = run_github_query ~methd:POST ~body:query github_graph_ql_endpoint in
  let open Yojson.Basic.Util in
  body_json
  |> member "data"
  |> member "repository"
  |> member "assignableUsers"
  |> member "edges"
  |> convert_each (fun json -> json |> member "node" |> person_of_json)
;;

let find_person_by_login login = List.find_opt (fun p -> p.login = login) all_users

(* ------- Queries using REST API ------- *)

(* Like column cards, we can only fetch 100 reactions at a time. This function
   therefore fetches the first page of reactions (which can contain up to 100
   results) and then fetches a second page if 100 results were returned. *)
let rec get_issue_reactions_async ?(page = 1) id =
  let batch_get page id =
    let uri =
      String.concat
        "/"
        [ Config.get_github_url ()
        ; "repos"
        ; Config.get_github_repo_owner ()
        ; Config.get_github_repo_name ()
        ; "issues"
        ; string_of_int id
        ; "reactions"
        ]
    in
    let params = [ "per_page", [ "100" ]; "page", [ string_of_int page ] ] in
    let* reactions_json = run_github_query_async ~params uri in
    let open Yojson.Basic.Util in
    let parse_reaction r =
      let login = r |> member "user" |> member "login" in
      (* Deleted users have no login *)
      if login = `Null
      then None
      else (
        let rxn = r |> member "content" |> to_string in
        let psn = login |> to_string |> find_person_by_login in
        match rxn, psn with
        | p, Some q -> Some (p, q)
        | p, None -> Some (p, { login = login |> to_string; name = None; email = None }))
    in
    let reactions =
      reactions_json |> Basic.Util.to_list |> List.filter_map parse_reaction
    in
    Lwt.return reactions
  in
  let* first_batch = batch_get page id in
  if List.length first_batch = 100
  then
    let* next_batch = get_issue_reactions_async ~page:(page + 1) id in
    Lwt.return (first_batch @ next_batch)
  else Lwt.return first_batch
;;

let get_issue_async ?col_name id =
  let ( let* ) = Lwt.bind in
  let open Yojson.Basic.Util in
  let issue_uri =
    String.concat
      "/"
      [ Config.get_github_url ()
      ; "repos"
      ; Config.get_github_repo_owner ()
      ; Config.get_github_repo_name ()
      ; "issues"
      ; string_of_int id
      ]
  in
  let* issue_json = run_github_query_async issue_uri in
  let* reactions = get_issue_reactions_async id in
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
      String.concat
        "/"
        [ Config.get_github_url (); "projects"; "columns"; string_of_int col.id; "cards" ]
    in
    let params = [ "per_page", [ "100" ]; "page", [ string_of_int page ] ] in
    let* cards = run_github_query_async ~params uri in
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

let parse_column (column_json : Basic.t) : column =
  let open Yojson.Basic.Util in
  let name = column_json |> member "name" |> to_string in
  let id = column_json |> member "id" |> to_int in
  { name; id; issues = [] }
;;

(* TODO: don't hardcode the project id; they can be gotten via
    https://api.github.com/repos/OWNER/REPO/projects *)
let get_project_issue_numbers_async () =
  let project_id =
    match Config.get_github_project_name () with
    | "Project Tracker" -> "621998"
    | "NowWhat Test Project" -> "14539393"
    | _ -> failwith "unknown project name"
  in
  let uri =
    String.concat "/" [ Config.get_github_url (); "projects"; project_id; "columns" ]
  in
  let* columns = run_github_query_async uri in
  let columns = columns |> Basic.Util.to_list |> List.map parse_column in
  let filtered_columns =
    match Config.get_github_project_columns () with
    | Some names -> columns |> List.filter (fun (c : column) -> List.mem c.name names)
    | None -> columns
  in
  let* issue_numbers' =
    filtered_columns |> List.map get_issue_numbers_in_column_async |> Lwt.all
  in
  Lwt.return (List.flatten issue_numbers')
;;

let get_project_issue_numbers () = get_project_issue_numbers_async () |> Lwt_main.run

let get_project_issues_async () =
  let* issue_numbers = get_project_issue_numbers_async () in
  get_issues_throttled issue_numbers
;;

let get_project_issues () = get_project_issues_async () |> Lwt_main.run

let populate_column_name ?project_issue_numbers issue =
  match issue.column with
  | Some _ -> issue
  | None ->
    let project_data =
      match project_issue_numbers with
      | None -> get_project_issue_numbers ()
      | Some x -> x
    in
    let col_name = List.assoc_opt issue.number project_data in
    { issue with column = col_name }
;;
