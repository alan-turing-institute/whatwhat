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

type issue_state =
  | Open
  | Closed
[@@deriving show]

let state_of_string = function
  | "open" -> Open
  | "closed" -> Closed
  | _ -> failwith "Invalid JSON for state"
;;

type issue =
  { number : int
  ; title : string
  ; body : string
  ; state : issue_state
  ; assignees : person list
  ; reactions : (string * person) list
  ; labels : string list
  }
[@@deriving show]

type column =
  { name : string
  ; id : int
  ; issues : issue list
  }
[@@deriving show]

type project =
  { id : int
  ; name : string
  ; columns : column list
  }
[@@deriving show]

(* Parsers -------------------------------------------------------------- *)

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

(* Generic functions for querying GitHub APIs --------------------------- *)

(* TODO: Check for errors in body JSON before returning; member returns `Null if
   the field isn't found. The code below only works if an object is returned; it fails if an array of objects is returned (e.g. when getting a list of issues
   in a column *)
(* let errors = Basic.Util.member "errors" body_json in *)
(* match errors with *)
(* | `Null -> Lwt.return body_json *)
(* | _ -> raise @@ QueryError (Basic.to_string errors) *)
let run_github_query_async ?(http_method = GET) ?(params = []) ?(body = "") uri =
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
    match http_method with
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

let run_github_query ?(http_method = GET) ?(params = []) ?(body = "") uri =
  run_github_query_async ~http_method ~params ~body uri |> Lwt_main.run
;;

let all_users =
  let query =
    Printf.sprintf
      {|{ "query": "query { repository(owner: \"%s\", name: \"%s\") { assignableUsers(first: 100) { edges { node { login name email } } } } }" } |}
      (Config.get_github_repo_owner ())
      (Config.get_github_repo_name ())
  in
  let github_graph_ql_endpoint = Config.get_github_url () ^ "/graphql" in
  let body_json =
    run_github_query ~http_method:POST ~body:query github_graph_ql_endpoint
  in
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

let get_issue_async id =
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
    ; state = issue_json |> member "state" |> to_string |> state_of_string
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
    }
;;

let get_issue id = get_issue_async id |> Lwt_main.run
let get_issues_async ids = ids |> List.map get_issue_async |> Lwt.all

(** Get a batch of issues, but with some throttling such that only
    [max_concurrent_issues] requests are fired off at any one time. This is
    necessary because asking GitHub for >300 issues at a go leads to
    Fatal error: exception Failure("TLS to non-TCP currently unsupported:
      host=api.github.com endp=(Unknown \"name resolution failed\")". *)
let rec get_issues_throttled (ids : int list) =
  let rec splitAt n xs =
    match n, xs with
    | _, [] -> [], []
    | 1, x :: x' -> [ x ], x'
    | n, x :: x' ->
      let y1, y2 = splitAt (n - 1) x' in
      x :: y1, y2
  in
  let max_concurrent_issues = 150 in
  match splitAt max_concurrent_issues ids with
  | [], [] -> Lwt.return []
  | xs, [] -> get_issues_async xs
  | xs, ys ->
    let* first_batch = get_issues_async xs in
    let* the_rest = get_issues_throttled ys in
    Lwt.return (first_batch @ the_rest)
;;

(* GitHub only allows for 100 items to be read from a column in a single
   request. *)
let rec get_issue_numbers_in_column_async ?(page = 1) col_id =
  let batch_get page col_id =
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
        [ Config.get_github_url (); "projects"; "columns"; string_of_int col_id; "cards" ]
    in
    let params = [ "per_page", [ "100" ]; "page", [ string_of_int page ] ] in
    let* cards = run_github_query_async ~params uri in
    let issue_numbers = cards |> Basic.Util.to_list |> List.filter_map get_issue_number in
    Lwt.return issue_numbers
  in
  let* first_batch = batch_get page col_id in
  if List.length first_batch = 100
  then
    let* next_batch = get_issue_numbers_in_column_async ~page:(page + 1) col_id in
    Lwt.return (first_batch @ next_batch)
  else Lwt.return first_batch
;;

let get_project_id project_name =
  let uri =
    String.concat
      "/"
      [ Config.get_github_url ()
      ; "repos"
      ; Config.get_github_repo_owner ()
      ; Config.get_github_repo_name ()
      ; "projects"
      ]
  in
  let* projects = run_github_query_async ~http_method:GET uri in
  let extract_id json =
    if json |> Basic.Util.member "name" |> Basic.Util.to_string = project_name
    then Some (json |> Basic.Util.member "id" |> Basic.Util.to_int)
    else None
  in
  Lwt.return
    (match projects |> Basic.Util.to_list |> List.filter_map extract_id with
     (* TODO: raise fatal errors instead *)
     | [] -> failwith "Project not found"
     | [ x ] -> x
     | _ -> failwith "More than one project with given name found")
;;

let get_column (name, id) =
  let* issue_numbers = get_issue_numbers_in_column_async id in
  let* issues = get_issues_throttled issue_numbers in
  Lwt.return { name; id; issues }
;;

let get_column_ids_and_names project_id =
  let uri =
    String.concat
      "/"
      [ Config.get_github_url (); "projects"; string_of_int project_id; "columns" ]
  in
  let* resp = run_github_query_async uri in
  let entries = resp |> Basic.Util.to_list in
  let get_id col = col |> Basic.Util.member "id" |> Basic.Util.to_int in
  let get_name col = col |> Basic.Util.member "name" |> Basic.Util.to_string in
  let filtered_entries =
    match Config.get_github_project_columns () with
    | Some names -> entries |> List.filter (fun c -> List.mem (get_name c) names)
    | None -> entries
  in
  Lwt.return (filtered_entries |> List.map (fun c -> get_name c, get_id c))
;;

let get_project_async () =
  let name = Config.get_github_project_name () in
  let* proj_id = get_project_id name in
  let* col_info = get_column_ids_and_names proj_id in
  let* columns = Lwt.all (List.map get_column col_info) in
  Lwt.return { id = proj_id; name; columns }
;;

let get_project () = get_project_async () |> Lwt_main.run
