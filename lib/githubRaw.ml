(* Queries Github API and traverses the GraphQL results. Validation is done in github.ml
 *)

open Batteries
open Cohttp
open Cohttp_lwt_unix
open Yojson

(* ---------------------------------------------------------------------- *)
(* TYPES *)

exception QueryError of string

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
(* PARSERS *)
(* Parsers for the JSON returned by the Github API *)

(* Convenient bindings for functions we use repeatedly when parsing the JSON returned by
   the API. *)
let member = Basic.Util.member
let member_to_string (str : string) json = json |> member str |> Basic.Util.to_string
let member_to_int (str : string) json = member str json |> Basic.Util.to_int

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
  { number = x |> member_to_int "number"
  ; title = x |> member_to_string "title"
  ; body = x |> member_to_string "body"
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

(** Query the Github GraphQL API with the given authentication token and request body.
    Return the body of the response as a JSON object, or raise a HttpError or a QueryError
    if something goes wrong. *)
let run_github_query (git_hub_token : string) request_body =
  let auth_cred = Auth.credential_of_string ("Bearer " ^ git_hub_token) in
  let header =
    Header.init ()
    |> (fun header -> Header.add_authorization header auth_cred)
    |> fun header -> Header.prepend_user_agent header "NowWhat"
  in
  let request_body_obj = Cohttp_lwt.Body.of_string request_body in
  let uri = Uri.of_string github_graph_ql_endpoint in
  let response, response_body =
    Client.post ~headers:header ~body:request_body_obj uri |> Lwt_main.run
  in
  let () = Utils.check_http_response response in
  let response_body_json =
    response_body |> Cohttp_lwt.Body.to_string |> Lwt_main.run |> Basic.from_string
  in
  let errors = Basic.Util.member "errors" response_body_json in
  (* member returns `Null if the field isn't found *)
  match errors with
  | `Null -> response_body_json
  | _ -> raise @@ QueryError (Basic.to_string errors)
;;

let issue_query_template_path = "./queries/issues-by-project-graphql.graphql"
let user_query_template_path = "./queries/users.graphql"

(* Remove line breaks from a string. This is for formatting the API query to enable
   correct parsing on Github's side *)
let remove_line_breaks (q : string) = Str.global_replace (Str.regexp "\n") "" q

let read_file_as_string filepath =
  let channel = open_in filepath in
  let return_string = channel |> BatIO.lines_of |> BatEnum.fold ( ^ ) "" in
  let () = close_in channel in
  return_string
;;

(* The Github API query's body is built by taking a template and replacing some
   placeholders with the project board name and cursor for paging. *)
let build_issue_query project_name cursor =
  let query_template = read_file_as_string issue_query_template_path in
  let cursor_query =
    let to_replace = Str.regexp "CURSOR" in
    let replace_with =
      match cursor with
      | None -> "null" (* Get first batch *)
      | Some crs -> "\\\"" ^ crs ^ "\\\"" (* Get subsequent batches *)
    in
    Str.global_replace to_replace replace_with query_template
  in

  let to_replace = Str.regexp "PROJECTNAME" in
  let replace_with = "\\\"" ^ project_name ^ "\\\"" in
  Str.global_replace to_replace replace_with cursor_query |> remove_line_breaks
;;

(* These are the main functions of this module, that would be called externally. *)

let get_users () =
  let github_token = Config.get_github_token () in
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

(* Recursively get the API query results page by page, and accumulate them into [acc]. *)
let rec get_project_issues_page
  (project_name : string)
  (github_token : string)
  cursor
  (acc : (issue * string) list)
  =
  let issue_query = build_issue_query project_name cursor in
  let body_json = run_github_query github_token issue_query in

  (* Parse the response into a list of issues and accumulate them. *)
  let issues = body_json |> project_root_of_json in
  (* TODO Why only the head? Might there be more projects? *)
  let issue_data =
    issues.projects
    |> List.hd
    |> (fun x -> x.columns)
    |> List.map (fun column -> column.cards)
    |> List.concat
  in
  let new_acc = acc @ issue_data in

  (* Cursor points to the last item returned, used for paging of the requests *)
  let next_cursor =
    if List.length issue_data = 0
    then None
    else issue_data |> List.last |> fun (_, c) -> Some c
  in

  match next_cursor with
  | Some _ -> get_project_issues_page project_name github_token next_cursor new_acc
  | None -> new_acc
;;

(* The external-facing function for getting issues, with arguments needed for recursion
   hidden away in get_project_issues_page. *)
let get_project_issues (project_name : string) =
  let github_token = Config.get_github_token () in
  let all_issues = get_project_issues_page project_name github_token None [] in
  List.map fst all_issues
;;
