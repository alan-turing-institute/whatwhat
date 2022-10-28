(* Queries Github API and traverses the GraphQL results. Validation is done in github.ml
 *)

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
  ; labels : string list
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
let convert_each = Basic.Util.convert_each
let member_to_string (str : string) json = json |> member str |> Basic.Util.to_string
let member_to_int (str : string) json = member str json |> Basic.Util.to_int

let person_of_json json =
  { login = json |> member_to_string "login"
  ; name = json |> member "name" |> Basic.Util.to_string_option
  ; email = json |> member "email" |> Basic.Util.to_string_option
  }
;;

let issue_of_json column_name json =
  json
  |> member "node"
  |> member "content"
  |> fun x ->
  { number = x |> member_to_int "number"
  ; title = x |> member_to_string "title"
  ; body = x |> member_to_string "body"
  ; state = x |> member_to_string "state"
  ; column = Some column_name
  ; assignees =
      x
      |> member "assignees"
      |> member "edges"
      |> convert_each (fun y -> y |> member "node" |> person_of_json)
  ; labels =
      x
      |> member "labels"
      |> member "edges"
      |> convert_each (fun y -> y |> member "node" |> member_to_string "name")
  ; reactions =
      x
      |> member "reactions"
      |> member "edges"
      |> convert_each (fun y ->
           ( y |> member "node" |> member_to_string "content"
           , y |> member "node" |> member "user" |> person_of_json ))
  }
;;

let column_of_json json =
  let name = json |> member "node" |> member_to_string "name" in
  let cards =
    json
    |> member "node"
    |> member "cards"
    |> member "edges"
    |> convert_each (fun y -> issue_of_json name y, y |> member_to_string "cursor")
  in
  { name; cards }
;;

let project_of_json json =
  json
  |> member "node"
  |> fun x ->
  { number = x |> member_to_int "number"
  ; name = x |> member_to_string "name"
  ; columns = x |> member "columns" |> member "edges" |> convert_each column_of_json
  }
;;

let project_root_of_json json =
  { projects =
      json
      |> member "data"
      |> member "repository"
      |> member "projects"
      |> member "edges"
      |> convert_each project_of_json
  }
;;

(* ---------------------------------------------------------------------- *)
(* QUERYING GITHUB *)
(* See
   https://docs.github.com/en/graphql/guides/forming-calls-with-graphql#communicating-with-graphql
*)

(** Query the Github GraphQL API with the given authentication token and request body.
    Return the body of the response as a JSON object, or raise a HttpError or a QueryError
    if something goes wrong. *)
let run_github_query (git_hub_token : string) request_body =
  let github_graph_ql_endpoint = Config.get_github_url () in
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
  let errors = member "errors" response_body_json in
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
  let channel = Batteries.open_in filepath in
  let return_string = channel |> BatIO.lines_of |> BatEnum.fold ( ^ ) "" in
  let () = Batteries.close_in channel in
  return_string
;;

(* Take a list of pairs of [regexp * string], and run [Str.global_replace] on each pair on
   the string [str].*)
let replace_multiple replacements str =
  let folder query (to_replace, replace_with) =
    Str.global_replace to_replace replace_with query
  in
  List.fold_left folder str replacements
;;

(* The Github API query's body is built by taking a template and replacing some
   placeholders with the project board name and cursor for paging. *)
let build_issue_query project_name cursor =
  let query_template = read_file_as_string issue_query_template_path in
  (* List of pairs (part of the template to replace, value to replace it with). *)
  let replacements =
    [ ( Str.regexp "CURSOR"
      , match cursor with
        | None -> "null" (* Get first batch *)
        | Some crs -> "\\\"" ^ crs ^ "\\\"" (* Get subsequent batches *) )
    ; Str.regexp "PROJECTNAME", "\\\"" ^ project_name ^ "\\\""
    ; Str.regexp "REPO_NAME", "\\\"" ^ Config.get_github_repo_name () ^ "\\\""
    ; Str.regexp "REPO_OWNER", "\\\"" ^ Config.get_github_repo_owner () ^ "\\\""
    ]
  in
  replace_multiple replacements query_template |> remove_line_breaks
;;

(* These are the main functions of this module, that would be called externally. *)

let get_users () =
  let github_token = Config.get_github_token () in
  let query_template = read_file_as_string user_query_template_path in
  let replacements =
    [ Str.regexp "REPO_NAME", "\\\"" ^ Config.get_github_repo_name () ^ "\\\""
    ; Str.regexp "REPO_OWNER", "\\\"" ^ Config.get_github_repo_owner () ^ "\\\""
    ]
  in
  let query = replace_multiple replacements query_template in
  let body_json = run_github_query github_token query in
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

(* The next cursor is the last cursor from the column with the most cards. If no cards are
   in the project given, then [None].*)
let find_next_cursor issues =
  issues.projects
  |> List.hd
  |> (fun x -> x.columns)
  |> List.map (fun column ->
       let cards = column.cards in
       let length = List.length cards in
       let last_cursor =
         if length > 0 then Batteries.List.last cards |> fun (_, c) -> Some c else None
       in
       length, last_cursor)
  |> List.fold_left
       (fun (max_length, max_cursor) (this_length, this_cursor) ->
         if this_length > max_length
         then this_length, this_cursor
         else max_length, max_cursor)
       (min_int, None)
  |> snd
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
    |> List.concat_map (fun column -> column.cards)
  in
  let new_acc = acc @ issue_data in

  (* The cursor is used for paging of the requests *)
  let next_cursor = find_next_cursor issues in
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
