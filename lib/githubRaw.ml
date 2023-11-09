open Yojson
open Lwt.Syntax

(** HTTP requests ------------------------------------ *)

type http_method =
  | GET
  | POST
  | PUT
  | DELETE

type accept =
  | Json
  | Raw
  | Html

let run_github_query_async
  ?(as_bot = false)
  ?(http_method = GET)
  ?(accept = Json)
  ?(params = [])
  ?(headers = [])
  ?(body = "")
  uri
  =
  let open Cohttp in
  let open Cohttp_lwt_unix in
  let github_token =
    if as_bot then Config.get_githubbot_token () else Config.get_github_token ()
  in
  let auth_cred = Auth.credential_of_string ("Bearer " ^ github_token) in
  let accept_header_val =
    match accept with
    | Json -> "application/vnd.github+json"
    | Raw -> "application/vnd.github.raw"
    | Html -> "application/vnd.github.html"
  in
  let header_obj =
    Header.of_list
      (headers @ [ "Accept", accept_header_val; "X-GitHub-Api-Version", "2022-11-28" ])
    |> fun headers ->
    Header.add_authorization headers auth_cred
    |> fun headers -> Header.prepend_user_agent headers "Whatwhat"
  in
  let body_obj = Cohttp_lwt.Body.of_string body in
  let uri = Uri.add_query_params (Uri.of_string uri) params in
  let* body =
    Lwt.catch
      (fun () ->
        let* r, b =
          match http_method with
          | GET -> Client.get ~headers:header_obj uri
          | POST -> Client.post ~headers:header_obj ~body:body_obj uri
          | PUT -> Client.put ~headers:header_obj ~body:body_obj uri
          | DELETE -> Client.delete ~headers:header_obj ~body:body_obj uri
        in
        Utils.check_http_response r;
        Lwt.return b)
      (function
       | Failure _ -> failwith "GitHub HTTP request failed."
       | Utils.HttpError e -> failwith ("GitHub HTTP request failed: " ^ e)
       | Utils.GithubRateLimitError e ->
         failwith ("Hit GitHub rate limit. Try again at " ^ e)
       | exn -> Lwt.fail exn)
  in
  let* body_string = Cohttp_lwt.Body.to_string body in
  match accept with
  | Json ->
    let body_json =
      match body_string with
      | "" -> `Null
      | _ -> Basic.from_string body_string
    in
    Lwt.return body_json
  | Raw -> Lwt.return (`String body_string)
  | Html -> Lwt.return (`String body_string)
;;

let run_github_query
  ?(as_bot = false)
  ?(http_method = GET)
  ?(accept = Json)
  ?(params = [])
  ?(headers = [])
  ?(body = "")
  uri
  =
  run_github_query_async ~as_bot ~http_method ~accept ~params ~headers ~body uri |> Lwt_main.run
;;

(** Users -------------------------------------------- *)

type person =
  { login : string
  ; name : string option
  ; email : string option
  }

let person_of_json json =
  let open Yojson.Basic.Util in
  { login = json |> member "login" |> to_string
  ; name = json |> member "name" |> to_string_option
  ; email = json |> member "email" |> to_string_option
  }
;;

let rec get_assignable_usernames_async ?(page = 1) () =
  (* These lines allow errors in missing config/secrets to be deferred until
     this promise is actually run. Otherwise, a missing config/secret file will
     cause the programme to fail even when the config/secret is not needed, e.g.
     when using the --help option. See #84. *)
  let* github_repo_owner, github_repo_name =
    try Lwt.return (Config.get_github_repo_owner (), Config.get_github_repo_name ()) with
    | Config.MissingConfig s -> Lwt.fail (Config.MissingConfig s)
    | Config.MissingSecret s -> Lwt.fail (Config.MissingSecret s)
  in
  let batch_get page =
    let uri =
      String.concat
        "/"
        [ Config.github_url; "repos"; github_repo_owner; github_repo_name; "assignees" ]
    in
    let params = [ "per_page", [ "100" ]; "page", [ string_of_int page ] ] in
    let* cards = run_github_query_async ~params uri in
    let users =
      cards
      |> Basic.Util.to_list
      |> List.map (fun u -> u |> Basic.Util.member "login" |> Basic.Util.to_string)
    in
    Lwt.return users
  in
  let* first_batch = batch_get page in
  if List.length first_batch = 100
  then
    let* next_batch = get_assignable_usernames_async ~page:(page + 1) () in
    Lwt.return (first_batch @ next_batch)
  else Lwt.return first_batch
;;

let get_person_async username =
  let person_uri = String.concat "/" [ Config.github_url; "users"; username ] in
  let* person_json = run_github_query_async person_uri in
  Lwt.return (person_of_json person_json)
;;

let get_all_users_async =
  let* usernames = get_assignable_usernames_async () in
  List.map get_person_async usernames |> Utils.all_throttled
;;

let find_person_by_login login =
  let* all_users = get_all_users_async in
  Lwt.return (List.find_opt (fun p -> p.login = login) all_users)
;;

(** Issues ------------------------------------------- *)

(** An issue is either open or closed. *)
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
  ; labels : string list
  ; assignees : string list
  }

let get_issue_async id =
  let open Yojson.Basic.Util in
  let issue_uri =
    String.concat
      "/"
      [ Config.github_url
      ; "repos"
      ; Config.get_github_repo_owner ()
      ; Config.get_github_repo_name ()
      ; "issues"
      ; string_of_int id
      ]
  in
  let* issue_json = run_github_query_async issue_uri in
  let assignee_usernames =
    issue_json
    |> member "assignees"
    |> to_list
    |> List.map (fun a -> a |> member "login" |> to_string)
  in
  Lwt.return
    { number = id
    ; title = issue_json |> member "title" |> to_string
    ; body = issue_json |> member "body" |> to_string
    ; state = issue_json |> member "state" |> to_string |> state_of_string
    ; assignees = assignee_usernames
    ; labels =
        issue_json
        |> member "labels"
        |> to_list
        |> List.map (fun j -> j |> member "name" |> to_string)
    }
;;

let get_issue id = get_issue_async id |> Lwt_main.run

(** Issues with reactions ---------------------------- *)

(** A GitHub issue, but with reactions. We have a separate type for this because
    fetching reactions is a separate request to the GitHub API, and we don't
    want to indiscriminately do this. *)
type issue_r =
  { issue : issue
  ; reactions : (string * person) list
  }

(* Like column cards, we can only fetch 100 reactions at a time. This function
   therefore fetches the first page of reactions (which can contain up to 100
   results) and then fetches a second page if 100 results were returned. *)
let rec get_reactions_async ?(page = 1) id =
  let batch_get page id =
    let uri =
      String.concat
        "/"
        [ Config.github_url
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
      then Lwt.return None
      else (
        let rxn = r |> member "content" |> to_string in
        let* psn = login |> to_string |> find_person_by_login in
        match rxn, psn with
        | p, Some q -> Lwt.return (Some (p, q))
        | p, None ->
          Lwt.return (Some (p, { login = login |> to_string; name = None; email = None })))
    in
    let* reactions_opt =
      reactions_json |> Basic.Util.to_list |> List.map parse_reaction |> Lwt.all
    in
    let reactions = List.filter_map Fun.id reactions_opt in
    Lwt.return reactions
  in
  let* first_batch = batch_get page id in
  if List.length first_batch = 100
  then
    let* next_batch = get_reactions_async ~page:(page + 1) id in
    Lwt.return (first_batch @ next_batch)
  else Lwt.return first_batch
;;

(* Fetch reactions for multiple issues at a time. *)
let get_multiple_reactions_async ids =
  let* reactions = List.map get_reactions_async ids |> Utils.all_throttled in
  Lwt.return (List.combine ids reactions |> List.to_seq |> Domain.IntMap.of_seq)
;;

let get_multiple_reactions ids = get_multiple_reactions_async ids |> Lwt_main.run

let get_issue_r_async id =
  let* issue = get_issue_async id in
  let* reactions = get_reactions_async id in
  Lwt.return { issue; reactions }
;;

let get_issue_r id = get_issue_r_async id |> Lwt_main.run

(* GitHub only allows for 100 items to be read from a column in a single
   request. *)
let rec get_issue_numbers_in_column_async ?(page = 1) col_id =
  let batch_get page col_id =
    let get_issue_number card_json : int option =
      let open Yojson.Basic.Util in
      match card_json |> member "content_url" |> to_string_option with
      | Some s ->
        (try
           Some (String.split_on_char '/' s |> List.rev |> List.hd |> int_of_string)
         with
         | _ -> failwith "GitHub column card had invalid content_url string")
      | None -> None
    in
    let uri =
      String.concat
        "/"
        [ Config.github_url; "projects"; "columns"; string_of_int col_id; "cards" ]
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

(** Columns ------------------------------------------ *)

type column =
  { name : string
  ; id : int
  ; issues : issue list
  }

(** The same as a column, but issues are represented only by their issue number. *)
type column_n =
  { name : string
  ; id : int
  ; issues_n : int list
  }

(** The same as a column, but issues additionally contain reactions. *)
type column_r =
  { name : string
  ; id : int
  ; issues_r : issue_r list
  }

(** Fetch an entire column, but only return issue numbers instead of issues
    themselves. *)
let get_column_n name id =
  let* issues_n = get_issue_numbers_in_column_async id in
  Lwt.return { name; id; issues_n }
;;

(** Projects ----------------------------------------- *)

type project =
  { id : int
  ; name : string
  ; columns : column list
  }

(** The same as a project, but issues are represented only by their issue
    number. *)
type project_n =
  { id : int
  ; name : string
  ; columns_n : column_n list
  }

(** The same as a project, but issues additionally contain reactions. *)
type project_r =
  { id : int
  ; name : string
  ; columns_r : column_r list
  }

(** Get the integer ID of a named project tracker in a repository. *)
let get_project_id project_name =
  let uri =
    String.concat
      "/"
      [ Config.github_url
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
     | [] -> failwith "Project not found"
     | [ x ] -> x
     | _ -> failwith "More than one project with given name found")
;;

(** Get (name, id) pairs for each column in a project. *)
let get_column_names_and_ids project_id =
  let uri =
    String.concat
      "/"
      [ Config.github_url; "projects"; string_of_int project_id; "columns" ]
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

(** A promise to fetch an entire project, but only with issue numbers. *)
let get_project_n_async () =
  let name = Config.get_github_project_name () in
  let* proj_id = get_project_id name in
  let* col_info = get_column_names_and_ids proj_id in
  let* columns_n = Lwt.all (List.map (fun (name, id) -> get_column_n name id) col_info) in
  Lwt.return { id = proj_id; name; columns_n }
;;

(** Fetch an entire project, but only with issue numbers. *)
let get_project_n () = get_project_n_async () |> Lwt_main.run

(** A promise to fetch an entire project. *)
let get_project_async () =
  let* project_num = get_project_n_async () in
  let get_colname_issuenum_pairs (col : column_n) =
    List.map (fun i -> col.name, i) col.issues_n
  in
  let get_issue_with_colname (col, num) =
    let* issue = get_issue_async num in
    Lwt.return (col, issue)
  in
  let* colname_issue_pairs =
    project_num.columns_n
    |> List.concat_map get_colname_issuenum_pairs
    |> List.map get_issue_with_colname
    |> Utils.all_throttled
  in
  let reconstruct_col (col : column_n) =
    let issues =
      colname_issue_pairs |> List.filter (fun (n, _) -> n = col.name) |> List.map snd
    in
    { name = col.name; id = col.id; issues }
  in
  Lwt.return
    { id = project_num.id
    ; name = project_num.name
    ; columns = List.map reconstruct_col project_num.columns_n
    }
;;

(** Fetch an entire project. *)
let get_project () = get_project_async () |> Lwt_main.run

(** A promise to fetch an entire project, additionally with issue reactions. *)
let get_project_r_async () =
  let* project_num = get_project_n_async () in
  let get_colname_issuenum_pairs (col : column_n) =
    List.map (fun i -> col.name, i) col.issues_n
  in
  let get_issue_rxn_with_colname (col, num) =
    let* issue_rxn = get_issue_r_async num in
    Lwt.return (col, issue_rxn)
  in
  let* colname_issue_pairs =
    project_num.columns_n
    |> List.concat_map get_colname_issuenum_pairs
    |> List.map get_issue_rxn_with_colname
    |> Utils.all_throttled
  in
  let reconstruct_col (col : column_n) =
    let issues_r =
      colname_issue_pairs |> List.filter (fun (n, _) -> n = col.name) |> List.map snd
    in
    { name = col.name; id = col.id; issues_r }
  in
  Lwt.return
    { id = project_num.id
    ; name = project_num.name
    ; columns_r = List.map reconstruct_col project_num.columns_n
    }
;;

let get_project_r () = get_project_r_async () |> Lwt_main.run
