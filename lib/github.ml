open Batteries
open Cohttp
open Cohttp_lwt_unix

(* ---------------------------------------------------------------------- *)

type issue = { number: int;
      title: string;
      body: string;
      state: string }
[@@deriving show, of_yojson] [@@yojson.allow_extra_fields]

type column = { name: string;
      cards: (issue * string) list; } (* string is cursor, would be nice to have a type alias for cursor *)
[@@deriving show, of_yojson] [@@yojson.allow_extra_fields]

type project = { number: int;
      name: string;
      columns: column list; }
[@@deriving show, of_yojson] [@@yojson.allow_extra_fields]

type project_root = { projects: project list }
[@@deriving show, of_yojson] [@@yojson.allow_extra_fields]

(* ---------------------------------------------------------------------- *)

let github_graph_ql_endpoint = "https://api.github.com/graphql"
let project_board = "Project Tracker"

(* TODO: async?
Query Github GraphQL endpoint
body is json with GraphQL query element
https://docs.github.com/en/graphql/guides/forming-calls-with-graphql#communicating-with-graphql
*)
let run_github_query (git_hub_token: string) body =
    (* TODO The next line must be prettiable with the right use of @@ or |> or
       similar *)
    let header = Header.prepend_user_agent (Header.add_authorization (Header.init ()) (Auth.credential_of_string git_hub_token)) "NowWhat" in
    let body_obj = Cohttp_lwt.Body.of_string body in
    let uri = Uri.of_string github_graph_ql_endpoint in
    let response = Client.post ~headers:header ~body:body_obj uri in
    response

(* Format JSON query to enable correct parsing on Github's side *)
let format_query (q: string) = Str.global_replace (Str.regexp "\n") "" q

(* ---------------------------------------------------------------------- *)

let read_query_template () =
     open_in "./queries/issues-by-project-graphql.json" |> BatIO.lines_of |> BatEnum.fold (^) ""
     (* TODO Should we close the stream? *)


let get_project_issues (project_name : string) = 
    (* the parent function is only wrapping up the recursive call that deals
       with paging of the responses *)
    let github_token = Config.settings.githubToken in

    let rec get_project_issues_page (project_name: string) cursor (acc: (issue * string) list) =
        let query_template = read_query_template () in

        (* fill in placeholders into the query - project board name and cursor for paging *)
        let query =
            let cursor_query =
                let cursor_exp = Str.regexp "CURSOR" in
                match cursor with
                | None ->
                    (* Get first batch *)
                    Str.global_replace cursor_exp "null" query_template
                | Some crs ->
                    (* Get subsequent batches *)
                    (* TODO Figure out how to do the string interpolation. *)
                    Str.global_replace cursor_exp "\\\"{crs}\\\"" query_template
            in

            (* TODO Figure out how to do the string interpolation. *)
            Str.global_replace (Str.regexp "PROJECTNAME") "\\\"{project_name}\\\"" cursor_query |> format_query in

        let result = run_github_query github_token query in

        (* TODO: Continue translating from F# here on down *)
        let issues: project_root =
            match result |> Decode.from_string project_root_decoder with
            | Ok issues -> issues
            | Error _ -> failwith "Failed to decode"
        in

        let issue_data: (issue * string) list =
            issues.projects.Head.columns
            |> list.collect (fun column -> column.cards) 
        in

        (* Cursor points to the last item returned, used for paging of the requests *)
        let next_cursor =
            if issue_data.Length = 0 then
                None
            else
                issue_data 
                |> list.last 
                |> (fun (_, c) -> Some c)
            in

        match next_cursor with
        | Some _ -> 
            return! (get_project_issues_page project_name next_cursor (list.append acc issue_data))
        | None -> 
            return (list.append acc issue_data)
        in

    let! all_issues = get_project_issues_page project_name None [] in
    all_issues |> list.map fst
