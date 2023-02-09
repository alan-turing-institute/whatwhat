open GithubRaw

let get_user_info_async login =
  let uri = "https://api.github.com/users/" ^ login in
  let github_token = Config.get_github_token () in
  let user_info = GithubRaw.run_github_query_async github_token uri in
  let open Yojson.Basic.Util in
  let open Lwt.Infix in
  user_info
  >|= fun user_info ->
  { login
  ; name = user_info |> member "name" |> to_string_option
  ; email = user_info |> member "email" |> to_string_option
  }
;;

let get_user_info login = get_user_info_async login |> Lwt_main.run

(* Note that this is slower than the graphQL query; ca 2.6 vs 0.7 seconds. *)
let hut23_users_rest =
  let open Yojson.Basic.Util in
  (* /assignees and /collaborators give the same results *)
  let uri = "https://api.github.com/repos/alan-turing-institute/Hut23/assignees" in
  let github_token = Config.get_github_token () in
  (* TODO: Github returns a maximum of 100 users at a time. There are only 82
     people for now, but in principle this can increase. To be safe we should
     also check page 2 to see if there are any more users, like this: *)
  (* let params = ["per_page", ["100"]; "page", ["2"]] in *)
  let params = [ "per_page", [ "100" ] ] in
  let users = GithubRaw.run_github_query ~params github_token uri in
  let usernames =
    users |> to_list |> List.map (fun b -> b |> member "login" |> to_string)
  in
  usernames |> List.map get_user_info_async |> Lwt.all |> Lwt_main.run
;;
