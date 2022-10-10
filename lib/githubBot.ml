(** Functions for posting to GitHub *)

open Cohttp
open Cohttp_lwt
open Cohttp_lwt_unix
open Yojson

let github_post repo issue post_body =
  let _, body =
    let headers =
      Header.of_list
        [ "Accept", "application/vnd.github+json"
        ; "Authorization", "Bearer " ^ Config.settings.githubbot_token
        ]
    and uri =
      Uri.of_string
        ("https://api.github.com/repos/alan-turing-institute/"
        ^ repo
        ^ "/issues/"
        ^ string_of_int issue
        ^ "/comments")
    and body = Body.of_string @@ Safe.to_string (`Assoc [ "body", `String post_body ]) in
    Client.post ~headers ~body uri |> Lwt_main.run
  in
  Lwt_main.run @@ Body.to_string body
;;
