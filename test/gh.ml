open Whatwhat

let () =
  let open GithubRaw in
  let x = all_users in
  List.iter (fun y -> y |> show_person |> print_endline) x
;;
