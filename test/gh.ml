open Whatwhat

let () =
  let open GithubRaw in
  let x = all_hut23_users in
  List.iter (fun y -> y |> show_person |> print_endline) x
