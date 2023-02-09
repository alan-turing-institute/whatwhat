open Whatwhat

let () =
  let open GithubRaw in
  let issue = get_issue 1072 in
  issue |> show_issue |> print_endline;
  let issue' = issue |> populate_column_name in
  issue' |> show_issue |> print_endline;
