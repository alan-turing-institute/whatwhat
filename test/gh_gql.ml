open Whatwhat

let trim s =
  let l = String.length s in
  String.sub s 0 (min l 20)

let () = 
  let open GithubRaw in
  print_endline "Using GithubRaw i.e. (GraphQL)";
  let open GithubTypes in
  let issues = get_project_issues "Project Tracker" in
  issues |> List.sort (fun (i1 : issue) i2 -> compare i1.number i2.number)
         |> List.map (fun (i : issue) -> {i with body = trim i.body})
         |> List.iter (fun i -> i |> show_issue |> print_endline)
