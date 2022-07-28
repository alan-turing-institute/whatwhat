(** Placeholder: attempts to download clients from Forecast *)

open Whatwhat

let () =
  let clients = ForecastRaw.getClients () in
  let issues = Github.get_project_issues "NowWhat Test Project" in
  print_endline "Obtained clients:";
  List.iter (fun c -> print_endline @@ ForecastRaw.show_client c) clients;
  print_endline "Obtained issues:";
  List.iter (fun c -> print_endline @@ Github.show_issue c) issues
