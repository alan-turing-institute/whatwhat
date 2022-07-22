(** Placeholder: attempts to download clients from Forecast *)

open Whatwhat

let () =
  let clients = ForecastRaw.getClients () in
  print_endline "Obtained:";
  List.iter (fun c -> print_endline @@ ForecastRaw.show_client c) clients
