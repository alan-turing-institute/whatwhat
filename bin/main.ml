(** Placeholder: attempts to download the schedule from Forecast *)

open Whatwhat

let () =
  let people, projects = Schedule.make_schedule () in
  let () = print_endline "People:" in
  let () = List.iter (fun c -> print_endline @@ Schedule.show_person c) people in
  let () = print_endline "Number of projects:" in
  let () = print_endline @@ Int.to_string @@ List.length projects in
  let () = print_endline "Projects:" in
  let () = List.iter (fun (c : Schedule.project) -> print_endline @@ c.name) projects in
  ()
;;
