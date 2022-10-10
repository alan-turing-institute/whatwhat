(** Placeholder: attempts to download the schedule from Forecast *)

(* Note: to run under dune and pass arguments use
   dune exec -- whatwhat <args>
 *)

open Whatwhat

let whatwhat target =
  let people, projects, assignments = Schedule.get_the_schedule () in
  print_endline "Whatwhat downloaded:";
  Printf.printf " %d people; " (List.length people);
  Printf.printf "%d projects; and " (List.length projects);
  Printf.printf "%d assignments\n\n" (List.length assignments);

  (* Emit errors and warnings *)
  if target = Notify.All || target = Notify.Github
  then Notify.post_metadata_reports ()
  else Notify.print_metadata_reports ()
;;

(* if target = Notify.Slack || target = Notify.All *)
(* ... *)
(*   |> Slack.post; *)

(* Command-line interface *)

open Cmdliner

let target =
  let tgs =
    Arg.enum
      [ "github", Notify.Github
      ; "slack", Notify.Slack
      ; "all", Notify.All
      ; "none", Notify.NoTarget
      ]
  in
  let doc =
    "Where to send notifications.\n\
    \             $(docv) may be $(b,github), $(b,slack), $(b,all), or $(b,none)."
  in

  Arg.(value & opt tgs Notify.NoTarget & info [ "t"; "target" ] ~docv:"TARGET" ~doc)
;;

let cmd =
  Cmd.v
    (Cmd.info "whatwhat" ~doc:"Report current project status")
    Term.(const whatwhat $ target)
;;

let main () = exit (Cmd.eval cmd)
let () = main ()
