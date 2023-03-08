open Whatwhat
open Cmdliner

let show str_opt =
  match str_opt with
  | None -> "None"
  | Some s -> "Some " ^ s


let export start_date_in end_date_in output_file =
  
  (* TODO: parse the dates from strings *)
  print_endline @@ "Start date: " ^ (show start_date_in);
  print_endline @@ "End date: " ^ (show end_date_in);
  print_endline @@ "Output file: " ^ (show output_file);

  let start_date = CalendarLib.Date.make 2023 2 1 in
  let end_date = CalendarLib.Date.make 2023 4 30 in

  let out = match output_file with
  | Some f -> f
  | None -> let start_str = CalendarLib.Printer.Date.sprint "%Y-%m-%d" start_date in
            let end_str = CalendarLib.Printer.Date.sprint "%Y-%m-%d" end_date in
            "forecast-team-export-from-" ^ start_str ^ "-to-" ^ end_str ^ ".csv"
  in

  ForecastExport.export_schedule ~start_date ~end_date out;
;;

let start_date_arg =
  Arg.(value & pos 0 (some string) None & info ~docv:"START" ~doc:"Start date" [])
;;

let end_date_arg =
  Arg.(value & pos 1 (some string) None & info ~docv:"END" ~doc:"End date" [])
;;

let output_file_arg =
  Arg.(value & opt (some string) None & info ~docv:"OUTPUT" ~doc:"Output file" ["o"; "output"])
;;

let cmd =
  Cmd.v
    (Cmd.info "wwexp" ~doc:"Export Forecast CSVs")
    Term.(const export $ start_date_arg $ end_date_arg $ output_file_arg)
;;

let main () = exit (Cmd.eval cmd)
let () = main ()
