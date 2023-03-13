open Whatwhat
open Cmdliner

let export start_date_in end_date_in output_file =
  let open CalendarLib.Date in
  let start_date = match start_date_in with
  | None     -> Utils.default_start_date ()
  | Some "_" -> Utils.default_start_date ()
  | Some s   -> Utils.parse_date s in
  let end_date = match end_date_in with
  | None     -> Utils.default_end_date ()
  | Some "_" -> Utils.default_end_date ()
  | Some s   -> Utils.parse_date s in

  print_endline @@ "Exporting Forecast data";
  print_endline @@ "-----------------------";
  print_endline @@ "Start date  : " ^ CalendarLib.Printer.Date.to_string start_date;
  print_endline @@ "End date    : " ^ CalendarLib.Printer.Date.to_string end_date;
  print_endline @@ "Output file : " ^ (Utils.show_opt output_file);

  if start_date >= end_date then failwith "Start date cannot be before end date!" else ();

  (* default output file *)
  let out =
      let start_str = CalendarLib.Printer.Date.sprint "%Y-%m-%d" start_date in
      let end_str = CalendarLib.Printer.Date.sprint "%Y-%m-%d" end_date in
      "forecast-team-export-from-" ^ start_str ^ "-to-" ^ end_str ^ ".csv"
  in

  (* TODO: probably better to get this function to return a string, and we dump
     it based on the value of [out].*)
  ForecastExport.export_schedule ~start_date ~end_date out;
;;

let start_date_arg =
  Arg.(value & pos 0 (some string) None & info ~docv:"START" ~doc:"Start date" [])
;;

let end_date_arg =
  Arg.(value & pos 1 (some string) None & info ~docv:"END" ~doc:"End date" [])
;;

(** Returns [None] if the [-o] option was completely absent, [Some ""] if the
    [-o] option was provided without an argument, or [Some s] if the [-o] option
    was provided with the argument [s]. *)
let output_file_arg =
  Arg.(value & opt ~vopt:(Some "") (some string) None & info ~docv:"OUTPUT" ~doc:"Output file" ["o"; "output"])
;;

let cmd =
  Cmd.v
    (Cmd.info "wwexp" ~doc:"Export Forecast CSVs")
    Term.(const export $ start_date_arg $ end_date_arg $ output_file_arg)
;;

let main () = exit (Cmd.eval cmd)
let () = main ()
