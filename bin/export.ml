open Whatwhat
open Cmdliner

let show_opt str_opt =
  match str_opt with
  | None -> "None"
  | Some s -> "Some " ^ s

let is_digit c =
  match c with
  | '0' .. '9' -> true
  | _ -> false

let parse_date s = 
  let digits = s |> String.to_seq |> Seq.filter is_digit |> String.of_seq in
  if String.length digits != 8 then failwith ("Unrecognised date: " ^ s) else ();
  let yyyy = int_of_string (String.sub digits 0 4) in
  let mm = int_of_string (String.sub digits 4 2) in
  let dd = int_of_string (String.sub digits 6 2) in
  let open CalendarLib.Date in
  if is_valid_date yyyy mm dd
  then make yyyy mm dd
  else failwith ("Date '" ^ s ^ "' is not valid!")

let default_start_date =
  let open CalendarLib.Date in
  let t = today () in
  match int_of_month (month t) with
  | 1 -> make (year t - 1) 12 1
  | n -> make (year t) (n - 1) 1

let default_end_date =
  let open CalendarLib.Date in
  let t = today () in
  match month t with
  | Jan -> let day = if is_valid_date (year t) 2 29 then 29 else 28
           in make (year t) 2 day
  | Feb -> make (year t) 3 31
  | Mar -> make (year t) 4 30
  | Apr -> make (year t) 5 31
  | May -> make (year t) 6 30
  | Jun -> make (year t) 7 31
  | Jul -> make (year t) 8 31
  | Aug -> make (year t) 9 30
  | Sep -> make (year t) 10 31
  | Oct -> make (year t) 11 30
  | Nov -> make (year t) 12 31
  | Dec -> make (year t + 1) 1 31

let export start_date_in end_date_in output_file =
  let start_date = match start_date_in with
  | None -> default_start_date
  | Some s -> parse_date s in
  let end_date = match end_date_in with
  | None -> default_end_date
  | Some s -> parse_date s in

  print_endline @@ "Exporting Forecast data";
  print_endline @@ "-----------------------";
  print_endline @@ "Start date  : " ^ CalendarLib.Printer.Date.to_string start_date;
  print_endline @@ "End date    : " ^ CalendarLib.Printer.Date.to_string end_date;
  print_endline @@ "Output file : " ^ (show_opt output_file);

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
