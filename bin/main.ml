(** Entry point for the whatwhat executable.
    Use [whatwhat --help] for usage instructions.
    *)

open Whatwhat
open Cmdliner

(** whatwhat export ... *)
let ww_export
  (start_date_in : string option)
  (end_date_in : string option)
  (output_file : string option)
  : unit
  =
  (* Determine start and end points. We export full weeks only, so the start
     must be a Monday and the end a Sunday. This mimics the interface provided
     on the Forecast website. *)
  let open CalendarLib.Date in
  let start_date =
    Utils.rollback_week
      (match start_date_in with
       | None -> Utils.default_start_date ()
       | Some "_" -> Utils.default_start_date ()
       | Some s -> Utils.parse_date s)
  in
  let end_date =
    Utils.rollforward_week
      ~with_weekend:true
      (match end_date_in with
       | None -> Utils.default_end_date ()
       | Some "_" -> Utils.default_end_date ()
       | Some s -> Utils.parse_date s)
  in
  if start_date >= end_date then failwith "Start date cannot be before end date!" else ();

  (* Calculate the schedule *)
  let csv = ForecastExport.export_schedule ~start_date ~end_date in

  (* Determine output file *)
  let output =
    match output_file with
    | Some "" ->
      let start_str = CalendarLib.Printer.Date.sprint "%Y-%m-%d" start_date in
      let end_str = CalendarLib.Printer.Date.sprint "%Y-%m-%d" end_date in
      Some ("forecast-project-export-from-" ^ start_str ^ "-to-" ^ end_str ^ ".csv")
    | Some f -> Some f
    | None -> None
  in

  (* Print some useful info *)
  let module ANSI = ANSITerminal in
  ANSI.eprintf [ Bold ] "Forecast export\n";
  ANSI.eprintf [ Bold ] "---------------\n";
  ANSI.eprintf [ Bold ] "Start date      ";
  ANSI.eprintf [] "%s\n" (CalendarLib.Printer.Date.to_string start_date);
  ANSI.eprintf [ Bold ] "End date        ";
  ANSI.eprintf [] "%s\n" (CalendarLib.Printer.Date.to_string end_date);
  ANSI.eprintf [ Bold ] "Output location ";
  ANSI.eprintf [] "%s\n" (Option.value output ~default:"standard output");

  (match output with
   | Some fname -> Csv.save fname csv
   | None -> Csv.print csv);

  Printf.eprintf "Done.\n"
;;

(* command-line options for [whatwhat export] *)
let start_date_arg : string option Term.t =
  Arg.(value & pos 0 (some string) None & info ~docv:"START" ~doc:"Start date" [])
;;

let end_date_arg : string option Term.t =
  Arg.(value & pos 1 (some string) None & info ~docv:"END" ~doc:"End date" [])
;;

(** Returns [None] if the [-o] option was completely absent, [Some ""] if the
    [-o] option was provided without an argument, or [Some s] if the [-o] option
    was provided with the argument [s]. *)
let output_file_arg : string option Term.t =
  Arg.(
    value
    & opt ~vopt:(Some "") (some string) None
    & info ~docv:"OUTPUT" ~doc:"Output file" [ "o"; "output" ])
;;

let ww_export_cmd : unit Cmd.t =
  Cmd.v
    (Cmd.info "export" ~doc:"Export Forecast CSVs")
    Term.(const ww_export $ start_date_arg $ end_date_arg $ output_file_arg)
;;

(* ------------------------------- *)

let ww_main notify person issue =
  let people, projects, assignments = Schedule.get_the_schedule () in
  print_endline "Whatwhat downloaded:";
  Printf.printf "%d people; " (List.length people);
  Printf.printf "%d projects; and " (List.length projects);
  Printf.printf "%d assignments\n\n" (List.length assignments);

  (* notification reports*)
  if notify <> Notify.NoTarget
  then (
    if (* Emit errors and warnings *)
       notify = Notify.All || notify = Notify.Github
    then
      print_endline "CATCH: this would post reports to github."
      (*To post to github replace CATCH string with 
       Notify.post_metadata_reports ()*)
    else if notify = Notify.Print
    then Notify.print_metadata_reports ())
  else print_endline "No notifications requested.";

  (* query person's reactions*)
  if person <> "none"
  then QueryReports.individuals_reactions person
  else print_endline "No person queried.";

  (* query issue reactions*)
  if issue <> "none"
  then QueryReports.issues_reactions issue
  else print_endline "No issue queried."
;;

(* Capture the arguments *)
let notify_arg =
  let tgs =
    Arg.enum
      [ "github", Notify.Github
      ; "slack", Notify.Slack
      ; "all", Notify.All
      ; "none", Notify.NoTarget
      ; "print", Notify.Print
      ]
  in
  let doc =
    "Where to send notifications.\n\
    \           $(docv) may be $(b,print), $(b,github), $(b,slack), $(b,all), or \
     $(b,none)."
  in

  Arg.(value & opt tgs Notify.NoTarget & info [ "n"; "notify" ] ~docv:"NOTIFY" ~doc)
;;

let person_arg =
  let doc = "Name of person to query. $(docv) must be a string argument." in
  Arg.(value & opt string "none" & info [ "p"; "person" ] ~docv:"PERSON" ~doc)
;;

let issue_arg =
  let doc =
    "Issue to query for team reactions. \n\
    \             Can be entered as issue title or number, \n\
    \             but must be a string argument."
  in
  Arg.(value & opt string "none" & info [ "i"; "issue" ] ~docv:"ISSUE" ~doc)
;;

let ww_main_term : unit Term.t =
  Term.(const ww_main $ notify_arg $ person_arg $ issue_arg)
;;

(* -------------------------------- *)

(** Define default command and other subcommands *)

let cmd : unit Cmd.t =
  Cmd.group
    ~default:ww_main_term
    (Cmd.info "whatwhat" ~doc:"Report current project status")
    [ ww_export_cmd ]
;;

let () = exit (Cmd.eval cmd)
