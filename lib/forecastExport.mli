(** Functionality for exporting Forecast CSVs. *)

(** Generate the weekly Forecast project schedule between [start_date] and
    [end_date]. *)
val export_project_schedule
  :  start_date:CalendarLib.Date.t
  -> end_date:CalendarLib.Date.t
  -> Csv.t

(** Generate the weekly Forecast team schedule between [start_date] and
    [end_date]. *)
val export_team_schedule
  :  start_date:CalendarLib.Date.t
  -> end_date:CalendarLib.Date.t
  -> Csv.t
