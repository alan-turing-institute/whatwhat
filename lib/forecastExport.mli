(** Functionality for exporting Forecast CSVs. *)

(** Generate the Forecast project schedule between [start_date] and [end_date].
    *)
val export_schedule
  : start_date:CalendarLib.Date.t
  -> end_date:CalendarLib.Date.t
  -> Csv.t
