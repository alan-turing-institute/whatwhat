module DateMap : module type of Map.Make (CalendarLib.Date)

val export_schedule
  : start_date:CalendarLib.Date.t
  -> end_date:CalendarLib.Date.t
  -> Csv.t
