module DateMap : module type of Map.Make (CalendarLib.Date)

type assignment_output =
  { client : ForecastRaw.client
  ; project : ForecastRaw.project
  ; entity : ForecastRaw.entity
  ; hours_per_week : float DateMap.t
  }

val export_schedule
  : start_date:CalendarLib.Date.t
  -> end_date:CalendarLib.Date.t
  -> Csv.t
