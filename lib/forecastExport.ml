let export_schedule ~start_date ~end_date output_file =
  let open Csv in

  let schedule = ForecastRaw.get_the_schedule ~start_date ~end_date in
  ignore schedule;

  let csv = [] in
  save output_file csv
