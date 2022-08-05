(** Placeholder: attempts to download the schedule from Forecast *)

open Whatwhat

let () =
  begin
    print_endline "Downloading schedule from Forecast...";
    let theSchedule = Forecast.getTheCurrentSchedule () in
    print_endline "Obtained:";
    Printf.printf "%d projects; %d people; and %d assingments\n"
      (Forecast.IntMap.cardinal theSchedule.projects)
      (Forecast.StringMap.cardinal theSchedule.people)
      (List.length theSchedule.assignments)
  end

