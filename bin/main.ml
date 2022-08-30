(** Placeholder: attempts to download the schedule from Forecast *)

open Whatwhat

let () =
  print_endline "Downloading schedule from Forecast...";
  let theSchedule = Forecast.getTheCurrentSchedule 180 in
  print_endline "Obtained:";
  Printf.printf
    "%d projects; %d people; and %d assignments\n"
    (Forecast.IntMap.cardinal theSchedule.projects)
    (Forecast.StringMap.cardinal theSchedule.people)
    (List.length theSchedule.assignments);
  print_endline "Obtaining issues from  Github:";
  let issues = Github.get_project_issues "Project Tracker" in
  Printf.printf "Obtained %d issues" (List.length issues);
;;