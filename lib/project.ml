open Domain

let ftes_of_assignment (asn : assignment) =
  let a = asn.allocation in
  a |> DateMap.bindings |> List.map snd |> FTE.sum_over_days
;;

let ftes_of_assignments (prj : project) (asns : assignment list)
  : (person * FTE.time) list
  =
  asns
  |> List.filter (fun a -> a.project.number = prj.number)
  |> List.map (fun a -> a.person, ftes_of_assignment a)
;;

let print_assignments_to (prj : project) (asns : assignment list) : unit =
  Printf.printf "Project %d: %s\n" prj.number prj.name;

  match List.filter (fun a -> a.project.number = prj.number) asns with
  | [] -> Printf.printf "No assignments to project %d found.\n" prj.number
  | asns' ->
    List.iter
      (fun asn ->
        Printf.printf
          "%s: %s to %s, %s\n"
          asn.person.full_name
          (CalendarLib.Printer.Date.to_string (get_first_day asn.allocation))
          (CalendarLib.Printer.Date.to_string (get_last_day asn.allocation))
          (FTE.show_time (ftes_of_assignment asn)))
      asns'
;;

let check_assignment_sum (prj : project) (asns : assignment list) =
  (* Accept up to 10% discrepancy *)
  let wiggle_room = 0.1 in

  let total_fte_time = ftes_of_assignments prj asns |> List.map snd |> FTE.sum_time in
  let budget = prj.plan.budget in

  Printf.printf "Total allocations found    : %s\n" (FTE.show_time total_fte_time);
  Printf.printf "Total allocations expected : %s\n" (FTE.show_time budget);

  if FTE.compare_time total_fte_time (FTE.mul_time budget (1. -. wiggle_room)) < 0
     || FTE.compare_time total_fte_time (FTE.mul_time budget (1. +. wiggle_room)) > 0
  then (
    print_endline "uhoh";
    print_assignments_to prj asns)
;;
