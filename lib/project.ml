open Domain

let ftes_of_assignment (asn : assignment) =
  let a = asn.allocation in
  a |> DateMap.bindings |> List.map snd |> FTE.sum_over_days
;;

let ftes_of_assignments (prj : project) (asns : assignment list)
  : (string * FTE.time) list
  =
  asns
  |> List.filter (fun a -> a.project.number = prj.number)
  |> List.map (fun a -> get_entity_name a.entity, ftes_of_assignment a)
;;

let print_assignments_to (prj : project) (asns : assignment list) : unit =
  match List.filter (fun a -> a.project.number = prj.number) asns with
  | [] -> Printf.printf "No assignments to project %d found.\n" prj.number
  | asns' ->
    List.iter
      (fun asn ->
        Printf.printf
          "%-32s %18s, %s to %s\n"
          (get_entity_name asn.entity)
          (FTE.show_time (ftes_of_assignment asn))
          (CalendarLib.Printer.Date.to_string (get_first_day asn.allocation))
          (CalendarLib.Printer.Date.to_string (get_last_day asn.allocation)))
      asns'
;;

let print_project_header prj =
  let open ANSITerminal in
  printf [Bold] "Project %d: %s\n" prj.number prj.name
;;

let print_assignments (prj : project) (asns : assignment list) =
  let total_fte_time = ftes_of_assignments prj asns |> List.map snd |> FTE.sum_time in
  let budget = prj.plan.budget in
  let discrepancy = FTE.div_time (FTE.sub_time total_fte_time budget) budget in

  print_project_header prj;
  print_assignments_to prj asns;
  print_endline (String.make 77 '-');
  Printf.printf "%-32s %18s (%+.2f%%)\n" "Allocations found" (FTE.show_time total_fte_time) (100. *. discrepancy);
  Printf.printf "%-32s %18s\n" "Allocations expected" (FTE.show_time
  budget);
;;
