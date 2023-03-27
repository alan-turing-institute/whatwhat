open Domain

let ftes_of_assignment (asn : assignment) =
  let a = asn.allocation in
  a |> DateMap.bindings |> List.map snd |> FTE.sum |> FTE.weeks
;;

let ftes_of_assignments (id : int) (asns : assignment list) : (person * FTE.time) list =
  asns
  |> List.filter (fun a -> a.project.number = id)
  |> List.map (fun a -> a.person, ftes_of_assignment a)
;;

let print_assignments (prj : project) (asns : assignment list) : unit =
  Printf.printf "Project %d: %s\n" prj.number prj.name;

  match List.filter (fun a -> a.project.number = prj.number) asns with
  | [] -> Printf.printf "No assignments to project %d found.\n" prj.number
  | asns' ->
    List.iter
      (fun asn ->
        print_endline asn.person.full_name;
        print_endline (CalendarLib.Printer.Date.to_string (get_first_day asn.allocation));
        print_endline (CalendarLib.Printer.Date.to_string (get_last_day asn.allocation));
        print_endline (FTE.show_time (ftes_of_assignment asn)))
      asns'
;;
