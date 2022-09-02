open CalendarLib

let dateprinter fmt (dt : Date.t) = Format.pp_print_string fmt (Printer.Date.to_string dt)

let dateprint_opt (dt : Date.t option) =
  match dt with
  | Some x -> Printer.Date.to_string x
  | None -> "None"
;;
