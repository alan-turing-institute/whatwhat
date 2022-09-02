(** A module with two functions: [dateprinter], which pretty-prints a [CalendarLib.Date]
    object, and [dateprint_opt] which does the same for a [CalendarLib.Date option],
    returning ["None"] for [None].
    *)
open CalendarLib

let dateprinter fmt (dt : Date.t) = Format.pp_print_string fmt (Printer.Date.to_string dt)

let dateprint_opt (dt : Date.t option) =
  match dt with
  | Some x -> Printer.Date.to_string x
  | None -> "None"
;;
