(** A module for pretty-printing [CalendarLib.Date] instances.*)
open CalendarLib

let string_of_date = Printer.Date.to_string

let string_of_date_opt (dt : Date.t option) =
  match dt with
  | Some x -> string_of_date x
  | None -> "None"
;;

let pp_print_date fmt dt = Format.pp_print_string fmt (string_of_date dt)
let pp_print_date_opt fmt dt = Format.pp_print_string fmt (string_of_date_opt dt)
