(* TODO: Change this to add the notifications to a stack *)

type log_type =
  | Error
  | Warning

let show_log_type = function
  | Error -> "Error"
  | Warning -> "Warning"
;;

(* TODO: While we get going, just print to stdout. *)
let lvl_prefix lvl = show_log_type lvl ^ ":"
let default_logger lvl msg = Printf.printf "%-8s %s\n" (lvl_prefix lvl) msg
let the_logger = ref default_logger

(* "Every problem in computer science can be solved through another layer of
   indirection" *)
let log (lvl : log_type) (msg : string) : unit = !the_logger lvl msg
