type what =
  | Panic
  | Error
  | Warn
  | Info

(* TODO: While we get going, just print to stdout. *)
type destination = Console

let string_of_what = function
  | Panic -> "Panic"
  | Error -> "Error"
  | Warn -> "Warn"
  | Info -> "Info"
;;

let defaultLogger _ lvl msg = Printf.printf "Whatwhat: %5s: %s" (string_of_what lvl) msg
let theLogger = ref defaultLogger

(* "Every problem in computer science can be solved through another layer of
   indirection" *)
let log (dst : destination) (lvl : what) (msg : string) : unit = !theLogger dst lvl msg
