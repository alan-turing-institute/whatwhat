(** Notify is used to tell certain people about certain problems

    The following individuals may receive messages:

    - Fallback (email): A hard-coded person if the Scheduler cannot be found
    - The Scheduler (GitHub): The people listed in the GitHub service area for Scheduling
    - Project Shepherd (GitHub): Anyone listed in the project GitHub issue
    - Project Team (email): Anyone allocated to the project in the next six months
    - Programme Shepherd (GitHub): Anyone listed in the Programme Service area
    - Programe Lead (Forecast): Anyone allocated to the Programme Service area in the next six months.
    - The On-Call (Forecast): The person referred to by a particular assignment

*)

type what = Panic | Error | Warn | Info

(* TODO: While we get going, just print to stdout. *)
type destination = Console

let string_of_what = function
  | Panic -> "Panic"
  | Error -> "Error"
  | Warn  -> "Warn"
  | Info  -> "Info"

let defaultNotifier _ lvl msg =
  Printf.printf "Whatwhat: %5s: %s" (string_of_what lvl) msg

let theNotifier = ref defaultNotifier

let notify (dst : destination) (lvl : what) (msg : string) : unit =
  !theNotifier dst lvl msg


            
            
  
