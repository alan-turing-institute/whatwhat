(** Retrieves and validates all objects from Forecast. Assembles these into a
    schedule.
    
    A {i schedule} is:
    - A set of {i projects};
    - A set of {i people};
    - A set of {i assignments} (of people to projects);

*)

open CalendarLib

type project =
  { number       : int; (* Ought to be a GitHub issue number *)
    name         : string;
    client       : string;
  }
(** A [project] represents what is known about a project from Forecast.

    There may be multiple Forecast projects with the same GitHub issue
    number. These are amalgamated into one [project]. 
    
*) 

(* type people *)

(* type assignment *)

type schedule =
  {
    projects : project list;
    (* team        : people list; *)
    (* assignments : assignment list *)
  }

 
val getTheSchedule : Date.t -> Date.t -> schedule
