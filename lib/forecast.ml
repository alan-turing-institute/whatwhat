open CalendarLib

module Raw = ForecastRaw

let (>>=) = Option.bind
let (>|=) o m = Option.map m o

(* Matches "hut23-NNN" where NNN is a number *)
let hut23_code_re = 
     Re.compile Re.(seq [start; str "hut23-"; group (rep1 digit); stop])

type project =
  { number       : int; (* Ought to be a GitHub issue number *)
    name         : string;
    client       : string;
  }
  
type schedule =
  {
    projects : project list;
    (* team        : people list; *)
    (* assignments : assignment list *)
  }

(** Emit a notification for an invalid project *)
let notify_of_invalid_project (_ : Raw.project) : unit =
  assert false

let extract_project_code (cd : string option) =
    cd
    >>= Re.exec_opt hut23_code_re
    >>= fun gp -> Re.Group.get_opt gp 1
    >|= int_of_string 

let validate_project (p : Raw.project) : project option = 
  if p.archived then None else
    (* TODO: This out to be an Option.map *)
    match extract_project_code p.code with
    | None ->
       begin
         notify_of_invalid_project p;
         None
       end
    | Some code ->
       Some { number = code;
              name = p.name;
              client = "TODO: LOOK THIS UP";
         }
       
let getTheSchedule (startDate : Date.t) (endDate : Date.t) =
  let _, _, _, projs, _ = Raw.getTheSchedule startDate endDate in
  let valid_projs = List.filter_map validate_project projs in
  { projects = valid_projs }


    
    (* Validate projects:
       
       - remove archived projects
     - check for syntatically correct project code
     - check for existence of finance code and make a lookup table so we can later give the right code to an allocation
     - merge projects having the same code
     - ... and also side-effect notifications along the way

   *)
     

