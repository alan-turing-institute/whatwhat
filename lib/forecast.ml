open CalendarLib

module Raw = ForecastRaw

let (>>=) = Option.bind

(* Matches "hut23-NNN" where NNN is a number *)
let hut23_code_re = 
     Re.compile Re.(seq [start; str "hut23-"; group (rep1 digit); stop])

type project =
  { number       : int; (* Ought to be a GitHub issue number *)
    name         : string;
    client       : string option;
  }
  
type schedule =
  {
    projects : project list;
    (* team        : people list; *)
    (* assignments : assignment list *)
  }

exception InvalidProjectCode of string

(** Emit a notification for an invalid project *)
let notify_of_invalid_project (p : Raw.project) (msg : string): unit =
  print_endline @@ msg ^ " for project '" ^ p.name
                   ^ "' [" ^ (Option.value ~default:"" p.code) ^ "]"

let extract_project_code (cd : string option) =
  try 
    Option.get cd  
    |> Re.exec hut23_code_re
    |> (fun gp -> Re.Group.get gp 1)
    |> int_of_string
  with 
  | Invalid_argument _ -> raise (InvalidProjectCode "Missing project code")
  | Not_found -> raise (InvalidProjectCode "Invalid project code")
  
let validate_project clientmap (p : Raw.project) : project option = 
  (* TODO: Get rid of "Time off" project *)
  (* TODO: Add Finance code *)
  (* TODO: De-dupe projects (but assign finance codes to allocations *) 
  if p.archived then None
  else try
      let code = extract_project_code p.code in 
      Some { number = code;
             name = p.name;
             client = p.client_id
                      >>= (fun cid ->  
               List.assoc_opt cid clientmap);
        }
    with
    | InvalidProjectCode msg ->
       notify_of_invalid_project p msg;
       None 
    
let make_client_assoc (clients : Raw.client list) =
  List.map (fun (c : Raw.client) -> (c.id, c.name)) clients
             
let getTheSchedule (startDate : Date.t) (endDate : Date.t) =
  let clnts, _, _, projs, _ = Raw.getTheSchedule startDate endDate in 
  let valid_projs = List.filter_map
                      (validate_project (make_client_assoc clnts))
                      projs in
  { projects = valid_projs }

let getTheCurrentSchedule () =
  let startDate = Date.today () in 
  let endDate = Date.add startDate (Date.Period.day 180) in
 getTheSchedule startDate endDate

    (* Validate projects:
       
       - remove archived projects
     - check for syntatically correct project code
     - check for existence of finance code and make a lookup table so we can later give the right code to an allocation
     - merge projects having the same code
     - ... and also side-effect notifications along the way

   *)
     

