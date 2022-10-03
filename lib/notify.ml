(** Notify is used to tell certain people about certain problems
    
   

    The plan is to emit a GitHub comment of the following form:
    {v
    Hi there, this is WhatWhat Bot. I'm a bot. Beep boop!

    I am unable to read the metadata block for this project. Please see [url]
    for details of how this block should be formatted. Among the problems I
    encountered were:

    I couldn't find the block at all. It should be in the body of the issue. 
    I couldn't find these fields:
       - etc
    I couldn't understand the value of these fields:
       - so forth    
    v}
    
 *)

type target =
  | NoTarget
  | GitHub
  | Slack
  | All

let dump_event (e : Log.event) =
  Printf.printf "%s: Module %s reports: %s\n"
    (Log.show_level e.level)
    (Log.show_source e.source)
    e.message

(* Dump all logged events to standard out *)
let dump_the_log () =
  Seq.iter dump_event @@ Log.get_the_log ()
  
  

