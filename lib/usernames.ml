
module FRaw = ForecastRaw

(* let forecast_details (p: FRaw.person) (oc) =

  (* if email exists the email, else ''*)
  let email = match (p.email) with
    | Some x -> x
    | None -> "" in

  Printf.fprintf oc "%s\n" (p.first_name ^ ", " ^ p.last_name ^ ", " ^ 
  (string_of_int p.id) ^ ", " ^  
  email ^ ", " ^  
  (string_of_bool p.archived) )
;;

let get_forecast_names = 
  let people = FRaw.get_people () in

  (* filter to non-archived people *)
  let people = List.filter (fun (x: FRaw.person) -> not x.archived) people in
  
  (* write to csv *)
  let oc = open_out "forecast_people.csv" in
  Printf.fprintf oc "%s\n" "Forecast first_name, Forecast last_name, Forecast username, Turing email, Archived";
  let _dummy = people |>  List.map (fun x -> forecast_details (x) oc) in
  close_out oc;
;;
*)

let get_github_names (p: GithubRaw.person) = 
  match (p.name) with
    | Some x -> String.lowercase_ascii(x)
    | None -> "" 
;; 


let create_github_string (p: GithubRaw.person) = 
  let name = match (p.name) with
    | Some x -> x
    | None -> "" in
  let email = match (p.email) with
    | Some x -> x
    | None -> "" in
  (p.login ^ ", " ^ name ^ ", " ^ email  )
;; 

let contains_kg s1 s2 =
  let re = Str.regexp_string (String.lowercase_ascii(s2))
  in
      try ignore (Str.search_forward re (String.lowercase_ascii(s1)) 0); true
      with Not_found -> false
;;

let create_forecast_string (p: FRaw.person) = 
  let email = match (p.email) with
    | Some x -> x
    |  None -> "" in

  (* check if any p.roles contains REG *)
  let is_reg = List.exists (fun x -> contains_kg (x) "REG") p.roles in

  (* collapse roles into one string *)
  let roles = String.concat ", " p.roles in
  let roles = Str.global_replace (Str.regexp ",") "/*" roles in

  p.first_name ^ ", "
  ^ p.last_name ^ ", " ^ 
  (string_of_int p.id) ^ ", " ^ 
  email ^ ", " ^  
  (string_of_bool is_reg) ^ ", " ^
  roles ^ ", " ^
  (string_of_bool p.archived)
;;

(* check if using initials (fuzzy) *)
let check_fuzzy (login) (surname) (firstname) = 
  let matched_surname = contains_kg (login) (surname) in
  let matched_firstname = contains_kg (login) (firstname) in

  let first_initial = String.sub (firstname) 0 1 in
  let sur_initial = String.sub (surname) 0 1 in
  let login_initial = String.lowercase_ascii(String.sub (login) 0 1) in

  let login_length = String.length login - 1 in
  let login_tail = String.lowercase_ascii( String.make 1 login.[login_length] ) in
 
  (* check if lower case first letter of login is equal to initial *)
  let matched_first_initial = first_initial = login_initial in
  let matched_sur_initial = sur_initial = login_tail in

  (matched_surname && matched_first_initial) || (matched_firstname && matched_sur_initial)
;;



(* For each person, try to get the matching Github user *)
let print_matched_details (p: FRaw.person) (github_people) (oc) =

  (* if email exists the email, else ''*)
  let email = match (p.email) with
    | Some x -> x
    | None -> "" in

  let forecast_match_name = (String.lowercase_ascii(p.first_name) ^ " " ^ String.lowercase_ascii(p.last_name) ) in

  let github_emails = List.map (fun (x: GithubRaw.person) -> ( Option.get x.email) ) github_people in
  let github_logins = List.map (fun (x: GithubRaw.person) -> ( x.login )) github_people in
  let github_names = List.map (fun (x: GithubRaw.person) -> ( get_github_names( x) ) ) github_people in
  
  (* Get the github person with that email address *)
  let matched_email = if email = "" then false else List.mem email github_emails in
  let matched_name = List.mem forecast_match_name github_names in 
  
  (* check if any github_logins contain the last_name as a substring *)
  let surname = String.lowercase_ascii(p.last_name) in
  let firstname = String.lowercase_ascii(p.first_name) in

  let matched_fuzzy_login = List.exists (fun x -> check_fuzzy (x) (surname) (firstname) ) github_logins in


  (* if matched_name then
    let github_person = List.find (fun (x: GithubRaw.person) -> ( get_github_names (x) ) = forecast_match_name ) github_people in *)
  let github_string = 
    if matched_email then
      let github_person = List.find (fun (x: GithubRaw.person) -> ( Option.get (x.email) ) = email ) github_people in 
      create_github_string (github_person)
    else if matched_name then
      let github_person = List.find (fun (x: GithubRaw.person) -> ( get_github_names (x) ) = forecast_match_name ) github_people in 
      create_github_string (github_person)
    else if matched_fuzzy_login then
      let github_person = List.find (fun (x: GithubRaw.person) -> check_fuzzy (x.login) (surname) (firstname) ) github_people in 
      create_github_string (github_person)
    else (" , , ") in

    let match_method = 
      if matched_email then "email"
      else if matched_name then "name"
      else if matched_fuzzy_login then "fuzzy login"
      else "-" in

  (* if email exists the email, else '' *)
  Printf.fprintf oc "%s\n" ( create_forecast_string (p) ^ ", " ^  match_method ^ ", " ^ github_string );

  String.sub github_string 0 (String.index github_string ',')
;;





let record_usernames = 
  let forecast_people = FRaw.get_people () in
  let github_people = GithubRaw.get_users () in
  let github_logins = List.map (fun (x: GithubRaw.person) -> ( x.login )) github_people in

  (* Print the matched information to a csv*)
  let oc = open_out "matched_people.csv" in
  Printf.fprintf oc "%s\n" ("Forecast first_name, Forecast last_name, Forecast username" ^ 
  ", Turing email, REG member, Role, Archived, Matched method" ^ 
  ", Github login, Github name, Github email");
  let matched_github_users = forecast_people 
    |> List.map (fun x -> print_matched_details (x) (github_people) (oc))
    |> List.filter (fun x -> x <> " ") 
    |> List.filter (fun x -> (List.mem x github_logins) ) in
  close_out oc;


  (* Record the unmatched github usernames - those in the repo, but not forecast*)
  let unmatched_github_users = List.filter (fun (x: GithubRaw.person) -> not (List.mem (x.login) matched_github_users) ) github_people in
  let oc = open_out "unmatched_github_users.csv" in
  Printf.fprintf oc "%s\n" ("Github login, Github name, Github email, Annotated name, Member of REG");
  unmatched_github_users 
    |> List.iter (fun x -> Printf.fprintf oc "%s\n" (create_github_string (x)));

  print_endline ("I've made a record of the unmatched github users in " ^
                  "unmatched_github_users.csv. You can annotate their names " ^
                  "manually, then I will try matching again.")

;;