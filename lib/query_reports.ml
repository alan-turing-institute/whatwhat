module Raw = GithubRaw

(* Query the github board and subset to a specific column*)
let project_column_issues (project_board : string) (project_column : string) = 
  let all_issues = Raw.get_project_issues (project_board) in

  (* function to check the column*)
  let test_column (colname) (i : Raw.issue)  = 
    if (Option.get i.column = colname) then Some i else None in

  all_issues
    |> List.map (fun x -> test_column project_column x)
    |> List.filter (fun x -> x <> None) 
;;

(* Function for printing issue summary*)
let print_issue (i : Raw.issue) = 
  print_endline (" ");
  print_endline ("Issue number: " ^ string_of_int i.number);
  print_endline ("Issue title: " ^ i.title) ;
  print_endline ("State: " ^ i.state);
  print_endline ("Column: " ^ Option.get i.column)
;;

(* Get the issue summary: number, title, state, column*)
let issue_summary (project_column : string) (issue_number : int)= 
  let column_issues = project_column_issues "Project Tracker" project_column in
  Option.get (List.nth column_issues issue_number) 
;;

(* For each element in all_names, get the name *)
(* TO DO: have a look-up for these names *)
let get_name (single_person : Raw.person) = 
  if (single_person.name <> None) then
    Option.get single_person.name
  else 
    single_person.login
;;

(* Possible emoji responses*)
let refactor_emoji (e)= 
  match e with
  | "LAUGH" -> 0
  | "THUMBS_UP" -> 1
  | "THUMBS_DOWN" -> 2
  | _ -> 3
;;

(* Table cell dimensions *)
let max_emoji_length = (String.length "THUMBS_DOWN") + 2 ;;
let half_cell = String.make (  (max_emoji_length / 2) ) ' ';;
let empty_cell = String.make (max_emoji_length + 3 ) ' ';;
let occupied_cell = half_cell ^ "  x " ^ half_cell;;

(* make sure laugh first, then thumbs up, then thumbs down then other *)
let compare_emojis a c = 
  let a2 = refactor_emoji a in
  let c2 = refactor_emoji c in
  if (a2 = c2) then 0
  else if (a2 > c2) then 1
  else -1
;;

(* Order alphabetically on a person's name*)
let compare_names a c = 
  let a2 = get_name a in
  let c2 = get_name c in
  compare a2 c2
;;

(* Refactor the emojis for table *)
let get_outcome (emoji : string) = 
  match emoji with
  | "LAUGH" -> "|" ^ occupied_cell ^ "|" ^ empty_cell ^ "|" ^ empty_cell ^ "|" ^ empty_cell ^ "|"
  | "THUMBS_UP" -> "|" ^ empty_cell ^ "|" ^ occupied_cell ^ "|" ^ empty_cell ^ "|" ^ empty_cell ^ "|"
  | "THUMBS_DOWN" -> "|" ^ empty_cell ^ "|" ^ empty_cell ^ "|" ^ occupied_cell ^ "|" ^ empty_cell ^ "|"
  | _ -> empty_cell ^ "|" ^ empty_cell ^ "|" ^ empty_cell ^ "|" ^ occupied_cell ^ "|"
;;

(* Create string of cells for table body *)
let body_list (name : string) (emoji_cells : string)  = 
  "| " ^ name ^ String.make (20 - String.length name) ' ' ^ " " ^ 
  emoji_cells
;;

(* Print line*)
let border_line (name_length : int) (emoji_length) = 
  print_endline ("| " ^ String.make (name_length) '-' ^
  " | " ^ String.make (emoji_length + 1) '-' ^
  " | " ^ String.make (emoji_length + 1) '-' ^
  " | " ^ String.make (emoji_length + 1) '-' ^
  " | " ^ String.make (emoji_length + 1) '-' ^ " |")
;;

(* Get the reactions *)
let get_reaction_table (issue : Raw.issue) = 
  (* Get issue reactions then sort by most love -> least love, then alphabetically *)
  let issue_reactions = issue.reactions 
    |> List.sort (fun (_a, b) (_c, d) -> compare_names b d)  
    |> List.sort (fun (a, _b) (c, _d) -> compare_emojis a c) in

  (* Get all emoji reactions and names *)
  let all_emoji = List.map (fun x -> (fun (a, _b) -> a) x) issue_reactions in
  let all_names = issue_reactions in
  let all_names =  List.map (fun x -> (fun (_a, b) -> b) x) all_names in
  let all_names = List.map (fun x -> get_name x) all_names in

  (* Find the longest name for cell size *)
  let max_name_length = List.fold_left (fun x y -> max x (String.length y)) 0 all_names in

  (* table format emojis*)
  let table_format_emojis = List.map (fun x -> get_outcome x) all_emoji in

  (* Print the number of reactions*)
  print_endline ("There are " ^ string_of_int (List.length issue_reactions) ^ " reactions for this issue:\n" );
 
  (* Print the table header *)
  border_line (max_name_length) (max_emoji_length );
  print_endline ("| Name" ^ String.make (max_name_length - 4) ' ' ^ 
  " | :laugh: " ^ String.make (max_emoji_length - (String.length(":laugh:")) ) ' ' ^ 
  " | :thumbs up: " ^ String.make (max_emoji_length - (String.length(":thumbs up:")) )  ' ' ^ 
  " | :thumbs down: " ^ String.make (max_emoji_length - (String.length(":thumbs down:")) )  ' ' ^ 
  " | other " ^ String.make (max_emoji_length - String.length "other") ' ' ^ " |");

  border_line (max_name_length) (max_emoji_length );

  List.iter print_endline (List.map2 body_list all_names table_format_emojis);

  border_line (max_name_length) (max_emoji_length )
;;



