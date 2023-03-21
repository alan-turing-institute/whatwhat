module Raw = GithubRaw

(* Function for printing issue summary*)
let print_issue ~(col_name : string) (i : Raw.issue_with_reactions) =
  print_endline " ";
  print_endline
    ("Issue number: "
    ^ string_of_int i.issue.number
    ^ " (https://github.com/"
    ^ Config.get_github_repo_owner ()
    ^ "/"
    ^ Config.get_github_repo_name ()
    ^ "/issues/"
    ^ string_of_int i.issue.number
    ^ ")");
  print_endline ("Issue title: " ^ i.issue.title);
  print_endline ("State: " ^ Raw.show_issue_state i.issue.state);
  print_endline ("Column: " ^ col_name)
;;

(* filter list by issue number *)
let test_issue_number (issue_number : int) (i : Raw.issue_with_reactions) =
  i.issue.number = issue_number
;;

let test_issue_title (issue_title : string) (i : Raw.issue_with_reactions) =
  (* ignores cases and whitespace *)
  String.trim (String.lowercase_ascii i.issue.title)
  = String.trim (String.lowercase_ascii issue_title)
;;

(* For each element in all_names, get the name *)
(* 
  TODO: have a look-up for these names 
*)
let get_name (single_person : Raw.person) =
  if single_person.name <> None
  then Option.get single_person.name
  else single_person.login
;;

let get_title (i : Raw.issue_with_reactions) = i.issue.title

(* check if this issue contain reactions from name*)
let test_person_name (name : string) (i : Raw.issue_with_reactions) =
  let all_names =
    i.reactions
    |> List.map (fun r -> r |> snd |> get_name |> String.lowercase_ascii |> String.trim)
  in

  let formatted_name = String.trim (String.lowercase_ascii name) in

  (* ignores cases and whitespace *)
  if List.mem formatted_name all_names then Some i else None
;;

(* Get the issue summary: number, title, state, column*)
let issue_summary lookup_term =
  let proj = GithubRaw.get_project_reactions () in
  let cols = proj.columns_rxn in
  let make_issues (col : Raw.column_reactions) =
    List.map (fun i -> col.name, i) col.issue_reactions
  in
  let issues = List.concat_map make_issues cols in
  let issues_subset =
    if String.for_all Utils.is_digit lookup_term
    then
      List.filter
        (fun x -> x |> snd |> test_issue_number (int_of_string lookup_term))
        issues
    else List.filter (fun x -> x |> snd |> test_issue_title lookup_term) issues
  in
  match issues_subset with
  | [] ->
    failwith
      (Printf.sprintf
         "No issue found for %s. Make sure you are searching by issue number (e.g. 1214) \
          or issue title (e.g. Molecular Biology).\n"
         lookup_term)
  | x :: _ -> x
;;

type emoji =
  | LAUGH
  | THUMBS_UP
  | THUMBS_DOWN
  | OTHER

(* Possible emoji responses*)
let refactor_emoji e =
  match e with
  | "laugh" -> LAUGH
  | "+1" -> THUMBS_UP
  | "-1" -> THUMBS_DOWN
  | _ -> OTHER
;;

(* Table cell dimensions: project specific table *)
let max_emoji_length = String.length "THUMBS_DOWN" + 2
let half_cell = String.make (max_emoji_length / 2) ' '
let empty_cell = String.make (max_emoji_length + 3) ' '
let occupied_cell = half_cell ^ "  x " ^ half_cell

(* make sure laugh first, then thumbs up, then thumbs down then other *)
let compare_emojis a c =
  let a2 = refactor_emoji a in
  let c2 = refactor_emoji c in
  compare a2 c2
;;

(* Order alphabetically on a person's name*)
let compare_names a c =
  let a2 = get_name a in
  let c2 = get_name c in
  compare a2 c2
;;

(* Refactor the emojis for table *)
let get_outcome (emoji : emoji) =
  match emoji with
  | LAUGH ->
    String.concat "|" [ ""; occupied_cell; empty_cell; empty_cell; empty_cell; "" ]
  | THUMBS_UP ->
    String.concat "|" [ ""; empty_cell; occupied_cell; empty_cell; empty_cell; "" ]
  | THUMBS_DOWN ->
    String.concat "|" [ ""; empty_cell; empty_cell; occupied_cell; empty_cell; "" ]
  | OTHER ->
    String.concat "|" [ ""; empty_cell; empty_cell; empty_cell; occupied_cell; "" ]
;;

(* Create string of cells for table body *)
let body_list (max_string : int) (name : string) (emoji_cells : string) =
  "| " ^ name ^ String.make (max_string - String.length name) ' ' ^ " " ^ emoji_cells
;;

(* Print line*)
let border_line (name_length : int) emoji_length =
  "| "
  ^ String.make name_length '-'
  ^ " | "
  ^ String.make (emoji_length + 1) '-'
  ^ " | "
  ^ String.make (emoji_length + 1) '-'
  ^ " | "
  ^ String.make (emoji_length + 1) '-'
  ^ " | "
  ^ String.make (emoji_length + 1) '-'
  ^ " |"
;;

let header_line (max_name_length : int) (max_emoji_length : int) =
  "| Name"
  ^ String.make (max_name_length - 4) ' '
  ^ " | :laugh: "
  ^ String.make (max_emoji_length - String.length ":laugh:") ' '
  ^ " | :thumbs up: "
  ^ String.make (max_emoji_length - String.length ":thumbs up:") ' '
  ^ " | :thumbs down: "
  ^ String.make (max_emoji_length - String.length ":thumbs down:") ' '
  ^ " | other "
  ^ String.make (max_emoji_length - String.length "other") ' '
  ^ " |"
;;

(* Get the reactions *)
(* 
  TODO: collapse people with multiple reactions (example: issue 1216) 
  This probably involves counting the reactions instead
  then refactoring the table according to the counts. 
*)
let get_reaction_table (issue : Raw.issue_with_reactions) =
  (* Get issue reactions then sort by most love -> least love, 
     then alphabetically *)
  let issue_reactions =
    issue.reactions
    |> List.sort (fun (_a, b) (_c, d) -> compare_names b d)
    |> List.sort (fun (a, _b) (c, _d) -> compare_emojis a c)
  in

  (* Get all emoji reactions and names *)
  let all_emoji, all_names = List.split issue_reactions in
  let all_emoji = List.map refactor_emoji all_emoji in
  let all_names = List.map get_name all_names in

  (* Find the longest name for cell size *)
  let max_name_length = List.fold_left (fun x y -> max x (String.length y)) 0 all_names in

  (* table format emojis*)
  let table_format_emojis = List.map (fun x -> get_outcome x) all_emoji in
  let table_body =
    List.map2 (fun x y -> body_list max_name_length x y) all_names table_format_emojis
  in

  let bl = border_line max_name_length max_emoji_length in
  let hl = header_line max_name_length max_emoji_length in

  bl, hl, table_body
;;

let get_person_reaction (i : Raw.issue_with_reactions) (name : string) =
  (* Get only the reactions of the person *)
  i.reactions
  |> List.filter (fun (_a, b) -> get_name b = name)
  |> List.map (fun r -> r |> fst |> refactor_emoji)
;;

let get_person_reaction_n (i : Raw.issue_with_reactions) (name : string) =
  (* Get only the reactions of the person *)
  let reactions =
    i.reactions |> List.filter (fun (_a, b) -> get_name b = name) |> List.map fst
  in
  List.length reactions
;;

let person_summary (name : string) =
  let proj = GithubRaw.get_project_reactions () in
  let cols = proj.columns_rxn in
  let issues =
    List.concat_map (fun (c : Raw.column_reactions) -> c.issue_reactions) cols
  in
  let issues_subset = issues |> List.filter_map (fun x -> test_person_name name x) in
  (* Print outputs*)
  if List.length issues_subset = 0
  then
    raise
      (Failure
         ("No issues found for '"
         ^ name
         ^ "'. Make sure you are spelling the name correctly. "));

  (* Get all issue names: those reacted to, and those not*)
  let all_issues_names = issues |> List.map (fun x -> get_title x) in
  let issues_subset_names = issues_subset |> List.map (fun x -> get_title x) in

  (* Get the difference between the two lists: those not reacted to*)
  let difference =
    List.filter (fun x -> not (List.mem x issues_subset_names)) all_issues_names
  in

  (* Create list of project reactions *)
  let persons_reactions =
    issues_subset |> List.map (fun x -> get_person_reaction x name) |> List.flatten
  in
  (* This line repeats the project name to create long format tables in cases 
     where someone reacts multiple times on the same issue *)
  let project_names =
    List.map2
      (fun x y -> Batteries.List.make (get_person_reaction_n x name) y)
      issues_subset
      issues_subset_names
    |> List.flatten
  in

  let max_name_length =
    List.fold_left (fun x y -> max x (String.length y)) 0 project_names
  in
  let table_format_emojis = List.map (fun x -> get_outcome x) persons_reactions in

  let table_body =
    List.map2 (fun x y -> body_list max_name_length x y) project_names table_format_emojis
  in

  (* print table *)
  let bl = border_line max_name_length max_emoji_length in
  let hl = header_line max_name_length max_emoji_length in

  bl, hl, table_body, difference
;;

let individuals_reactions target =
  let bl, hl, table_body, difference = person_summary target in

  (* print the person's reactions *)
  print_endline
    ("\n"
    ^ target
    ^ " has reacted to "
    ^ string_of_int (List.length table_body)
    ^ " issues:\n");

  print_endline bl;
  print_endline hl;
  print_endline bl;
  List.iter print_endline table_body;
  print_endline bl;

  print_endline
    ("\nThey have not reacted to "
    ^ string_of_int (List.length difference)
    ^ " issues: "
    ^ String.concat ", " difference)
;;

let issues_reactions target =
  let col_name, issue = issue_summary target in

  print_endline "";
  print_issue ~col_name issue;

  let bl, hl, table_body = get_reaction_table issue in

  print_endline
    ("There are "
    ^ string_of_int (List.length table_body)
    ^ " reactions for this issue:\n");

  (* Print the table *)
  print_endline bl;
  print_endline hl;
  print_endline bl;
  List.iter print_endline table_body;
  print_endline bl
;;
