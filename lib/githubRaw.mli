type person = { login : string; name : string option; email : string option }

type issue = {
  number : int;
  title : string;
  body : string;
  state : string;
  assignees : person list;
  reactions : (string * person) list;
}

type column = { name : string; cards : (issue * string) list }
type project = { number : int; name : string; columns : column list }
type project_root = { projects : project list }

val show_issue : issue -> string
val show_column : column -> string
val show_project : project -> string
val show_project_root : project_root -> string
val get_project_issues : string -> issue list
