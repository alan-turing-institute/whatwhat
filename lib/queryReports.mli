(** Query_reports is used to query reactions for a specific person or issue on 
    Github.

    In general, for issues on the project board, emojis are gathered. Then they 
    are filtered to either a specific person or issue. These reactions are then 
    returned in a table.    
 *)

module Raw = GithubRaw

(** [print_issue issue] prints the [issue] summary. *)
val print_issue : col_name:string -> Raw.issue_r -> unit

(** [test_issue_number] Determines whether the input issue identifier is an 
int (i.e. the issue number) or the issue title. *)
val test_issue_number : int -> Raw.issue_r -> bool

(** Filter issues by title to those matching the input [issue_title] string. *)
val test_issue_title : string -> Raw.issue_r -> bool

(** [get_name] returns a [Github.person] name. If no name is provided then the 
    login is used. *)
val get_name : Raw.person -> string

(** [get_title] Returns an issue's title. *)
val get_title : Raw.issue_r -> string

(** [test_person_name] check if this issue contain reactions from name. *)
val test_person_name : string -> Raw.issue_r -> Raw.issue_r option

(** [issue_summary] returns the issue summary: number, title, state, column. *)
val issue_summary : string -> string * Raw.issue_r

(** The types of emoji reactions we care about. **)
type emoji =
  | LAUGH
  | THUMBS_UP
  | THUMBS_DOWN
  | OTHER

(** [refactor_emoji] turns emoji strings into emoji. *)
val refactor_emoji : string -> emoji

(** The number of characters in the longest emoji string. *)
val max_emoji_length : int

val half_cell : string

(** The format/contents of an 'empty' cell in the output table. *)
val empty_cell : string

(** The format/contents of a 'ticked' cell in the output table. *)
val occupied_cell : string

(** Ranks the emojis (most liked to least) so they appear ordered in the 
    table. *)
val compare_emojis : string -> string -> int

(** Orders a people's names alphabetically *)
val compare_names : Raw.person -> Raw.person -> int

(** [get_outcome] converts the emoji responses to the approriate table format *)
val get_outcome : emoji -> string

(** Creates string of responses cells for the table body. *)
val body_list : int -> string -> string -> string

(** prints a horizontal line in the table, of appropriate length based on the 
    number of chararcters in the name or emoji strings. *)
val border_line : int -> int -> string

(** Creates a string for the header row *)
val header_line : int -> int -> string

(** [get_reaction_table] returns all strings required for the table summarising
    an issue's reactions.  *)
val get_reaction_table : Raw.issue_r -> string * string * string list

(** Subset an issue's reactions to only those by [name] *)
val get_person_reaction : Raw.issue_r -> string -> emoji list

(** Return the number of reactions by [name] *)
val get_person_reaction_n : Raw.issue_r -> string -> int

(** Return strings required to build table summarising a person's reactions *)
val person_summary : string -> string * string * string list * string list

(** Print the table of reactions for user: [target] *)
val individuals_reactions : string -> unit

(** Print the table of reactions for issue: [target] *)
val issues_reactions : string -> unit
