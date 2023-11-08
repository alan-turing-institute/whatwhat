(** Module for terminal pretty-printing utilities *)

module ANSI = ANSITerminal

(** ----- Re-export -------------------- *)

let wcwidth = Wcwidth.wcwidth
let wcswidth = Wcwidth.wcswidth

(** ----- String manipulation ---------- *)

type alignment =
  | ALeft
  | ACenter
  | ARight

(** Right-pad a string to a length of [n] with the character [fill_char]. For
    example: [pad 'a' 10 'hello' -> 'helloaaaaa]. *)
let pad_right ?(fill_char = ' ') (n : int) (s : string) : string =
  let len = wcswidth s in
  if len >= n then s else s ^ String.make (n - len) fill_char
;;

(** Left-pad a string to a length of [n] with the character [fill_char]. For
    example: [pad_left 'a' 10 'hello' -> 'aaaaahello']. *)
let pad_left ?(fill_char = ' ') (n : int) (s : string) : string =
  let len = wcswidth s in
  if len >= n then s else String.make (n - len) fill_char ^ s
;;

(** Pad a string on both sides to a length of [n] with the character
    [fill_char]. If the number of padding characters needed is odd, the left
    padding is arbitrarily chosen to be shorter. For example: [pad_center 'a' 10
    'hello' -> 'aahelloaaa']. *)
let pad_center ?(fill_char = ' ') (n : int) (s : string) : string =
  let len = wcswidth s in
  if len >= n
  then s
  else (
    let left_pad = (n - len) / 2 in
    let right_pad = n - len - left_pad in
    String.make left_pad fill_char ^ s ^ String.make right_pad fill_char)
;;

let pad ?(fill_char = ' ') ?(alignment = ALeft) (n : int) (s : string) : string =
  match alignment with
  | ALeft -> pad_right ~fill_char n s
  | ACenter -> pad_center ~fill_char n s
  | ARight -> pad_left ~fill_char n s
;;

(** Encase a string in a box *)
let make_box ?(alignment = ALeft) (s : string) : string =
  let lines = String.split_on_char '\n' s in
  let width = Utils.max_by ~default:0 wcswidth lines in
  let top_and_bottom_row = "+" ^ String.make (width + 2) '-' ^ "+" in
  let middle_rows = List.map (fun s -> "| " ^ pad ~alignment width s ^ " |") lines in
  String.concat "\n" ([ top_and_bottom_row ] @ middle_rows @ [ top_and_bottom_row ])
;;

(** Construct a table from a list of lists. Each nested list is one row of the
    table. The number of columns of the resulting table will correspond to the
    length of the longest nested list. *)
let make_table
  ?(header_rows : int = 0)
  ?(column_padding : int = 0)
  (rows : string list list)
  : string
  =
  (* First pad all rows to the same length *)
  let lengths = List.map List.length rows in
  let n_columns = Utils.max_by ~default:0 Fun.id lengths in
  let padded_rows =
    List.map
      (fun row ->
        if List.length row < n_columns
        then row @ List.init (n_columns - List.length row) (fun _ -> "")
        else row)
      rows
  in

  (* Then determine widths of each column *)
  let columns = Utils.transpose padded_rows in
  let widths = List.map (Utils.max_by ~default:0 wcswidth) columns in
  let padded_widths = List.map (fun w -> w + (2 * column_padding)) widths in

  (* Finally, construct the table *)
  let horizontal_border =
    "+" ^ String.concat "+" (List.map (fun w -> String.make w '-') padded_widths) ^ "+"
  in
  let make_row row =
    "|"
    ^ String.concat
        "|"
        (List.map2
           (fun w s ->
             String.make column_padding ' '
             ^ pad ~fill_char:' ' w s
             ^ String.make column_padding ' ')
           widths
           row)
    ^ "|"
  in
  let headers, remainder = Utils.split_at header_rows padded_rows in
  String.concat
    "\n"
    ([ horizontal_border ]
     @ List.map make_row headers
     @ [ horizontal_border ]
     @ List.map make_row remainder
     @ [ horizontal_border ])
;;

(** ----- Printing --------------------- *)

(** Prints a string to standard output. If ~use_color is true, then the string
    is printed with the given styles. *)
let prout ~(use_color : bool) (styles : ANSI.style list) (string : string) : unit =
  if use_color then ANSI.print_string styles string else print_string string;
  flush stdout
;;

(** Prints a string to standard error. If ~use_color is true, then the string
    is printed with the given styles. *)
let prerr ~(use_color : bool) (styles : ANSI.style list) (string : string) : unit =
  if use_color then ANSI.prerr_string styles string else prerr_string string;
  flush stderr
;;

(** Print a bold, underlined heading *)
let print_heading ~(use_color : bool) (heading : string) : unit =
  let n = wcswidth heading in
  prout ~use_color [ ANSI.Bold ] (heading ^ "\n");
  prout ~use_color [ ANSI.Bold ] (String.make n '-' ^ "\n")
;;
