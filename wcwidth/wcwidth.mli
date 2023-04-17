(** Decompose a string into a list of Unicode characters. *)
val to_utf8 : string -> Uchar.t list

(** Get the display width of a Unicode character. Returns -1 if the character is
    unprintable. (Note that there isn't actually a good way to actually {i
    input} a single Unicode character. For example, you can't use a character
    literal. You can use {!to_utf8}, though.) *)
val wcwidth : Uchar.t -> int

(** Get the display width of a string. Returns -1 if any of the characters
    within it are unprintable. Note especially that line feeds and carriage
    returns WILL return -1, so make sure the string you're passing doesn't
    contain those. *)
val wcswidth : string -> int
