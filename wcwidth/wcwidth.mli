(** Get the display width of a Unicode character. (Note that there isn't actually
    a good way to actually *input* a single Unicode character.) *)
val wcwidth : Uchar.t -> int

(** Decompose a string into a list of Unicode characters. *)
val to_utf8 : string -> Uchar.t list

(** Get the display width of a string. *)
val wcswidth : string -> int
