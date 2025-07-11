(** Utility module. *)

open CalendarLib

(** {1 Shims for the standard library}

    Many of the functions in this module are simple functions which should
    arguably be in the OCaml standard library, but aren't, because the standard
    library is so barebones. *)

(** Check whether a character is [0-9]. *)
val is_digit : char -> bool

(** Check whether a character is [0-9a-f]. *)
val is_hex_digit : char -> bool

(** Generate a range from i to j-1 inclusive. *)
val range : int -> int -> int list

(** Group adjacent elements of a list together using a predicate. *)
val group_by : ('a -> 'a -> bool) -> 'a list -> 'a list list

(** Sort and then group elements of a list according to a key. *)
val sort_and_group_by : ('a -> 'key) -> 'a list -> ('key * 'a list) list

(** Take the first n elements of a list. *)
val take : int -> 'a list -> 'a list

(** Drop the first n elements of a list. *)
val drop : int -> 'a list -> 'a list

(** [split_at n xs] returns [(take n xs, drop n xs)] (i.e. the first [n] elements
    and the rest). *)
val split_at : int -> 'a list -> 'a list * 'a list

(** Sum a list of floats. *)
val sum : float list -> float

(** Get the maximum of a list using a key function. *)
val max_by : default:'b -> ('a -> 'b) -> 'a list -> 'b

(** Transpose a list of lists. *)
val transpose : 'a list list -> 'a list list

(** Check if the second argument is a substring of the first. *)
val contains : ?case_sensitive:bool -> string -> string -> bool

(** Attempt to read all text from a file. Returns [None] for various errors
    (e.g. if the file doesn't exist). *)
val read_file : string -> string option

(** Print a string option. *)
val show_opt : string option -> string

(** {1 HTTP exceptions} *)

(** Generic HTTP error. *)
exception HttpError of string

(** Raised when the GitHub API rate limit is exceeded. *)
exception GithubRateLimitError of string

(** Inspect a response and raise a [HTTPError] if the code is not a 200, or a
    [GithubRateLimitError] if the GitHub API rate limit is exceeded. *)
val check_http_response : Cohttp.Response.t -> unit

(** {1 Dates} *)

(** We redefine the standard [Date.t] type here so that we can
    derive Show instances for record types containing it. *)
type date = Date.t

val pp_date : Format.formatter -> Printer.Date.t -> unit
val show_date : Printer.Date.t -> string

(** Parse a string as a date in the format YYYY-MM-DD. If the string is not
    in this format, return [Error (`Msg s)] (where [s] is some error message),
    else [Ok date].

    If [lax] is set to [true], then punctuation is ignored, so e.g. YYYYMMDD and
    YYYY/MM/DD will also parse correctly. [lax] is false by default. *)
val date_of_string : ?lax:bool -> string -> (Date.t, [> `Msg of string ]) result

(** Calculate the default start date for Forecast export, which is the 1st of the
    previous month (relative to today's date). The [relative_to] argument can be
    specified to allow for unit testing. *)
val default_export_start_date : ?relative_to:Date.t -> unit -> Date.t

(** Calculate the default end date for Forecast export, which is the last day of
    the next month (relative to today's date). The [relative_to] argument can be
    specified to allow for unit testing. *)
val default_export_end_date : ?relative_to:Date.t -> unit -> Date.field Date.date

(** Roll a date back to a Monday. Leaves Mondays untouched. *)
val rollback_week : Date.t -> Date.t

(** Roll a date forward to a Friday. Leaves Fridays untouched.
    If [with_weekend] is [true], then rolls forward to Sundays. *)
val rollforward_week : ?with_weekend:bool -> Date.t -> Date.t

(** Get all days between the start and end dates, inclusive, which are the day
    specified in [day]. *)
val get_xdays_between
  :  day:Date.day
  -> start_date:Date.t
  -> end_date:Date.t
  -> Date.t list

(** Get all Mondays of the weeks where the Thursday belongs to a calendar month.
    The day of the month of the input argument does not matter. *)
val get_turing_weeks_in_month : [> `Year | `Month ] Date.date -> Date.t list

(** Get a list of the weekdays in a week. Input argument can be any day of the
    week in question. *)
val get_weekdays_in_week : Date.t -> Date.t list

(** Get a list of the weekdays in a month. *)
val get_weekdays_in_month : [> `Year | `Month ] Date.date -> Date.t list

(** Maps ints from 1-12 to month names Jan-Dec. *)
val show_month : int -> string

(** {1 Other stuff} *)

(** [all_throttled] is like [Lwt.all] but only resolves [max_concurrent]
    promises at a time. Defaults to 150 (which, as determined by trial and
    error, is approximately how many requests GitHub likes taking at a go). *)
val all_throttled : ?max_concurrent:int -> 'a Lwt.t list -> 'a list Lwt.t

(** Escape a string such that characters show up correctly in (GitHub-Flavoured)
    Markdown. Note: this means that formatting etc. will be lost in the
    resulting post, and it'll purely appear as a string. *)
val gfm_escape : string -> string

(** Remove double quotes around a string (if present. *)
val dequote : string -> string

(** Truncate a string to a maximum length, adding an ellipsis if it was
    truncated. *)
val elide : ?max_length:int -> string -> string
