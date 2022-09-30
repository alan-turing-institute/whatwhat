(** Parse a string as a date in the format year-month-day. If the string is not in this
    format, return [Error ()], else [Ok date]. *)
let date_of_string (str : string) =
  let datelist = Str.split (Str.regexp {|-|}) str in
  match datelist with
  | [ year; month; day ] ->
    (try
       Ok
         (CalendarLib.Date.make
            (int_of_string year)
            (int_of_string month)
            (int_of_string day))
     with
     | _ -> Error ())
  | _ -> Error ()
;;
