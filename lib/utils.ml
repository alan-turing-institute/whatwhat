type date_parsing_error = DateParsingError

let date_opt_of_string (str : string) =
  let datelist = Str.split (Str.regexp {|-|}) str in
  match datelist with
  | [ year; month; day ] ->
    Some
      (CalendarLib.Date.make
         (int_of_string year)
         (int_of_string month)
         (int_of_string day))
  | _ -> None
;;

let date_opt_of_string_opt (str : string option) =
  match str with
  | None -> None
  | Some x -> date_opt_of_string x
;;
