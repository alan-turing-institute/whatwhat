open Cohttp

exception HttpError of string

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

(** Raise a HttpError if the API request response indicates failure. *)
let check_http_response response =
  if Response.status response |> Code.code_of_status |> Code.is_success |> not
  then (
    let code_string = Response.status response |> Code.string_of_status in
    raise @@ HttpError code_string)
;;
