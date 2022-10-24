open Cohttp
open Cohttp_lwt
open Cohttp_lwt_unix
open Yojson
open Yojson.Basic.Util
open CalendarLib

(* An IdMap is a map from Forecast ids *)
module IdMap = Map.Make (Int)

let forecast_request ?(query = []) endpoint =
  let _, body =
    let headers =
      Header.of_list
        [ "Forecast-Account-ID", Config.get_forecast_id ()
        ; "Authorization", "Bearer " ^ Config.get_forecast_token ()
        ]
    and uri =
      Uri.with_query' (Uri.of_string ("https://api.forecastapp.com/" ^ endpoint)) query
    in
    Client.get ~headers uri |> Lwt_main.run
  in
  Basic.from_string (Lwt_main.run @@ Body.to_string body)
  |> member endpoint (* Forecast returns, eg, {"clients", [...]} *)
;;

(* ---------------------------------------------------------------------- *)

type project =
  { id : int
  ; harvest_id : int option
  ; client_id : int option (* The built-in project has no client !! *)
  ; name : string
  ; code : string option
  ; tags : string list
  ; notes : string option
  ; archived : bool
  }
[@@deriving show, of_yojson] [@@yojson.allow_extra_fields]

let get_projects () =
  forecast_request "projects"
  |> to_list
  |> List.map (fun x -> project_of_yojson (x : Basic.t :> Safe.t))
;;

(* ---------------------------------------------------------------------- *)

type client =
  { id : int
  ; name : string
  ; archived : bool
  }
[@@deriving show, of_yojson] [@@yojson.allow_extra_fields]

let get_clients () =
  forecast_request "clients"
  |> to_list
  |> List.map (fun x -> client_of_yojson (x : Basic.t :> Safe.t))
;;

(* ---------------------------------------------------------------------- *)

type person =
  { id : int
  ; first_name : string
  ; last_name : string
  ; email : string option
  ; login : string
  ; roles : string list
  ; archived : bool
  }
[@@deriving show, of_yojson] [@@yojson.allow_extra_fields]

let get_people () =
  forecast_request "people"
  |> to_list
  |> List.map (fun x -> person_of_yojson (x : Basic.t :> Safe.t))
;;

(* ---------------------------------------------------------------------- *)

type placeholder =
  { id : int
  ; name : string
  ; roles : string list
  ; archived : bool
  }
[@@deriving show, of_yojson] [@@yojson.allow_extra_fields]

let get_placeholders () =
  forecast_request "placeholders"
  |> to_list
  |> List.map (fun x -> placeholder_of_yojson (x : Basic.t :> Safe.t))
;;

(* ---------------------------------------------------------------------- *)

type assignment =
  { id : int
  ; project_id : int
  ; person_id : int option
  ; placeholder_id : int option
  ; start_date : string
  ; end_date : string
  ; allocation : int
  ; notes : string option
  }
[@@deriving show, of_yojson] [@@yojson.allow_extra_fields]

(* The Forecast API sets the longest period assignments can be queried for in one query.
 *)
let max_period = Date.Period.lmake ~day:179 ()

let get_assignments_period (start_date : Date.t) (end_date : Date.t) =
  assert (Date.sub end_date start_date <= max_period);
  let st = Printer.DatePrinter.to_string start_date
  and ed = Printer.DatePrinter.to_string end_date in
  forecast_request "assignments" ~query:[ "start_date", st; "end_date", ed ]
  |> to_list
  |> List.map (fun x -> assignment_of_yojson (x : Basic.t :> Safe.t))
;;

module IntMap = Map.Make (Int)

let rec get_assignments_inner
  ?(acc = IntMap.empty)
  (start_date : Date.t)
  (end_date : Date.t)
  =
  (* If we can't do the whole requested period in one Forecast API call, get as much as
k    we can, recursively call for the rest, and merge the results into a Map to avoid
     duplicates. *)
  let max_end_date = Date.add start_date max_period in
  let batch_end_date = if max_end_date < end_date then max_end_date else end_date in
  let new_assignments =
    get_assignments_period start_date batch_end_date
    |> List.to_seq
    |> Seq.map (fun a -> a.id, a)
    |> IntMap.of_seq
  in
  let new_acc =
    IntMap.union
      (fun _ x y ->
        assert (x = y);
        Some x)
      acc
      new_assignments
  in
  if end_date > batch_end_date
  then get_assignments_inner ~acc:new_acc batch_end_date end_date
  else new_acc
;;

let get_assignments (start_date : Date.t) (end_date : Date.t) =
  get_assignments_inner start_date end_date |> IntMap.to_seq |> Seq.map snd |> List.of_seq
;;

(* ---------------------------------------------------------------------- *)

(* TODO: Have these run concurrently *)
let get_the_schedule (start_date : Date.t) (end_date : Date.t) =
  let make_map identify xs = List.map identify xs |> List.to_seq |> IdMap.of_seq in
  ( get_clients () |> make_map (function ({ id; _ } as c : client) -> id, c)
  , get_people () |> make_map (function ({ id; _ } as p : person) -> id, p)
  , get_placeholders () |> make_map (function ({ id; _ } as p : placeholder) -> id, p)
  , get_projects () |> make_map (function ({ id; _ } as p : project) -> id, p)
  , get_assignments start_date end_date )
;;
