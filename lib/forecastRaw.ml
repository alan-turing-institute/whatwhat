open Cohttp
open Cohttp_lwt
open Cohttp_lwt_unix
open Yojson
open Yojson.Basic.Util
open CalendarLib

(* An IdMap is a map from Forecast ids *)
module IdMap = Map.Make (Int)

let forecast_request_async ?(query = []) endpoint =
  let open Lwt.Syntax in
  let headers =
    Header.of_list
      [ "Forecast-Account-ID", Config.get_forecast_id ()
      ; "Authorization", "Bearer " ^ Config.get_forecast_token ()
      ]
  in
  let uri =
    Uri.with_query' (Uri.of_string (Config.get_forecast_url () ^ endpoint)) query
  in
  let* response = Client.get ~headers uri in
  let* body_string = response |> snd |> Body.to_string in
  (* Forecast returns, eg, {"clients", [...]} *)
  Lwt.return (body_string |> Yojson.Basic.from_string |> member endpoint)
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

let get_projects_async () =
  let open Lwt.Syntax in
  let* projects = forecast_request_async "projects" in
  Lwt.return
    (projects |> to_list |> List.map (fun x -> project_of_yojson (x : Basic.t :> Safe.t)))
;;

(* ---------------------------------------------------------------------- *)

type client =
  { id : int
  ; name : string
  ; archived : bool
  }
[@@deriving show, of_yojson] [@@yojson.allow_extra_fields]

let get_clients_async () =
  let open Lwt.Syntax in
  let* clients = forecast_request_async "clients" in
  Lwt.return
    (clients |> to_list |> List.map (fun x -> client_of_yojson (x : Basic.t :> Safe.t)))
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

let get_people_async () =
  let open Lwt.Syntax in
  let* people = forecast_request_async "people" in
  Lwt.return
    (people |> to_list |> List.map (fun x -> person_of_yojson (x : Basic.t :> Safe.t)))
;;

(* ---------------------------------------------------------------------- *)

type placeholder =
  { id : int
  ; name : string
  ; roles : string list
  ; archived : bool
  }
[@@deriving show, of_yojson] [@@yojson.allow_extra_fields]

let get_placeholders_async () =
  let open Lwt.Syntax in
  let* placeholders = forecast_request_async "placeholders" in
  Lwt.return
    (placeholders
    |> to_list
    |> List.map (fun x -> placeholder_of_yojson (x : Basic.t :> Safe.t)))
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

module IntMap = Map.Make (Int)

let parse_combined_assignment_json (bs : Basic.t list) =
  let merge_maps asns =
    List.fold_right
      (IntMap.union (fun _ x y ->
         assert (x = y);
         Some x))
      asns
      IntMap.empty
  in
  let parse_one_assignment b =
    b
    |> Yojson.Basic.Util.to_list
    |> List.map (fun x -> assignment_of_yojson (x : Basic.t :> Safe.t))
  in
  bs
  |> List.map (fun s ->
       s
       |> parse_one_assignment
       |> List.to_seq
       |> Seq.map (fun a -> a.id, a)
       |> IntMap.of_seq)
  |> merge_maps
  |> IntMap.to_seq
  |> Seq.map snd
  |> List.of_seq
;;

(* The Forecast API sets the longest period assignments can be queried for in one query.
 *)
let get_assignments_async (start_date : Date.t) (end_date : Date.t) =
  let open Lwt.Syntax in
  let rec get_assignments_inner start_date end_date =
    let open CalendarLib.Printer in
    let open CalendarLib.Date in
    assert (start_date <= end_date);
    let max_period = Period.day 179 in
    let batch_end_date = add start_date max_period in
    if batch_end_date <= end_date
    then
      forecast_request_async
        "assignments"
        ~query:
          [ "start_date", DatePrinter.to_string start_date
          ; "end_date", DatePrinter.to_string batch_end_date
          ]
      :: get_assignments_inner batch_end_date end_date
    else
      [ forecast_request_async
          "assignments"
          ~query:
            [ "start_date", DatePrinter.to_string start_date
            ; "end_date", DatePrinter.to_string end_date
            ]
      ]
  in
  let* assignments_json = get_assignments_inner start_date end_date |> Lwt.all in
  Lwt.return (parse_combined_assignment_json assignments_json)
;;

(* ---------------------------------------------------------------------- *)

let get_the_schedule_async ~start_date ~end_date =
  let open Lwt.Syntax in
  let make_map identify xs = List.map identify xs |> List.to_seq |> IdMap.of_seq in

  let* clients = get_clients_async () in
  let* people = get_people_async () in
  let* placeholders = get_placeholders_async () in
  let* projects = get_projects_async () in
  let* assignments = get_assignments_async start_date end_date in

  Lwt.return
    ( clients |> make_map (function ({ id; _ } as c : client) -> id, c)
    , people |> make_map (function ({ id; _ } as p : person) -> id, p)
    , placeholders |> make_map (function ({ id; _ } as p : placeholder) -> id, p)
    , projects |> make_map (function ({ id; _ } as p : project) -> id, p)
    , assignments )
;;

let get_the_schedule ~start_date ~end_date =
  get_the_schedule_async ~start_date ~end_date |> Lwt_main.run
;;
