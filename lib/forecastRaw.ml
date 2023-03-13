open Cohttp
open Cohttp_lwt
open Cohttp_lwt_unix
open Yojson
open Yojson.Basic.Util
open CalendarLib

(* An IdMap is a map from Forecast ids *)
module IdMap = Map.Make (Int)

let make_map identify xs = List.map identify xs |> List.to_seq |> IdMap.of_seq

let forecast_request_async ?(query = []) endpoint =
  let open Lwt.Syntax in
  let headers =
    Header.of_list
      [ "Forecast-Account-ID", Config.get_forecast_id ()
      ; "Authorization", "Bearer " ^ Config.get_forecast_token ()
      ]
  in
  let uri =
    Uri.with_query' (Uri.of_string (Config.get_forecast_url () ^ "/" ^ endpoint)) query
  in
  let* response = Client.get ~headers uri in
  let* body_string = response |> snd |> Body.to_string in
  (* Forecast returns, eg, {"clients", [...]} *)
  Lwt.return (body_string |> Yojson.Basic.from_string |> member endpoint)
;;

(* ---------------------------------------------------------------------- *)

type project_schema =
  { id : int
  ; harvest_id : int option
  ; client_id : int option (* The built-in project has no client !! *)
  ; name : string
  (* TODO: why can project code be None? *)
  ; code : string option
  ; tags : string list
  ; notes : string option
  ; color : string
  ; archived : bool
  }
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

let get_project_schemas_async () =
  let open Lwt.Syntax in
  let* projects = forecast_request_async "projects" in
  Lwt.return
    (projects
    |> to_list
    |> List.map (fun x -> project_schema_of_yojson (x : Basic.t :> Safe.t))
    |> make_map (function ({ id; _ } as p : project_schema) -> id, p))
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
    (clients
    |> to_list
    |> List.map (fun x -> client_of_yojson (x : Basic.t :> Safe.t))
    |> make_map (function ({ id; _ } as c : client) -> id, c))
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
    (people
    |> to_list
    |> List.map (fun x -> person_of_yojson (x : Basic.t :> Safe.t))
    |> make_map (function ({ id; _ } as p : person) -> id, p))
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
    |> List.map (fun x -> placeholder_of_yojson (x : Basic.t :> Safe.t))
    |> make_map (function ({ id; _ } as p : placeholder) -> id, p))
;;

(* A person or a placeholder. *)
type entity =
  | Person of person
  | Placeholder of placeholder
[@@deriving show, of_yojson]

let get_entity_name e =
  match e with
  | Person p -> p.first_name ^ " " ^ p.last_name
  | Placeholder p -> p.name
;;

let get_entity_roles e =
  match e with
  | Person p -> p.roles
  | Placeholder p -> p.roles
;;

let get_entity_id e =
  match e with
  | Person p -> p.id
  | Placeholder p -> p.id
;;

let get_entity_archived e =
  match e with
  | Person p -> p.archived
  | Placeholder p -> p.archived
;;

(* ---------------------------------------------------------------------- *)

type assignment_schema =
  { id : int
  ; project_id : int
  ; person_id : int option
  ; placeholder_id : int option
  ; start_date : string
  ; end_date : string
  ; allocation : int
  ; notes : string option
  }
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

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
    |> List.map (fun x -> (x : Basic.t :> Safe.t) |> assignment_schema_of_yojson)
  in
  bs
  |> List.map (fun s ->
       s
       |> parse_one_assignment
       |> List.to_seq
       |> Seq.map (fun (a : assignment_schema) -> a.id, a)
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

type project =
  { id : int
  ; harvest_id : int option
  ; client : client option
  ; name : string
  ; code : string option
  ; tags : string list
  ; notes : string option
  ; color : string
  ; archived : bool
  }
[@@deriving show]

type assignment =
  { id : int
  ; project : project
  ; entity : entity
  ; start_date : string
  ; end_date : string
  ; allocation : int
  ; notes : string option
  }
[@@deriving show]

let populate_project_client clients prj =
  let client =
    match prj.client_id with
    | None -> None
    | Some i -> IdMap.find_opt i clients
  in
  { id = prj.id
  ; harvest_id = prj.harvest_id
  ; client = client
  ; name = prj.name
  ; code = prj.code
  ; tags = prj.tags
  ; notes = prj.notes
  ; color = prj.color
  ; archived = prj.archived
  }
;;

let populate_assignment_subfields people placeholders projects asn =
  (* The raw Forecast JSON only returns client IDs, person IDs, etc. It makes
     life much easier later if we actually associate the clients, people, etc.
     themselves with each assignment. *)
  let project =
    match IdMap.find_opt asn.project_id projects with
    | Some prj -> prj
    | None ->
      failwith
        (Printf.sprintf "project %d in assignment %d not found" asn.project_id asn.id)
  in
  let entity =
    match asn.person_id, asn.placeholder_id with
    | Some p, None ->
      (match IdMap.find_opt p people with
       | Some person -> Person person
       | None -> failwith (Printf.sprintf "person %d in assignment %d not found" p asn.id))
    | None, Some p ->
      (match IdMap.find_opt p placeholders with
       | Some placeholder -> Placeholder placeholder
       | None ->
         failwith (Printf.sprintf "placeholder %d in assignment %d not found" p asn.id))
    | Some _, Some _ ->
      failwith (Printf.sprintf "assignment %d had both person and placeholder" asn.id)
    | None, None -> failwith (Printf.sprintf "assignment %d had no entity" asn.id)
  in
  { id = asn.id
  ; project
  ; entity
  ; start_date = asn.start_date
  ; end_date = asn.end_date
  ; allocation = asn.allocation
  ; notes = asn.notes
  }
;;

let get_the_schedule_async ~start_date ~end_date =
  let open Lwt.Syntax in
  let* clients = get_clients_async () in
  let* people = get_people_async () in
  let* placeholders = get_placeholders_async () in
  let* project_schemas = get_project_schemas_async () in
  let projects = IdMap.map (populate_project_client clients) project_schemas in
  let* assignment_schemas = get_assignments_async start_date end_date in
  let assignments =
    List.map
      (populate_assignment_subfields people placeholders projects)
      assignment_schemas
  in

  Lwt.return (clients, people, placeholders, projects, assignments)
;;

let get_the_schedule ~start_date ~end_date =
  get_the_schedule_async ~start_date ~end_date |> Lwt_main.run
;;
