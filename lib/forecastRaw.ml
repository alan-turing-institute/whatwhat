open Cohttp
open Cohttp_lwt
open Cohttp_lwt_unix
open Yojson
open Yojson.Basic.Util
module IntMap = Map.Make (Int)

(** HTTP requests --------------------------------------------- *)

(** Convenience function to generate a map from a list based on a key-generating
    function [identify]. *)
let make_map identify xs =
  List.map (fun x -> identify x, x) xs |> List.to_seq |> IntMap.of_seq
;;

(** Perform a HTTP request to the Forecast API. *)
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
  let* body =
    Lwt.catch
      (fun () ->
        let* r, b = Client.get ~headers uri in
        Utils.check_http_response r;
        Lwt.return b)
      (function
       | Failure _ -> failwith "Forecast HTTP request failed."
       | Utils.HttpError e -> failwith ("Forecast HTTP request failed: " ^ e)
       | exn -> Lwt.fail exn)
  in
  let* body_string = Body.to_string body in
  (* Forecast returns, eg, {"clients", [...]}, so we extract the list here *)
  Lwt.return (body_string |> Yojson.Basic.from_string |> member endpoint)
;;

(** Clients --------------------------------------------------- *)

(** A client on Forecast is (roughly speaking) a programme. *)
type client =
  { id : int
  ; name : string
  ; archived : bool
  }
[@@deriving show, of_yojson] [@@yojson.allow_extra_fields]

(** Retrieve all clients from Forecast. *)
let get_clients_async () =
  let open Lwt.Syntax in
  let* clients = forecast_request_async "clients" in
  Lwt.return
    (clients
    |> to_list
    |> List.map (fun x -> client_of_yojson (x : Basic.t :> Safe.t))
    |> make_map (fun (c : client) -> c.id))
;;

(** People, placeholders, and entities ------------------------ *)

(** A person on Forecast is a real person. *)
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

(** A placeholder on Forecast is not a real person, but behaves like one. *)
type placeholder =
  { id : int
  ; name : string
  ; roles : string list
  ; archived : bool
  }
[@@deriving show, of_yojson] [@@yojson.allow_extra_fields]

(** An entity is either a person or a placeholder. *)
type entity =
  | Person of person
  | Placeholder of placeholder
[@@deriving show]

(** Generate the full name of a person. *)
let make_person_name p = p.first_name ^ " " ^ p.last_name

(** Retrieve all people from Forecast. *)
let get_people_async () =
  let open Lwt.Syntax in
  let* people = forecast_request_async "people" in
  Lwt.return
    (people
    |> to_list
    |> List.map (fun x -> person_of_yojson (x : Basic.t :> Safe.t))
    |> make_map (fun (p : person) -> p.id))
;;

(** Retrieve all placeholders from Forecast. *)
let get_placeholders_async () =
  let open Lwt.Syntax in
  let* placeholders = forecast_request_async "placeholders" in
  Lwt.return
    (placeholders
    |> to_list
    |> List.map (fun x -> placeholder_of_yojson (x : Basic.t :> Safe.t))
    |> make_map (fun (p : placeholder) -> p.id))
;;

(** Get the name of an entity. *)
let get_entity_name e =
  match e with
  | Person p -> make_person_name p
  | Placeholder p -> "Placeholder: " ^ p.name
;;

(** Get the roles of an entity. *)
let get_entity_roles e =
  match e with
  | Person p -> p.roles
  | Placeholder p -> p.roles
;;

(** Get the Forecast ID of an entity. *)
let get_entity_id e =
  match e with
  | Person p -> p.id
  | Placeholder p -> p.id
;;

(** Get the archived status of an entity. *)
let get_entity_archived e =
  match e with
  | Person p -> p.archived
  | Placeholder p -> p.archived
;;

(** Projects -------------------------------------------------- *)

(** This type represents the raw JSON of a project as returned by Forecast.
    Instead of providing a client, it only provides a client ID, so we 'fill in
    the client' by looking it up in the list of clients before passing the data
    on to other modules. *)
type project_schema =
  { id : int
  ; harvest_id : int option
  ; client_id : int option (* The built-in project has no client !! *)
  ; name : string
  ; code : string option (* This should have the form "hut23-X" *)
  ; tags : string list
  ; notes : string option
  ; color : string
  ; archived : bool
  }
[@@deriving of_yojson] [@@yojson.allow_extra_fields]

(** This is a filled-in project datatype. *)
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

(** Retrieve all projects from Forecast. *)
let get_project_schemas_async () =
  let open Lwt.Syntax in
  let* projects = forecast_request_async "projects" in
  Lwt.return
    (projects
    |> to_list
    |> List.map (fun x -> project_schema_of_yojson (x : Basic.t :> Safe.t))
    |> make_map (fun (p : project_schema) -> p.id))
;;

(** This converts a [project_schema] to a [project], i.e., fills in the client
    by looking it up in the list of clients. *)
let populate_project_client clients prj =
  let client = Option.bind prj.client_id (fun id -> IntMap.find_opt id clients) in
  { id = prj.id
  ; harvest_id = prj.harvest_id
  ; client
  ; name = prj.name
  ; code = prj.code
  ; tags = prj.tags
  ; notes = prj.notes
  ; color = prj.color
  ; archived = prj.archived
  }
;;

(** Assignments ----------------------------------------------- *)

(** This type represents the raw JSON of an assignment as returned by Forecast.
    Similarly to the project JSON above, we fill in some subfields of the
    assignment before passing it on to other modules. *)
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

(** This is the 'filled-in' assignment type. *)
type assignment =
  { id : int
  ; project : project
  ; entity : entity
  ; start_date : Utils.date
  ; end_date : Utils.date
  ; allocation : int
  ; notes : string option
  }
[@@deriving show]

(** Parse the JSON returned by Forecast (which in general is a list of list of
    assignments, because each query returns a list of assignments, and we
    perform multiple queries to cover 180-day periods as necessary). This also
    performs a merge of the results from each query, discarding any duplicate
    assignments. *)
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

(** Obtain all assignments between [start_date] and [end_date].

    Note that the Forecast API restricts the maximum query period to be 180
    days. Thus, if [start_date] and [end_date] differ by a longer period,
    several sub-queries must be made and the results combined. The combination
    process is carried out in the [parse_combined_assignment_json] function.
    *)
let get_assignments_async (start_date : Utils.date) (end_date : Utils.date) =
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

(** Converts an [assignment_schema] to a proper [assignment]. *)
let populate_assignment_subfields people placeholders projects asn =
  (* The raw Forecast JSON only returns client IDs, person IDs, etc. It makes
     life much easier later if we actually associate the clients, people, etc.
     themselves with each assignment. *)
  let project =
    match IntMap.find_opt asn.project_id projects with
    | Some prj -> prj
    | None ->
      failwith
        (Printf.sprintf "Project %d in assignment %d not found" asn.project_id asn.id)
  in
  let entity =
    match asn.person_id, asn.placeholder_id with
    | Some p, None ->
      (match IntMap.find_opt p people with
       | Some person -> Person person
       | None -> failwith (Printf.sprintf "Person %d in assignment %d not found" p asn.id))
    | None, Some p ->
      (match IntMap.find_opt p placeholders with
       | Some placeholder -> Placeholder placeholder
       | None ->
         failwith (Printf.sprintf "Placeholder %d in assignment %d not found" p asn.id))
    | Some _, Some _ ->
      failwith (Printf.sprintf "Multiple entities found for assignment %d" asn.id)
    | None, None -> failwith (Printf.sprintf "No entities found for assignment %d" asn.id)
  in
  match Utils.date_of_string asn.start_date, Utils.date_of_string asn.end_date with
  | Ok sdate, Ok edate ->
    { id = asn.id
    ; project
    ; entity
    ; start_date = sdate
    ; end_date = edate
    ; allocation = asn.allocation
    ; notes = asn.notes
    }
  | Error _, _ -> failwith (Printf.sprintf "Invalid start date for assignment %d" asn.id)
  | _, Error _ -> failwith (Printf.sprintf "Invalid end date for assignment %d" asn.id)
;;

(** Get the schedule ------------------------------------------ *)

(** Fetch clients, people, placeholders, projects, and assignments from
    Forecast. Returns a promise. *)
let get_the_schedule_async ~start_date ~end_date =
  let open Lwt.Syntax in
  let* clients = get_clients_async () in
  let* people = get_people_async () in
  let* placeholders = get_placeholders_async () in
  let* project_schemas = get_project_schemas_async () in
  let projects = IntMap.map (populate_project_client clients) project_schemas in
  let* assignment_schemas = get_assignments_async start_date end_date in
  let assignments =
    List.map
      (populate_assignment_subfields people placeholders projects)
      assignment_schemas
  in
  Lwt.return (clients, people, placeholders, projects, assignments)
;;

(** The same as above, but returns the data directly. *)
let get_the_schedule ~start_date ~end_date =
  get_the_schedule_async ~start_date ~end_date |> Lwt_main.run
;;
