#use "topfind";;
#require "yojson";;
(*#require "ppx_yojson_conv";;*)
#require "ppx_deriving_yojson";;
#require "ppx_deriving.show"
open Yojson.Basic.Util
open Yojson
open Result

(*a script to mess around with the yojson parser and @deriving methods 
 
run directly with `ocaml lib/jsonplay.ml`

play.json is a nested structure similar to github api body:

{"top": {"inner": [{"nodes": {"numbers": [1, 2, 3]}}]}}

where the challenge is to neatly parse the type 'numbers', 
accounting for known nested fields in a neat way

*)



(* option 1 - nested types. Works but is ugly, 
   and to access numbers you need to work through the types *)


(* an example of a field-level @of_yojson in action - 
   see https://github.com/ocaml-ppx/ppx_deriving_yojson#to_yojson--of_yojson *)
let add20_result = function
  | Ok i -> ok (i + 20)
  | Error str -> error str

type nums = {
  numbers : int [@of_yojson fun x -> ok ((get_ok x) + 20)] [@to_yojson fun i -> `Int (i + 1)]; (* let add1 y = y + 1  in List.map add1 x];*)
}

[@@deriving show, yojson ] 

type nodes = {
  nodes : nums;
}
[@@deriving show, of_yojson]

type inner = {
  edges : nodes list; [@key "inner"]
}
[@@deriving show, of_yojson]

type root = {
  top : inner; 
}
[@@deriving show, of_yojson]

let () = print_endline @@ show_nums @@ nums_of_yojson @@ Yojson.Safe.from_string "{\"numbers\": 5}"
let () = print_endline @@ Yojson.Safe.to_string @@ nums_to_yojson @@ {numbers = 5}

let () = 
  let js = Yojson.Basic.from_file "lib/play.json" |> (fun x -> (x : Yojson.Basic.t :> Yojson.Safe.t)) in
  let rt = root_of_yojson js in
  List.iter (fun x -> print_endline @@ show_nums x.nodes) rt.top.edges

(*-----------------------*)

let extract_nodes json = 
  let js = member "edges" json in
  Yojson.Basic.Util.filter_list [js] |> List.hd |> filter_member "node"
  
let extract_columns json = 
  json 
  |> member "data" 
  |> member "repository"
  |> member "projects"
  |> extract_nodes |> List.hd
  |> member "columns" 
  |> extract_nodes 


  (*let j = extract_numbers js in 
  List.iter (fun c -> print_endline @@ Yojson.Basic.to_string c) j;*)
  

(*
  fun x -> (x : Yojson.Basic.t :> Yojson.Safe.t)) in 
  print_endline (js |> Yojson.Safe.to_string); *)

(*
let () =
  let json1 = Yojson.Basic.from_file "lib/sample.json" in 
  print_endline (json1 |> Yojson.Basic.to_string)
*)
