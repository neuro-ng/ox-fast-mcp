open Core
open Async
open Cohttp
open Cohttp_async
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module Phue_exception = struct
  exception Error of string
end

type t = {
  ip : string;
  username : string;
}

let create ~ip ~username = { ip; username }

let base_url t = Uri.of_string (sprintf "http://%s/api/%s" t.ip t.username)

let check_response resp body =
  let status = Response.status resp in
  match status with
  | `OK ->
      let%map body_str = Body.to_string body in
      (try
         Yojson.Safe.from_string body_str
       with _ -> `Null)
  | _ ->
      raise (Phue_exception.Error (sprintf "HTTP Error: %s" (Code.string_of_status status)))

(* Generic GET request *)
let get t path =
  let uri = Uri.with_path (base_url t) (Uri.path (base_url t) ^ "/" ^ path) in
  let%bind resp, body = Client.get uri in
  check_response resp body

(* Generic PUT request *)
let put t path data =
  let uri = Uri.with_path (base_url t) (Uri.path (base_url t) ^ "/" ^ path) in
  let body = Cohttp_async.Body.of_string (Yojson.Safe.to_string data) in
  let%bind resp, body = Client.put ~body uri in
  check_response resp body

(* Lights API *)
type light_state = {
  on : bool option;
  bri : int option;
  hue : int option;
  sat : int option;
  xy : float list option;
} [@@deriving yojson] [@@yojson.allow_extra_fields]

type light = {
  id : int;
  name : string;
  state : light_state;
}

(* Helper to parse properties from the dictionary response *)
let parse_lights_map json =
  match json with
  | `Assoc fields ->
      List.filter_map fields ~f:(fun (id_str, data) ->
          match data with
          | `Assoc props ->
              let name =
                List.Assoc.find props ~equal:String.equal "name"
                |> Option.value_map ~default:("Light " ^ id_str) ~f:(function
                     | `String s -> s
                     | _ -> "Light " ^ id_str)
              in
              let state =
                List.Assoc.find props ~equal:String.equal "state"
                |> Option.value_map ~default:{ on=None; bri=None; hue=None; sat=None; xy=None } ~f:(fun json ->
                    try light_state_of_yojson json
                    with e -> 
                      Stdlib.Printf.eprintf "Parse error: %s\n" (Exn.to_string e);
                      { on=None; bri=None; hue=None; sat=None; xy=None })
              in
              (try
                 let id = Int.of_string id_str in
                 Some { id; name; state }
               with _ -> None)
          | _ -> None)
  | _ -> []

let get_lights t =
  let%bind json = get t "lights" in
  return (parse_lights_map json)

let set_light_state t id state_json =
  let path = sprintf "lights/%d/state" id in
  let%map result = put t path state_json in
  (* Hue API returns a list of success/error objects *)
  result

(* Groups API *)
type group = {
  id : string;
  name : string;
  lights : string list;
  type_ : string; [@key "type"]
} [@@deriving yojson]

let get_groups t =
  let%bind json = get t "groups" in
  match json with
  | `Assoc fields ->
      List.filter_map fields ~f:(fun (id, data) ->
        try
          let g = group_of_yojson data in
          Some { g with id }
        with _ -> None
      ) |> return
  | _ -> return []

let set_group_action t id action_json =
  let path = sprintf "groups/%s/action" id in
  let%map result = put t path action_json in
  result

(* Scenes API *)
type scene = {
  id : string;
  name : string;
  lights : string list option;
  group : string option;
} [@@deriving yojson]

let get_scenes t =
  let%bind json = get t "scenes" in
  match json with
  | `Assoc fields ->
      List.filter_map fields ~f:(fun (id, data) ->
        try
          let s = scene_of_yojson data in
          Some { s with id }
        with _ -> None
      ) |> return
  | _ -> return []

let get_light_attributes t id = 
  let%bind json = get t (sprintf "lights/%d" id) in
  return json

let get_group_attributes t id =
  let%bind json = get t (sprintf "groups/%s" id) in
  return json
