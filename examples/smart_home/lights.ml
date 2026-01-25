open Core
open Async
open Ppx_yojson_conv_lib.Yojson_conv.Primitives
module FastMCP = Server.Ox_fast_mcp

type json = Yojson.Safe.t

let json_of_yojson (j : Yojson.Safe.t) : json = j
let yojson_of_json (j : json) : Yojson.Safe.t = j

exception KeyError of string

(* Mock Bridge Removed - using Phue module *)

module Phue_exception = struct
  exception Error of string
end

(* -------------------------------------------------------------------------- *)
(* Helpers                                                                    *)
(* -------------------------------------------------------------------------- *)

let _get_bridge () =
  try
    let client =
      Phue.create ~ip:Config.config.hue_bridge_ip
        ~username:Config.config.hue_bridge_username
    in
    Some client
  with _ -> None

let handle_phue_error light_or_group operation error =
  let msg =
    match error with
    | Phue.Phue_exception.Error e ->
      Printf.sprintf "Phue error during %s: %s" operation e
    | _ -> Printf.sprintf "Unexpected error during %s" operation
  in
  `Assoc
    [
      ("target", `String light_or_group);
      ("operation", `String operation);
      ("success", `Bool false);
      ("error", `String msg);
    ]

(* -------------------------------------------------------------------------- *)
(* Lights MCP                                                                 *)
(* -------------------------------------------------------------------------- *)

let lights_mcp = FastMCP.create ~name:"Hue Lights Service (phue2)" ()

(* Types for Attributes *)
module HueAttributes = struct
  type t = {
    on : bool option;
    bri : int option;
    hue : int option;
    xy : float list option;
    ct : int option;
    alert : string option;
    effect : string option;
    transitiontime : int option;
  }
  [@@deriving yojson]
end

(* Helper to extract argument from JSON object *)
let get_arg args name convert_fn default =
  match args with
  | `Assoc fields -> (
    match List.Assoc.find fields ~equal:String.equal name with
    | Some v -> convert_fn v
    | None -> default)
  | _ -> default

let get_string_arg args name =
  get_arg args name
    (function
      | `String s -> Some s
      | _ -> None)
    None

let get_bool_arg args name =
  get_arg args name
    (function
      | `Bool b -> Some b
      | _ -> None)
    None

let get_int_arg args name =
  get_arg args name
    (function
      | `Int i -> Some i
      | _ -> None)
    None

let get_dict_arg args name =
  get_arg args name
    (function
      | `Assoc _ as v -> Some v
      | _ -> None)
    None

(* Tool: read_all_lights *)
let () =
  FastMCP.add_simple_tool lights_mcp ~name:"read_all_lights"
    ~description:"Lists the names of all available Hue lights using phue2."
    ~handler:(fun _ ->
      match _get_bridge () with
      | None -> return (`List [ `String "Error: Bridge not connected" ])
      | Some client -> (
        try
          let%map lights = Phue.get_lights client in
          `List (List.map lights ~f:(fun (l : Phue.light) -> `String l.name))
        with
        | Phue.Phue_exception.Error e ->
          return
            (`List [ `String (Printf.sprintf "Error listing lights: %s" e) ])
        | e ->
          return
            (`List
              [
                `String
                  (Printf.sprintf "Error listing lights: %s" (Exn.to_string e));
              ])))

(* Tool: toggle_light *)
let toggle_light_schema =
  `Assoc
    [
      ("type", `String "object");
      ( "properties",
        `Assoc
          [
            ("light_name", `Assoc [ ("type", `String "string") ]);
            ("state", `Assoc [ ("type", `String "boolean") ]);
          ] );
      ("required", `List [ `String "light_name"; `String "state" ]);
    ]

let () =
  FastMCP.add_simple_tool lights_mcp ~name:"toggle_light"
    ~description:"Turns a specific light on (true) or off (false) using phue2."
    ~parameters:toggle_light_schema ~handler:(fun args ->
      let light_name_opt = get_string_arg args "light_name" in
      let state_opt = get_bool_arg args "state" in
      match light_name_opt, state_opt with
      | None, _ -> return (`Assoc [ ("error", `String "Missing required argument: light_name"); ("success", `Bool false) ])
      | _, None -> return (`Assoc [ ("error", `String "Missing required argument: state"); ("success", `Bool false) ])
      | Some light_name, Some state ->
      match _get_bridge () with
      | None ->
        return
          (`Assoc
            [
              ("error", `String "Bridge not connected"); ("success", `Bool false);
            ])
      | Some client -> (
        try
          (* We need the ID, so list all and find by name *)
          let%bind lights = Phue.get_lights client in
          let target_light = List.find lights ~f:(fun l -> String.equal l.name light_name) in
          match target_light with
          | None -> return (`Assoc [ ("error", `String "Light not found"); ("success", `Bool false) ])
          | Some l ->
            let%map result = Phue.set_light_state client l.id (`Assoc [("on", `Bool state)]) in
            `Assoc
              [
                ("light", `String light_name);
                ("set_on_state", `Bool state);
                ("success", `Bool true);
                ("phue_result", result);
              ]
        with e -> return (handle_phue_error light_name "toggle_light" e)))

(* Tool: set_brightness *)
let set_brightness_schema =
  `Assoc
    [
      ("type", `String "object");
      ( "properties",
        `Assoc
          [
            ("light_name", `Assoc [ ("type", `String "string") ]);
            ("brightness", `Assoc [ ("type", `String "integer") ]);
          ] );
      ("required", `List [ `String "light_name"; `String "brightness" ]);
    ]

let () =
  FastMCP.add_simple_tool lights_mcp ~name:"set_brightness"
    ~description:"Sets the brightness of a specific light (0-254) using phue2."
    ~parameters:set_brightness_schema ~handler:(fun args ->
      let light_name_opt = get_string_arg args "light_name" in
      let brightness_opt = get_int_arg args "brightness" in
      match light_name_opt, brightness_opt with
      | None, _ -> return (`Assoc [ ("error", `String "Missing required argument: light_name"); ("success", `Bool false) ])
      | _, None -> return (`Assoc [ ("error", `String "Missing required argument: brightness"); ("success", `Bool false) ])
      | Some light_name, Some brightness ->
      match _get_bridge () with
      | None ->
        return
          (`Assoc
            [
              ("error", `String "Bridge not connected"); ("success", `Bool false);
            ])
      | Some client -> (
        if brightness < 0 || brightness > 254 then
          return
            (`Assoc
              [
                ("light", `String light_name);
                ("error", `String "Brightness must be between 0 and 254");
                ("success", `Bool false);
              ])
        else
          try
             (* We need the ID, so list all and find by name *)
            let%bind lights = Phue.get_lights client in
            let target_light = List.find lights ~f:(fun l -> String.equal l.name light_name) in
            match target_light with
            | None -> return (`Assoc [ ("error", `String "Light not found"); ("success", `Bool false) ])
            | Some l ->
              let%map result = Phue.set_light_state client l.id (`Assoc [("bri", `Int brightness)]) in
              `Assoc
                [
                  ("light", `String light_name);
                  ("set_brightness", `Int brightness);
                  ("success", `Bool true);
                  ("phue_result", result);
                ]
          with e -> return (handle_phue_error light_name "set_brightness" e)))

(* Tool: list_groups *)
let () =
  FastMCP.add_simple_tool lights_mcp ~name:"list_groups"
    ~description:"Lists the names of all available Hue light groups."
    ~handler:(fun _ ->
      match _get_bridge () with
      | None -> return (`List [ `String "Error: Bridge not connected" ])
      | Some client -> (
        try
          let%map groups = Phue.get_groups client in
          `List (List.map groups ~f:(fun (g: Phue.group) -> `String g.name))
        with e ->
          return
            (`List
              [
                `String
                  (Printf.sprintf "Error listing groups: %s" (Exn.to_string e));
              ])))

(* Tool: list_scenes *)
let () =
  FastMCP.add_simple_tool lights_mcp ~name:"list_scenes"
    ~description:"Lists Hue scenes, grouped by the light group they belong to."
    ~handler:(fun _ ->
      match _get_bridge () with
      | None -> return (`List [ `String "Error: Bridge not connected" ])
      | Some client -> (
        try
          let%bind scenes_list = Phue.get_scenes client in
          let%bind groups_list = Phue.get_groups client in
          
          let group_id_to_name =
            List.map groups_list ~f:(fun (g: Phue.group) -> (g.id, g.name))
            |> String.Map.of_alist_exn (* Assuming unique ideas *)
          in

          let scenes_by_group = String.Table.create () in

          List.iter scenes_list ~f:(fun (s: Phue.scene) ->
             let scene_name = s.name in
             match s.group with
             | Some group_id -> (
                match Map.find group_id_to_name group_id with
                | Some group_name ->
                  Hashtbl.add_multi scenes_by_group ~key:group_name ~data:scene_name
                | None -> ())
             | None -> ()
          );

          let result_assoc =
            Hashtbl.to_alist scenes_by_group
            |> List.map ~f:(fun (k, v) ->
                   let sorted_v =
                     List.sort v ~compare:String.compare
                     |> List.dedup_and_sort ~compare:String.compare
                   in
                   (k, `List (List.map sorted_v ~f:(fun s -> `String s))))
          in
          return (`Assoc result_assoc)
        with e ->
          return
            (`List
              [
                `String
                  (Printf.sprintf "Error listing scenes by group: %s"
                     (Exn.to_string e));
              ])))

(* Tool: activate_scene *)
let activate_scene_schema =
  `Assoc
    [
      ("type", `String "object");
      ( "properties",
        `Assoc
          [
            ("group_name", `Assoc [ ("type", `String "string") ]);
            ("scene_name", `Assoc [ ("type", `String "string") ]);
          ] );
      ("required", `List [ `String "group_name"; `String "scene_name" ]);
    ]

let () =
  FastMCP.add_simple_tool lights_mcp ~name:"activate_scene"
    ~description:"Activates a specific scene within a specified light group."
    ~parameters:activate_scene_schema ~handler:(fun args ->
      let group_name_opt = get_string_arg args "group_name" in
      let scene_name_opt = get_string_arg args "scene_name" in
      match group_name_opt, scene_name_opt with
      | None, _ -> return (`Assoc [ ("error", `String "Missing required argument: group_name"); ("success", `Bool false) ])
      | _, None -> return (`Assoc [ ("error", `String "Missing required argument: scene_name"); ("success", `Bool false) ])
      | Some group_name, Some scene_name ->
      match _get_bridge () with
      | None ->
        return
          (`Assoc
            [
              ("error", `String "Bridge not connected"); ("success", `Bool false);
            ])
      | Some client -> (
        try
          (* 1. Find target group ID *)
          let%bind groups_list = Phue.get_groups client in
          let target_group =
             List.find groups_list ~f:(fun (g: Phue.group) -> String.equal g.name group_name)
          in

          match target_group with
          | None ->
            return
              (`Assoc
                [
                  ( "error",
                    `String (Printf.sprintf "Group '%s' not found" group_name)
                  );
                  ("success", `Bool false);
                ])
          | Some g -> (
            (* 2. Find target scene and check association *)
            let%bind scenes_list = Phue.get_scenes client in
            let scene_opt =
              List.find scenes_list ~f:(fun (s: Phue.scene) -> String.equal s.name scene_name)
            in
            match scene_opt with
            | None ->
              return
                (`Assoc
                  [
                    ( "error",
                      `String (Printf.sprintf "Scene '%s' not found" scene_name)
                    );
                    ("success", `Bool false);
                  ])
            | Some s ->
              let scene_in_group =
                match s.group with
                | Some gid -> String.equal gid g.id
                | None -> false
              in
              if not scene_in_group then
                return
                  (`Assoc
                    [
                      ( "error",
                        `String
                          (Printf.sprintf
                             "Scene '%s' does not belong to group '%s'"
                             scene_name group_name) );
                      ("success", `Bool false);
                    ])
              else
                (* 3. Activate *)
                (* Using set_group_action with scene *)
                let%map result = Phue.set_group_action client g.id (`Assoc [("scene", `String s.id)]) in
                (* Assuming success if result is not empty/error *)
                `Assoc
                  [
                    ("group", `String group_name);
                    ("activated_scene", `String scene_name);
                    ("success", `Bool true);
                    ("phue_result", result);
                  ])
        with e ->
          return
            (handle_phue_error
               (group_name ^ "/" ^ scene_name)
               "activate_scene" e)))

(* Tool: set_light_attributes *)
let set_light_attributes_schema =
  `Assoc
    [
      ("type", `String "object");
      ( "properties",
        `Assoc
          [
            ("light_name", `Assoc [ ("type", `String "string") ]);
            ("attributes", `Assoc [ ("type", `String "object") ]);
          ] );
      ("required", `List [ `String "light_name"; `String "attributes" ]);
    ]

let () =
  FastMCP.add_simple_tool lights_mcp ~name:"set_light_attributes"
    ~description:"Sets multiple attributes for a specific light."
    ~parameters:set_light_attributes_schema ~handler:(fun args ->
      let light_name_opt = get_string_arg args "light_name" in
      let attributes_json_opt = get_dict_arg args "attributes" in
      match light_name_opt, attributes_json_opt with
      | None, _ -> return (`Assoc [ ("error", `String "Missing required argument: light_name"); ("success", `Bool false) ])
      | _, None -> return (`Assoc [ ("error", `String "Missing required argument: attributes"); ("success", `Bool false) ])
      | Some light_name, Some attributes_json ->
      let attributes = HueAttributes.t_of_yojson attributes_json in
      match _get_bridge () with
      | None ->
        return
          (`Assoc
            [
              ("error", `String "Bridge not connected"); ("success", `Bool false);
            ])
      | Some client -> (
        try
          let%bind lights = Phue.get_lights client in
          let target_light = List.find lights ~f:(fun l -> String.equal l.name light_name) in
          match target_light with
          | None -> return (`Assoc [ ("error", `String "Light not found"); ("success", `Bool false) ])
          | Some l ->
            (* Convert attributes to yojson *)
            let attributes_json = HueAttributes.yojson_of_t attributes in
            let%map _ = Phue.set_light_state client l.id attributes_json in
            `Assoc [ ("light", `String light_name); ("success", `Bool true) ]
        with e ->
          return (handle_phue_error light_name "set_light_attributes" e)))

(* Tool: set_group_attributes *)
let set_group_attributes_schema =
  `Assoc
    [
      ("type", `String "object");
      ( "properties",
        `Assoc
          [
            ("group_name", `Assoc [ ("type", `String "string") ]);
            ("attributes", `Assoc [ ("type", `String "object") ]);
          ] );
      ("required", `List [ `String "group_name"; `String "attributes" ]);
    ]

let () =
  FastMCP.add_simple_tool lights_mcp ~name:"set_group_attributes"
    ~description:
      "Sets multiple attributes for all lights within a specific group."
    ~parameters:set_group_attributes_schema ~handler:(fun args ->
      let group_name_opt = get_string_arg args "group_name" in
      let attributes_json_opt = get_dict_arg args "attributes" in
      match group_name_opt, attributes_json_opt with
      | None, _ -> return (`Assoc [ ("error", `String "Missing required argument: group_name"); ("success", `Bool false) ])
      | _, None -> return (`Assoc [ ("error", `String "Missing required argument: attributes"); ("success", `Bool false) ])
      | Some group_name, Some attributes_json ->
      let attributes = HueAttributes.t_of_yojson attributes_json in
      match _get_bridge () with
      | None ->
        return
          (`Assoc
            [
              ("error", `String "Bridge not connected"); ("success", `Bool false);
            ])
      | Some client -> (
        try
          let%bind groups = Phue.get_groups client in
          let target_group = List.find groups ~f:(fun g -> String.equal g.name group_name) in
          match target_group with
          | None -> return (`Assoc [ ("error", `String "Group not found"); ("success", `Bool false) ])
          | Some g ->
            (* Convert attributes to yojson *)
            let attributes_json = HueAttributes.yojson_of_t attributes in
            let%map _ = Phue.set_group_action client g.id attributes_json in
            `Assoc [ ("group", `String group_name); ("success", `Bool true) ]
        with e ->
          return (handle_phue_error group_name "set_group_attributes" e)))

(* Tool: list_lights_by_group *)
let () =
  FastMCP.add_simple_tool lights_mcp ~name:"list_lights_by_group"
    ~description:"Lists Hue lights, grouped by the room/group they belong to."
    ~handler:(fun _ ->
      match _get_bridge () with
      | None -> return (`List [ `String "Error: Bridge not connected" ])
      | Some client -> (
        try
          let%bind groups_list = Phue.get_groups client in
          (* For each group, we don't really have "lights by group" API easily available from Phue module 
             The Phue.group type has 'lights' field which is list of IDs.
             So we need to get all lights first to map IDs to Names.
          *)
          let%bind lights_list = Phue.get_lights client in
          let light_id_to_name =
             List.map lights_list ~f:(fun l -> (l.id, l.name))
             |> Int.Map.of_alist_exn
          in

          let lights_by_group = String.Table.create () in

          List.iter groups_list ~f:(fun (g: Phue.group) ->
             let group_name = g.name in
             let light_names = 
               List.filter_map g.lights ~f:(fun lid_str ->
                 try
                   let lid = Int.of_string lid_str in
                   Map.find light_id_to_name lid
                 with _ -> None
               )
             in
             if not (List.is_empty light_names) then
                 Hashtbl.add_exn lights_by_group ~key:group_name ~data:(List.sort light_names ~compare:String.compare)
          );

          let result_assoc =
            Hashtbl.to_alist lights_by_group
            |> List.map ~f:(fun (k, v) ->
                   (k, `List (List.map v ~f:(fun s -> `String s))))
          in
          return (`Assoc result_assoc)
        with e ->
          return
            (`List
              [
                `String
                  (Printf.sprintf "Error listing lights by group: %s"
                     (Exn.to_string e));
              ])))
