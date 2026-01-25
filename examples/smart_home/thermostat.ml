open Core
open Async
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module FastMCP = Server.Ox_fast_mcp

(* -------------------------------------------------------------------------- *)
(* Mock Thermostat State                                                      *)
(* -------------------------------------------------------------------------- *)

module Mode = struct
  type t = Off | Heat | Cool [@@deriving yojson, sexp, equal]

  let to_string = function
    | Off -> "off"
    | Heat -> "heat"
    | Cool -> "cool"

  let of_string = function
    | "off" -> Some Off
    | "heat" -> Some Heat
    | "cool" -> Some Cool
    | _ -> None
end

type state = {
  mutable temperature : float;
  mutable target_temperature : float;
  mutable mode : Mode.t;
}
[@@deriving yojson, sexp]

let current_state = {
  temperature = 72.0;
  target_temperature = 70.0;
  mode = Mode.Off;
}

(* -------------------------------------------------------------------------- *)
(* Thermostat MCP                                                             *)
(* -------------------------------------------------------------------------- *)

let thermostat_mcp = FastMCP.create ~name:"Thermostat Service" ()

(* Helper to extract argument from JSON object *)
let get_arg args name convert_fn default =
  match args with
  | `Assoc fields ->
      (match List.Assoc.find fields ~equal:String.equal name with
       | Some v -> convert_fn v
       | None -> default)
  | _ -> default

let get_string_arg args name = 
  get_arg args name (function `String s -> Some s | _ -> None) None

let get_float_arg args name = 
  get_arg args name (function `Float f -> Some f | `Int i -> Some (Float.of_int i) | _ -> None) None

(* Tool: get_thermostat_state *)
let () =
  FastMCP.add_simple_tool thermostat_mcp ~name:"get_thermostat_state"
    ~description:"Returns the current state of the thermostat."
    ~handler:(fun _ ->
      return (yojson_of_state current_state))

(* Tool: set_target_temperature *)
let set_target_temperature_schema =
  `Assoc [
    ("type", `String "object");
    ("properties", `Assoc [
      ("target", `Assoc [("type", `String "number")]);
    ]);
    ("required", `List [`String "target"]);
  ]

let () =
  FastMCP.add_simple_tool thermostat_mcp ~name:"set_target_temperature"
    ~description:"Sets the target temperature of the thermostat."
    ~parameters:set_target_temperature_schema
    ~handler:(fun args ->
      let target_opt = get_float_arg args "target" in
      match target_opt with
      | None -> return (`Assoc [ ("error", `String "Missing required argument: target"); ("success", `Bool false) ])
      | Some target ->
      current_state.target_temperature <- target;
      return (`Assoc [("success", `Bool true); ("target_temperature", `Float target)])
    )

(* Tool: set_mode *)
let set_mode_schema =
  `Assoc [
    ("type", `String "object");
    ("properties", `Assoc [
      ("mode", `Assoc [("type", `String "string"); ("enum", `List [`String "off"; `String "heat"; `String "cool"])]);
    ]);
    ("required", `List [`String "mode"]);
  ]

let () =
  FastMCP.add_simple_tool thermostat_mcp ~name:"set_mode"
    ~description:"Sets the operating mode of the thermostat (off, heat, cool)."
    ~parameters:set_mode_schema
    ~handler:(fun args ->
      let mode_opt = get_string_arg args "mode" in
      match mode_opt with
      | None -> return (`Assoc [ ("error", `String "Missing required argument: mode"); ("success", `Bool false) ])
      | Some mode_str ->
      match Mode.of_string mode_str with
      | Some m ->
          current_state.mode <- m;
          return (`Assoc [("success", `Bool true); ("mode", `String (Mode.to_string m))])
      | None ->
          return (`Assoc [("success", `Bool false); ("error", `String "Invalid mode")])
    )
