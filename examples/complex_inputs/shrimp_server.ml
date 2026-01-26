open Core
open Async

(* open Ox_fast_mcp - unused *)
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* Define the input types *)
module Shrimp = struct
  type t = { name : string [@key "name"] } [@@deriving yojson, sexp]
end

module Shrimp_tank = struct
  type t = { shrimp : Shrimp.t list [@key "shrimp"] } [@@deriving yojson, sexp]
end

(* Define the arguments for the tool *)
module Name_shrimp_args = struct
  type t = {
    tank : Shrimp_tank.t; [@key "tank"]
    extra_names : string list; [@key "extra_names"]
  }
  [@@deriving yojson, sexp]
end

module FastMCP = Server.Ox_fast_mcp

let create () =
  let server = FastMCP.create ~name:"Shrimp Tank" () in

  (* Manually construction the JSON schema for the complex input *)
  (* In a more advanced version, we might generate this from the type *)
  let parameters =
    `Assoc
      [
        ("type", `String "object");
        ( "properties",
          `Assoc
            [
              ( "tank",
                `Assoc
                  [
                    ("type", `String "object");
                    ( "properties",
                      `Assoc
                        [
                          ( "shrimp",
                            `Assoc
                              [
                                ("type", `String "array");
                                ( "items",
                                  `Assoc
                                    [
                                      ("type", `String "object");
                                      ( "properties",
                                        `Assoc
                                          [
                                            ( "name",
                                              `Assoc
                                                [
                                                  ("type", `String "string");
                                                  ("maxLength", `Int 10);
                                                ] );
                                          ] );
                                      ("required", `List [ `String "name" ]);
                                    ] );
                              ] );
                        ] );
                    ("required", `List [ `String "shrimp" ]);
                  ] );
              ( "extra_names",
                `Assoc
                  [
                    ("type", `String "array");
                    ( "items",
                      `Assoc
                        [ ("type", `String "string"); ("maxLength", `Int 10) ]
                    );
                  ] );
            ] );
        ("required", `List [ `String "tank"; `String "extra_names" ]);
      ]
  in

  FastMCP.add_simple_tool server ~name:"name_shrimp"
    ~description:"List all shrimp names in the tank" ~parameters
    ~handler:(fun args_json ->
      match Name_shrimp_args.t_of_yojson args_json with
      | args ->
        let shrimp_names = List.map args.tank.shrimp ~f:(fun s -> s.name) in
        let all_names = shrimp_names @ args.extra_names in
        return (`List (List.map all_names ~f:(fun s -> `String s)))
      | exception exn -> raise_s [%message "Invalid arguments" (exn : exn)]);

  server

let main () =
  let server = create () in
  let transport = Server.Transport.Stdio in
  FastMCP.run_async ~transport server ()
