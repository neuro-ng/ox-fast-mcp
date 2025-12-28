(** STDIO Transport Module

    Provides STDIO transport for OxFastMCP server, enabling communication via
    stdin/stdout following the MCP protocol specification.

    This is a pure Async implementation that reads JSON-RPC messages from stdin
    and writes responses to stdout. *)

open! Core
open! Async

type t = {
  server : Server.Ox_fast_mcp.t;
  handlers : Server.Protocol.method_map;
}

(** Handle a single MCP message and return response *)
let handle_message (handlers : Server.Protocol.method_map)
    (message : Yojson.Safe.t) : Yojson.Safe.t Deferred.t =
  (* Extract method and params from JSON-RPC message *)
  match message with
  | `Assoc fields -> (
    match
      ( List.Assoc.find fields ~equal:String.equal "method",
        List.Assoc.find fields ~equal:String.equal "params",
        List.Assoc.find fields ~equal:String.equal "id" )
    with
    | Some (`String method_name), params_opt, id_opt -> (
      (* Look up handler for this method *)
      match Hashtbl.find handlers method_name with
      | Some handler ->
        (* Create context for this request *)
        let ctx =
          Context.create ~method_name:(Some method_name) ~params:params_opt ()
        in
        (* Call handler and wrap response in JSON-RPC format *)
        let%bind result = handler ctx in
        let response_fields =
          [ ("jsonrpc", `String "2.0"); ("result", result) ]
        in
        let response_fields =
          match id_opt with
          | Some id -> ("id", id) :: response_fields
          | None -> response_fields
        in
        return (`Assoc response_fields)
      | None ->
        (* Method not found *)
        let error =
          `Assoc
            [
              ("code", `Int (-32601));
              ("message", `String "Method not found");
              ("data", `Assoc [ ("method", `String method_name) ]);
            ]
        in
        let response_fields =
          [ ("jsonrpc", `String "2.0"); ("error", error) ]
        in
        let response_fields =
          match id_opt with
          | Some id -> ("id", id) :: response_fields
          | None -> response_fields
        in
        return (`Assoc response_fields))
    | _ ->
      (* Invalid request format *)
      let error =
        `Assoc
          [
            ("code", `Int (-32600));
            ("message", `String "Invalid request");
            ("data", message);
          ]
      in
      return (`Assoc [ ("jsonrpc", `String "2.0"); ("error", error) ]))
  | _ ->
    (* Not a JSON object *)
    let error =
      `Assoc
        [
          ("code", `Int (-32600));
          ("message", `String "Invalid request - expected object");
        ]
    in
    return (`Assoc [ ("jsonrpc", `String "2.0"); ("error", error) ])

(** Handle MCP initialization request *)
let handle_initialize (server : Server.Ox_fast_mcp.t) (id : Yojson.Safe.t) :
    Yojson.Safe.t =
  let server_info =
    `Assoc
      [
        ("name", `String (Server.Ox_fast_mcp.name server));
        ( "version",
          match Server.Ox_fast_mcp.version server with
          | Some v -> `String v
          | None -> `String "0.1.0" );
      ]
  in
  let capabilities =
    `Assoc
      [
        ( "tools",
          `Assoc
            [ ("listChanged", `Bool true); ("supportsProgress", `Bool false) ]
        );
        ( "resources",
          `Assoc [ ("subscribe", `Bool false); ("listChanged", `Bool true) ] );
        ("prompts", `Assoc [ ("listChanged", `Bool true) ]);
      ]
  in
  let result =
    `Assoc
      [
        ("protocolVersion", `String "2024-11-05");
        ("serverInfo", server_info);
        ("capabilities", capabilities);
      ]
  in
  `Assoc [ ("jsonrpc", `String "2.0"); ("id", id); ("result", result) ]

(** Read a single JSON-RPC message from stdin *)
let read_message (reader : Reader.t) : Yojson.Safe.t option Deferred.t =
  match%bind Reader.read_line reader with
  | `Ok line -> (
    try
      let json = Yojson.Safe.from_string line in
      return (Some json)
    with exn ->
      Logging.Global.error
        (sprintf "Failed to parse JSON: %s - %s" line (Exn.to_string exn));
      return None)
  | `Eof -> return None

(** Write a JSON-RPC message to stdout *)
let write_message (writer : Writer.t) (message : Yojson.Safe.t) :
    unit Deferred.t =
  let json_str = Yojson.Safe.to_string message in
  Writer.write_line writer json_str;
  Writer.flushed writer

(** Main STDIO transport loop *)
let run (server : Server.Ox_fast_mcp.t) : unit Deferred.t =
  Logging.Global.info "Starting STDIO transport";

  (* Setup protocol handlers *)
  let handlers = Server.Ox_fast_mcp.setup_handlers server in

  (* Get stdin/stdout *)
  let reader = Lazy.force Reader.stdin in
  let writer = Lazy.force Writer.stdout in

  (* Message processing loop *)
  let rec process_loop () =
    match%bind read_message reader with
    | None ->
      Logging.Global.info "EOF on stdin, shutting down";
      return ()
    | Some message -> (
      (* Check if this is initialize *)
      match message with
      | `Assoc fields -> (
        match
          ( List.Assoc.find fields ~equal:String.equal "method",
            List.Assoc.find fields ~equal:String.equal "id" )
        with
        | Some (`String "initialize"), Some id ->
          (* Handle initialization *)
          let response = handle_initialize server id in
          let%bind () = write_message writer response in
          process_loop ()
        | Some (`String "initialized"), _ ->
          (* Initialized notification - just acknowledge *)
          process_loop ()
        | Some (`String _method_name), _id_opt ->
          (* Handle regular message *)
          let%bind response = handle_message handlers message in
          let%bind () = write_message writer response in
          process_loop ()
        | _ ->
          (* Invalid message format *)
          Logging.Global.warning "Received message without method field";
          process_loop ())
      | _ ->
        (* Not an object *)
        Logging.Global.warning "Received non-object message";
        process_loop ())
  in

  process_loop ()
