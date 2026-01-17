open Core
open Async
module Types = Mcp.Types

[@@@warning "-32"] (* Skeleton implementation has unused functions *)

(** WebSocket client transport for OxFastMCP.

    Provides client-side WebSocket transport for MCP, symmetrical to the server
    version. Connects using the 'mcp' subprotocol. *)

type config = { url : string; subprotocol : string [@default "mcp"] }
[@@deriving sexp_of]
(** WebSocket client configuration *)

(** Create WebSocket client configuration *)
let create_config ~url ?(subprotocol = "mcp") () = { url; subprotocol }

type t = {
  config : config;
  read_stream :
    [ `Message of Types.jsonrpc_message | `Error of Error.t ] Pipe.Reader.t;
  write_stream : Types.jsonrpc_message Pipe.Writer.t;
  mutable is_connected : bool;
}
(** WebSocket connection state *)

(** WebSocket reader task - reads messages from WebSocket *)
let ws_reader ~read_writer:_ () =
  let () = Logs.debug (fun m -> m "Starting WebSocket reader") in
  (* Note: This is a skeleton implementation. Full implementation would: 1.
     Connect to WebSocket endpoint with 'mcp' subprotocol 2. Read text messages
     from WebSocket 3. Parse each message as JSON-RPC using
     Types.jsonrpc_message_of_yojson 4. Send SessionMessage to
     read_stream_writer 5. On parse/validation error, send exception to
     read_stream_writer *)
  Deferred.never ()

(** WebSocket writer task - writes messages to WebSocket *)
let ws_writer ~write_reader:_ () =
  let () = Logs.debug (fun m -> m "Starting WebSocket writer") in
  (* Note: This is a skeleton implementation. Full implementation would: 1. Read
     SessionMessage objects from write_stream_reader 2. Convert message to JSON
     using Types.jsonrpc_message_to_yojson 3. Send JSON string over WebSocket 4.
     Handle errors *)
  Deferred.never ()

(** Connect to WebSocket endpoint

    Returns a connection handle with bidirectional streams. The implementation
    would:
    1. Connect using websockets library with 'mcp' subprotocol
    2. Start reader and writer tasks concurrently
    3. Yield read_stream and write_stream
    4. Cancel tasks on close *)
let connect config =
  let read_reader, _read_writer = Pipe.create () in
  let _write_reader, write_stream = Pipe.create () in
  let () = Logs.info (fun m -> m "Connecting to WebSocket: %s" config.url) in
  (* Note: Full implementation would: 1. Establish WebSocket connection with
     subprotocol 2. Start ws_reader and ws_writer tasks in task group 3. Set
     is_connected = true 4. Return connection handle *)
  { config; read_stream = read_reader; write_stream; is_connected = false }

(** Close WebSocket connection *)
let close t =
  let () = Logs.info (fun m -> m "Closing WebSocket connection") in
  t.is_connected <- false;
  (* Note: Full implementation would: 1. Cancel task group 2. Close WebSocket
     connection 3. Close pipes *)
  Pipe.close_read t.read_stream;
  Pipe.close t.write_stream;
  return ()

(** Check if WebSocket is connected *)
let is_connected t = t.is_connected

(** Access the read stream *)
let read_stream t = t.read_stream

(** Access the write stream *)
let write_stream t = t.write_stream
