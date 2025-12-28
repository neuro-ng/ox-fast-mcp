(** STDIO Transport Module Interface

    Provides STDIO transport for OxFastMCP server, enabling communication via
    stdin/stdout following the MCP protocol specification. *)

open! Core
open! Async

type t
(** STDIO transport state *)

val run : Server.Ox_fast_mcp.t -> unit Deferred.t
(** Create and run STDIO transport for the given server.

    This function:
    1. Sets up stdin/stdout streaming
    2. Handles MCP initialization handshake
    3. Routes incoming messages to protocol handlers
    4. Sends responses back via stdout

    @param server The OxFastMCP server instance
    @return Deferred that completes when transport shuts down *)

val handle_message :
  Server.Protocol.method_map -> Yojson.Safe.t -> Yojson.Safe.t Deferred.t
(** Handle a single MCP message and return response.

    @param handlers Protocol handler map
    @param message Incoming JSON-RPC message
    @return JSON-RPC response message *)
