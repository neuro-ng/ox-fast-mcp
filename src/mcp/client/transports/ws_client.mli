open Core
open Async
module Types = Mcp.Types

(** WebSocket client transport for OxFastMCP *)

type config [@@deriving sexp_of]
(** WebSocket client configuration *)

val create_config : url:string -> ?subprotocol:string -> unit -> config
(** Create WebSocket client configuration

    @param url The WebSocket endpoint URL (e.g., ws://localhost:8000/ws)
    @param subprotocol WebSocket subprotocol to use (default "mcp") *)

type t
(** WebSocket connection handle *)

val connect : config -> t
(** Connect to WebSocket endpoint

    Establishes WebSocket connection with 'mcp' subprotocol and returns
    bidirectional streams for message exchange. *)

val close : t -> unit Deferred.t
(** Close WebSocket connection *)

val is_connected : t -> bool
(** Check if WebSocket is connected *)

val read_stream :
  t -> [ `Message of Types.jsonrpc_message | `Error of Error.t ] Pipe.Reader.t
(** Access the read stream *)

val write_stream : t -> Types.jsonrpc_message Pipe.Writer.t
(** Access the write stream *)
