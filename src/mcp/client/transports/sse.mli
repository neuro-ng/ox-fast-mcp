open Core
open Async
module Types = Mcp.Types

(** SSE client transport for OxFastMCP *)

val remove_request_params : string -> string
(** Remove query parameters from URL *)

val validate_endpoint_origin :
  connection_url:string -> endpoint_url:string -> unit Or_error.t
(** Validate endpoint origin matches connection origin *)

type sse_event = { event : string; data : string } [@@deriving sexp_of]
(** SSE event representation *)

type config [@@deriving sexp_of]
(** SSE client configuration *)

val create_config :
  url:string ->
  ?headers:(string * string) list ->
  ?timeout:Time_float.Span.t ->
  ?sse_read_timeout:Time_float.Span.t ->
  unit ->
  config
(** Create SSE client configuration

    @param url The SSE endpoint URL
    @param headers Optional HTTP headers to include in requests
    @param timeout HTTP timeout for regular operations (default 5s)
    @param sse_read_timeout Timeout for SSE read operations (default 300s) *)

type t
(** SSE connection handle *)

val connect : config -> t
(** Connect to SSE endpoint

    Returns a connection handle with bidirectional streams:
    - read_stream: receives messages from server (or errors)
    - write_stream: sends messages to server *)

val close : t -> unit Deferred.t
(** Close SSE connection *)

val is_connected : t -> bool
(** Check if connection is active *)

val endpoint_url : t -> string option
(** Get the current endpoint URL (if connected) *)

val read_stream :
  t -> [ `Message of Types.jsonrpc_message | `Error of Error.t ] Pipe.Reader.t
(** Access the read stream *)

val write_stream : t -> Types.jsonrpc_message Pipe.Writer.t
(** Access the write stream *)
