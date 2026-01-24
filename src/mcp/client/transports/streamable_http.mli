open Core
open Async
module Types = Mcp.Types

(** StreamableHTTP client transport for OxFastMCP *)

val mcp_session_id : string
(** HTTP header constants *)

val mcp_protocol_version : string
val last_event_id : string
val content_type_header : string
val accept_header : string

val json_content_type : string
(** Content type constants *)

val sse_content_type : string

(** StreamableHTTP transport errors *)
module Error : sig
  type t = Streamable_http_error of string | Resumption_error of string
  [@@deriving sexp_of]

  val to_string : t -> string
end

type request_context [@@deriving sexp_of]
(** Request context for processing *)

type config [@@deriving sexp_of]
(** StreamableHTTP transport configuration *)

val create_config :
  url:string ->
  ?headers:(string * string) list ->
  ?timeout:Time_float.Span.t ->
  ?sse_read_timeout:Time_float.Span.t ->
  unit ->
  config
(** Create StreamableHTTP configuration

    @param url The endpoint URL
    @param headers Optional HTTP headers to include in requests
    @param timeout HTTP timeout for regular operations (default 30s)
    @param sse_read_timeout Timeout for SSE read operations (default 300s) *)

type t
(** StreamableHTTP transport handle *)

val prepare_request_headers :
  t -> (string * string) list -> (string * string) list
(** Prepare request headers with session ID and protocol version *)

val is_initialization_request : Types.jsonrpc_message -> bool
(** Check if message is an initialization request *)

val is_initialized_notification : Types.jsonrpc_message -> bool
(** Check if message is an initialized notification *)

val maybe_extract_session_id_from_response : t -> Cohttp.Response.t -> unit
(** Extract session ID from response headers *)

val maybe_extract_protocol_version_from_message :
  t -> Types.jsonrpc_message -> unit
(** Extract protocol version from initialization response *)

val get_session_id : t -> string option
(** Get current session ID *)

val connect : config -> t
(** Create StreamableHTTP transport connection *)

val close : t -> terminate_on_close:bool -> unit Deferred.t
(** Close StreamableHTTP connection

    @param terminate_on_close If true, send DELETE to terminate session *)

val read_stream :
  t -> [ `Message of Types.jsonrpc_message | `Error of Error.t ] Pipe.Reader.t
(** Access the read stream *)

val write_stream : t -> Types.jsonrpc_message Pipe.Writer.t
(** Access the write stream *)
