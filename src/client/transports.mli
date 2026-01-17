(** Client Transport Implementations

    Provides various transport mechanisms for connecting MCP clients to servers. *)

open Core
open Async
module Mcp_client = Mcp_client
module Session = Mcp_client.Session

(** {1 Session Configuration} *)

type sampling_fn = unit -> unit Deferred.t
type list_roots_fn = unit -> unit Deferred.t
type logging_fn = string -> unit Deferred.t
type elicitation_fn = unit -> unit Deferred.t
type message_handler_fn = unit -> unit Deferred.t
type implementation = { name : string; version : string }

type session_kwargs = {
  read_timeout_seconds : Time_ns.Span.t option;
  sampling_callback : sampling_fn option;
  list_roots_callback : list_roots_fn option;
  logging_callback : logging_fn option;
  elicitation_callback : elicitation_fn option;
  message_handler : message_handler_fn option;
  client_info : implementation option;
}

val default_session_kwargs : session_kwargs

(** {1 Client Session} *)

module Client_session = Session

(** {1 Transport Types} *)

type auth_config =
  | No_auth
  | OAuth
  | Bearer of string
  | Custom of (unit -> unit Deferred.t)

type sse_config = {
  url : string;
  headers : (string * string) list;
  auth : auth_config;
  sse_read_timeout : Time_ns.Span.t option;
}

type streamable_http_config = {
  url : string;
  headers : (string * string) list;
  auth : auth_config;
  sse_read_timeout : Time_ns.Span.t option;
}

type ws_config = { url : string }

type stdio_config = {
  command : string;
  args : string list;
  env : (string * string) list option;
  cwd : string option;
  keep_alive : bool;
  log_file : string option;
}

type python_stdio_config = {
  script_path : string;
  args : string list option;
  env : (string * string) list option;
  cwd : string option;
  python_cmd : string;
  keep_alive : bool;
  log_file : string option;
}

type t =
  | Ws_transport of ws_config
  | Sse_transport of sse_config
  | Streamable_http_transport of streamable_http_config
  | Stdio_transport of stdio_config
  | Python_stdio_transport of python_stdio_config
  | Node_stdio_transport of python_stdio_config
  | Fastmcp_stdio_transport of python_stdio_config
  | Uv_stdio_transport of stdio_config
  | Uvx_stdio_transport of stdio_config
  | Npx_stdio_transport of stdio_config
  | Fastmcp_transport of string
  | Mcp_config_transport of string

(** {1 Constructors} *)

val create_ws_transport : string -> (t, Error.t) Result.t

val create_sse_transport :
  ?headers:(string * string) list ->
  ?auth:auth_config ->
  ?sse_read_timeout:Time_ns.Span.t option ->
  string ->
  (t, Error.t) Result.t

val create_streamable_http_transport :
  ?headers:(string * string) list ->
  ?auth:auth_config ->
  ?sse_read_timeout:Time_ns.Span.t option ->
  string ->
  (t, Error.t) Result.t

val create_stdio_transport :
  ?env:(string * string) list option ->
  ?cwd:string option ->
  ?keep_alive:bool ->
  ?log_file:string option ->
  string ->
  string list ->
  (t, Error.t) Result.t

val create_python_stdio_transport :
  ?args:string list option ->
  ?env:(string * string) list option ->
  ?cwd:string option ->
  ?python_cmd:string ->
  ?keep_alive:bool ->
  ?log_file:string option ->
  string ->
  (t, Error.t) Result.t

(** {1 Operations} *)

val connect_session :
  t -> session_kwargs -> (Client_session.t, Error.t) Result.t Deferred.t

val close : t -> unit Deferred.t
val to_string : t -> string

(** {1 Type Inference} *)

val infer_transport_from_string : string -> (t, Error.t) Result.t Deferred.t
