(** MCP Configuration types for server transport configuration *)

open! Core
open! Async

(** Transport types supported by MCP servers *)
module Transport_type : sig
  type t = Stdio | Sse | Streamable_http
  [@@deriving sexp, compare, equal, yojson]

  val to_string : t -> string
  val of_string : string -> t
end

type stdio_mcp_server = {
  command : string;
  args : string list;
  env : (string * string) list;
  cwd : string option;
  timeout : int option;
  description : string option;
  icon : string option;
}
[@@deriving sexp, compare, yojson]
(** Stdio MCP server configuration *)

val create_stdio_server :
  command:string ->
  ?args:string list ->
  ?env:(string * string) list ->
  ?cwd:string ->
  ?timeout:int ->
  ?description:string ->
  ?icon:string ->
  unit ->
  stdio_mcp_server

type remote_mcp_server = {
  url : string;
  transport : Transport_type.t option;
  headers : (string * string) list;
  sse_read_timeout : float option;
  timeout : int option;
  description : string option;
  icon : string option;
}
[@@deriving sexp, compare, yojson]
(** Remote MCP server configuration for HTTP/SSE transport *)

val create_remote_server :
  url:string ->
  ?transport:Transport_type.t ->
  ?headers:(string * string) list ->
  ?sse_read_timeout:float ->
  ?timeout:int ->
  ?description:string ->
  ?icon:string ->
  unit ->
  remote_mcp_server

(** MCP server configuration - either stdio or remote *)
type mcp_server = Stdio of stdio_mcp_server | Remote of remote_mcp_server
[@@deriving sexp, compare]

val mcp_server_of_yojson : Yojson.Safe.t -> mcp_server
val yojson_of_mcp_server : mcp_server -> Yojson.Safe.t

type mcp_config = { mcp_servers : (string * mcp_server) list }
[@@deriving sexp, compare, yojson]
(** MCP configuration containing multiple servers *)

val create_config : ?servers:(string * mcp_server) list -> unit -> mcp_config
val add_server : mcp_config -> name:string -> server:mcp_server -> mcp_config
val get_server : mcp_config -> name:string -> mcp_server option
val remove_server : mcp_config -> name:string -> mcp_config

val infer_transport_type_from_url : string -> Transport_type.t
(** Infer transport type from URL *)

val get_transport_type : remote_mcp_server -> Transport_type.t
(** Get the effective transport type for a remote server *)

val load_from_file : string -> mcp_config Deferred.t
(** Load MCP config from a JSON file *)

val save_to_file : mcp_config -> string -> unit Deferred.t
(** Save MCP config to a JSON file *)

val update_config_file :
  path:string -> name:string -> server:mcp_server -> unit Deferred.t
(** Update a server in a config file *)
