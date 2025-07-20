open! Core
open! Async

module Transport_type : sig
  type t = Http | Sse | Stdio [@@deriving yojson, compare, sexp]
end

val infer_transport_type_from_url : string -> Transport_type.t

module Stdio_mcp_server : sig
  type t = {
    command : string;
    args : string list;
    env : (string * string) list;
    cwd : string option;
    transport : Transport_type.t;
  }
  [@@deriving yojson, compare, sexp]

  val to_transport : t -> Client.Transports.stdio_transport
end

module Remote_mcp_server : sig
  type auth = Bearer of string | OAuth | Custom of Client.Auth.t
  [@@deriving compare, sexp]

  val auth_of_yojson : Yojson.Safe.t -> (auth, string) result
  val yojson_of_auth : auth -> Yojson.Safe.t

  type t = {
    url : string;
    headers : (string * string) list;
    transport : Transport_type.t option;
    auth : auth option;
  }
  [@@deriving yojson, compare, sexp]

  val to_transport : t -> Client.Transports.streamable_transport
end

module Mcp_config : sig
  type server = Stdio of Stdio_mcp_server.t | Remote of Remote_mcp_server.t
  [@@deriving yojson, compare, sexp]

  type t = { mcp_servers : (string * server) list }
  [@@deriving yojson, compare, sexp]

  val of_dict : Yojson.Safe.t -> t
end
