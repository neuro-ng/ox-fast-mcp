open Async

(** Simplified test client for in-process server testing *)

type initialize_result = {
  server_info : Mcp.Types.implementation;
  capabilities : Mcp.Types.server_capabilities;
}

type t

val create : Server.Ox_fast_mcp.t -> t Deferred.t
(** Create test client connected to server *)

val close : t -> unit Deferred.t
(** Close client and cleanup *)

val initialize_result : t -> initialize_result
(** Get initialization result *)

val list_tools_json : t -> Yojson.Safe.t list Deferred.t
(** List operations - return raw JSON from server *)

val list_resources_json : t -> Yojson.Safe.t list Deferred.t

val call_tool :
  t -> string -> (string * Yojson.Safe.t) list -> Yojson.Safe.t Deferred.t
(** Tool operations *)

val read_resource : t -> string -> string Deferred.t
(** Resource operations *)

val get_prompt :
  t -> string -> (string * Yojson.Safe.t) list -> Yojson.Safe.t Deferred.t
(** Prompt operations *)
