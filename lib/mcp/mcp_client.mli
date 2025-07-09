open Core
open Async
open Types

(** Transport module for MCP client *)
module Transport : sig
  type t = {
    send : jsonrpc_message -> unit Deferred.t;
    receive : unit -> jsonrpc_message Deferred.t;
    close : unit -> unit Deferred.t;
  }

  val create :
    send:(jsonrpc_message -> unit Deferred.t) ->
    receive:(unit -> jsonrpc_message Deferred.t) ->
    close:(unit -> unit Deferred.t) ->
    t
end

type t
(** MCP client type *)

val create : Transport.t -> t
(** Create a new MCP client *)

val close : t -> unit Deferred.t
(** Close the client connection *)

val send_request :
  t ->
  id:request_id ->
  method_:string ->
  params:Yojson.Safe.t option ->
  jsonrpc_message Deferred.t
(** Send a request to the server *)

val send_notification :
  t -> method_:string -> params:Yojson.Safe.t option -> unit Deferred.t
(** Send a notification to the server *)

val receive : t -> jsonrpc_message Deferred.t
(** Receive a message from the server *)

val initialize :
  t ->
  client_info:implementation ->
  capabilities:client_capabilities ->
  (initialize_result, Mcp_error.t) Result.t Deferred.t
(** Initialize the client *)

val initialized : t -> (unit, Mcp_error.t) Result.t Deferred.t
(** Send initialized notification *)

val ping : t -> jsonrpc_message Deferred.t
(** Send ping request *)

val set_level : t -> level:logging_level -> jsonrpc_message Deferred.t
(** Set logging level *)

val list_resources : t -> ?cursor:cursor -> unit -> jsonrpc_message Deferred.t
(** List available resources *)

val read_resource : t -> uri:string -> jsonrpc_message Deferred.t
(** Read a resource *)

val list_tools : t -> ?cursor:cursor -> unit -> jsonrpc_message Deferred.t
(** List available tools *)

val call_tool :
  t ->
  name:string ->
  arguments:Yojson.Safe.t option ->
  jsonrpc_message Deferred.t
(** Call a tool *)

val list_prompts : t -> ?cursor:cursor -> unit -> jsonrpc_message Deferred.t
(** List available prompts *)

val get_prompt :
  t ->
  name:string ->
  ?arguments:(string * string) list ->
  unit ->
  jsonrpc_message Deferred.t
(** Get a specific prompt *)

val list_roots : t -> jsonrpc_message Deferred.t
(** List roots *)

val create_message :
  t ->
  messages:message list ->
  model_preferences:model_preferences option ->
  system_prompt:string option ->
  include_context:include_context option ->
  temperature:float option ->
  max_tokens:int ->
  stop_sequences:string list option ->
  metadata:Yojson.Safe.t option ->
  jsonrpc_message Deferred.t
(** Create a message *)

val elicit :
  t ->
  message:string ->
  requested_schema:Yojson.Safe.t ->
  jsonrpc_message Deferred.t
(** Elicit information *)

val subscribe : t -> uri:string -> jsonrpc_message Deferred.t
(** Subscribe to resource updates *)

val unsubscribe : t -> uri:string -> jsonrpc_message Deferred.t
(** Unsubscribe from resource updates *)
