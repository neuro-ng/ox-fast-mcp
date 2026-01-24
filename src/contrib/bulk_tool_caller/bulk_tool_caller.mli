(** Bulk Tool Caller for OxFastMCP

    Provides bulk tool calling functionality for OxFastMCP servers. Enables
    calling multiple tools in a single request for improved performance. *)

open! Core
open! Async

(** {1 Types} *)

type json = Yojson.Safe.t
(** JSON type alias for Yojson.Safe.t *)

val yojson_of_json : json -> Yojson.Safe.t
val json_of_yojson : Yojson.Safe.t -> json
val compare_json : json -> json -> int
val sexp_of_json : json -> Sexp.t

type call_tool_request = {
  tool : string;  (** The name of the tool to call *)
  arguments : json;  (** Arguments for the tool call *)
}
[@@deriving yojson, compare, sexp]
(** A request to call a single tool with specific arguments *)

type call_tool_request_result = {
  tool : string;  (** The name of the tool that was called *)
  arguments : json;  (** The arguments used for the tool call *)
  is_error : bool; [@key "isError"]
      (** Whether the call resulted in an error *)
  content : json list;  (** The result content as JSON *)
}
[@@deriving yojson, compare, sexp]
(** Result of a bulk tool call, extending call_tool_result with request info *)

val call_tool_request_result_from_call_tool_result :
  result:Mcp.Types.call_tool_result ->
  tool:string ->
  arguments:json ->
  call_tool_request_result
(** Create a call_tool_request_result from a call_tool_result *)

(** {1 Bulk Tool Caller} *)

(** Bulk tool caller module type *)
module type S = sig
  type t

  val create : unit -> t
  (** Create a new bulk tool caller *)

  val call_tools_bulk :
    t ->
    tool_calls:call_tool_request list ->
    ?continue_on_error:bool ->
    call_tool:
      (name:string -> arguments:json -> Mcp.Types.call_tool_result Deferred.t) ->
    unit ->
    call_tool_request_result list Deferred.t
  (** Call multiple tools in a single request *)

  val call_tool_bulk :
    t ->
    tool:string ->
    tool_arguments:json list ->
    ?continue_on_error:bool ->
    call_tool:
      (name:string -> arguments:json -> Mcp.Types.call_tool_result Deferred.t) ->
    unit ->
    call_tool_request_result list Deferred.t
  (** Call a single tool multiple times with different arguments *)
end

module Bulk_tool_caller : S
(** Default implementation of bulk tool caller *)

(** {1 Convenience Functions} *)

val create : unit -> Bulk_tool_caller.t
(** Create a new bulk tool caller *)

val call_tools_bulk :
  Bulk_tool_caller.t ->
  tool_calls:call_tool_request list ->
  ?continue_on_error:bool ->
  call_tool:
    (name:string -> arguments:json -> Mcp.Types.call_tool_result Deferred.t) ->
  unit ->
  call_tool_request_result list Deferred.t
(** Call multiple tools in a single request *)

val call_tool_bulk :
  Bulk_tool_caller.t ->
  tool:string ->
  tool_arguments:json list ->
  ?continue_on_error:bool ->
  call_tool:
    (name:string -> arguments:json -> Mcp.Types.call_tool_result Deferred.t) ->
  unit ->
  call_tool_request_result list Deferred.t
(** Call a single tool multiple times with different arguments *)
