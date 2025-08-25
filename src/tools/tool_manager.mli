open Core
open Async
open! Tool_types

(** Tool manager module for FastMCP.

    This module provides functionality for managing tools, including:
    - Tool registration and removal
    - Tool discovery and lookup
    - Tool execution with proper error handling
    - Server mounting for distributed tools

    @raise Not_found_error when a requested tool cannot be found
    @raise Tool_error when a tool execution fails *)

module DuplicateBehavior : sig
  type t = Warn | Replace | Error | Ignore
  [@@deriving sexp, compare, equal, enumerate]

  val of_string : string -> t Or_error.t
  val of_string_exn : string -> t
end

module Tool : sig
  type t = {
    key : string;
    name : string option;
    description : string option;
    tags : string list;
    annotations : (string * string) list;
    parameters : Yojson.Safe.t;
    enabled : bool;
    fn : Tool_types.tool_handler;
  }

  val with_key : t -> string -> t
  val enable : t -> t
  val disable : t -> t

  val to_mcp_tool :
    ?overrides:(string * Yojson.Safe.t) list -> t -> Yojson.Safe.t

  val default_serializer : Yojson.Safe.t -> string

  val convert_to_content :
    ?serializer:(Yojson.Safe.t -> string) ->
    Yojson.Safe.t ->
    Fmcp_types.content_type list

  val from_function :
    ?name:string ->
    ?description:string ->
    ?tags:string list ->
    ?annotations:(string * string) list ->
    ?_exclude_args:string list ->
    ?_serializer:(Yojson.Safe.t -> string) ->
    ?enabled:bool ->
    (Yojson.Safe.t -> Fmcp_types.content_type list Deferred.t) ->
    t
end

type t

type mounted_server = {
  prefix : string option;
      (* server field removed - Server module not available *)
}

val create :
  ?duplicate_behavior:DuplicateBehavior.t ->
  ?mask_error_details:bool ->
  unit ->
  t
(** Create a new tool manager.
    @param duplicate_behavior How to handle duplicate tool registrations
    @param mask_error_details Whether to mask internal error details *)

val mount : t -> _server:unit -> prefix:string option -> unit
(** Mount a server as a source of tools. Tools from mounted servers are
    available with their prefix (if any). *)

val has_tool : t -> string -> bool Deferred.t
(** Check if a tool exists. This includes tools from mounted servers. *)

val get_tool : t -> string -> Tool.t Deferred.t
(** Get a tool by key.
    @raise Not_found_error if the tool is not found *)

val get_tools : t -> Tool.t String.Map.t Deferred.t
(** Get all tools, including from mounted servers. Returns the complete,
    unfiltered inventory. *)

val list_tools : t -> Tool.t list Deferred.t
(** List all tools, applying protocol filtering. This is used for the
    server-to-server protocol. *)

val add_tool_from_fn :
  t ->
  (Yojson.Safe.t -> Fmcp_types.content_type list Deferred.t) ->
  ?name:string ->
  ?description:string ->
  ?tags:string list ->
  ?annotations:(string * string) list ->
  ?exclude_args:string list ->
  unit ->
  Tool.t
(** [DEPRECATED since 2.7.0] Add a tool from a function. Use
    Tool.from_function() and call add_tool() instead. *)

val add_tool : t -> Tool.t -> Tool.t
(** Add a tool to the manager.
    @raise Tool_error if the tool already exists and duplicate_behavior is Error *)

val remove_tool : t -> string -> unit
(** Remove a tool from the manager.
    @raise Not_found_error if the tool is not found *)

val call_tool :
  t -> string -> Yojson.Safe.t -> Fmcp_types.content_type list Deferred.t
(** Call a tool with arguments.
    @raise Not_found_error if the tool is not found
    @raise Tool_error if the tool execution fails *)
