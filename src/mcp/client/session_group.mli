(** Session Group - Manages multiple concurrent MCP sessions

    This module provides functionality to connect to multiple MCP servers and
    aggregate their tools, resources, and prompts into unified dictionaries.

    MVP implementation supports stdio transport only. *)

open Core
open Async

(** {1 Server Parameters} *)

module Server_parameters : sig
  type t = Mcp_client_transports.Stdio.stdio_server_parameters
  [@@deriving sexp_of]
  (** Parameters for stdio server transport *)

  val create :
    command:string ->
    ?args:string list ->
    ?env:(string * string) list ->
    ?cwd:string ->
    ?encoding:string ->
    ?encoding_error_handler:Mcp_client_transports.Stdio.encoding_error_handler ->
    unit ->
    t
  (** Create stdio server parameters *)
end

(** {1 Session Parameters} *)

module Client_session_parameters : sig
  type t = {
    read_timeout_seconds : Time_ns.Span.t option;
    sampling_callback : Session.sampling_fn option;
    elicitation_callback : Session.elicitation_fn option;
    list_roots_callback : Session.list_roots_fn option;
    logging_callback : Session.logging_fn option;
    message_handler : Session.message_handler option;
    client_info : Mcp.Types.implementation option;
  }
  [@@deriving fields]
  (** Parameters for establishing a client session *)

  val create :
    ?read_timeout_seconds:Time_ns.Span.t ->
    ?sampling_callback:Session.sampling_fn ->
    ?elicitation_callback:Session.elicitation_fn ->
    ?list_roots_callback:Session.list_roots_fn ->
    ?logging_callback:Session.logging_fn ->
    ?message_handler:Session.message_handler ->
    ?client_info:Mcp.Types.implementation ->
    unit ->
    t
  (** Create session parameters with optional callbacks *)

  val default : t
  (** Default session parameters with all callbacks as None *)
end

(** {1 Main Type} *)

type t
(** Session group managing multiple server connections *)

type component_name_hook = string -> Mcp.Types.implementation -> string
(** Hook for customizing component names to avoid collisions. Takes
    (component_name, server_info) and returns modified name. *)

(** {1 Creation} *)

val create : ?component_name_hook:component_name_hook -> unit -> t
(** Create a new session group.

    @param component_name_hook
      Optional function to customize component names to prevent collisions
      across servers *)

(** {1 Connection Management} *)

val connect_to_server :
  t ->
  server_params:Server_parameters.t ->
  ?session_params:Client_session_parameters.t ->
  stderr:Writer.t ->
  unit ->
  Session.t Deferred.t
(** Connect to an MCP server and aggregate its components.

    @param server_params Stdio server spawn parameters
    @param session_params Optional session configuration
    @param stderr Writer for server stderr output
    @return The initialized client session
    @raise Mcp_error if duplicate component names detected *)

val disconnect_from_server : t -> session:Session.t -> unit Deferred.t
(** Disconnect from a server and remove its components.

    @param session The session to disconnect
    @raise Mcp_error if session is not managed by this group *)

(** {1 Component Access} *)

val prompts : t -> Mcp.Types.prompt String.Map.t
(** Get aggregated prompts from all connected servers *)

val resources : t -> Mcp.Types.resource String.Map.t
(** Get aggregated resources from all connected servers *)

val tools : t -> Mcp.Types.tool String.Map.t
(** Get aggregated tools from all connected servers *)

val sessions : t -> Session.t list
(** Get list of all managed sessions *)

(** {1 Tool Calling} *)

val call_tool :
  t ->
  name:string ->
  arguments:Yojson.Safe.t ->
  ?read_timeout:int ->
  ?progress_callback:Session.progress_fn ->
  unit ->
  Mcp.Types.call_tool_result Deferred.t
(** Call a tool by its aggregated name.

    The tool name must exist in the tools map. The call is routed to the
    appropriate session that provides this tool.

    @param name Aggregated tool name (possibly modified by component_name_hook)
    @param arguments Tool arguments as JSON
    @raise Not_found_s if tool name not found
    @raise Mcp_error if underlying tool call fails *)
