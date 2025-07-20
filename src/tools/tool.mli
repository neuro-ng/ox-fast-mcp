open Tool_types
open Utilities.Types

type tool_manager = {
  mutable tools : (string, tool) Hashtbl.t;
  mutable duplicate_behavior : [ `Warn | `Error | `Replace | `Ignore ];
  mutable mask_error_details : bool;
}
(** Tool manager for managing multiple tools *)

val create_manager :
  ?duplicate_behavior:[ `Warn | `Error | `Replace | `Ignore ] ->
  ?mask_error_details:bool ->
  unit ->
  tool_manager
(** Create a new tool manager *)

val create_tool :
  name:string ->
  description:string ->
  ?parameters:json ->
  ?enabled:bool ->
  ?tags:string list ->
  ?annotations:(string * json) list option ->
  tool_handler ->
  tool
(** Create a new function tool *)

val from_tool :
  ?name:string ->
  ?description:string ->
  ?tags:string list ->
  ?transform_fn:(execution_context -> json -> content_type list Lwt.t) ->
  ?transform_args:(string * Arg_transform.t) list ->
  ?annotations:(string * json) list option ->
  ?serializer:(content_type list -> string) ->
  ?enabled:bool ->
  tool ->
  transformed
(** Transform a tool with the specified parameters *)

val to_mcp_tool : tool -> json
(** Convert tool to MCP tool definition *)

val validate_schema : json -> bool
(** Validate JSON schema for tool parameters *)

val register_tool : tool_manager -> tool -> unit
(** Register a tool with the manager *)

val remove_tool : tool_manager -> string -> unit
(** Remove a tool from the manager *)

val get_tool : tool_manager -> string -> tool option
(** Get a tool by name *)

val get_all_tools : tool_manager -> tool list
(** Get all tools *)

val get_enabled_tools : tool_manager -> tool list
(** Get enabled tools only *)

val filter_tools_by_tags : tool_manager -> string list -> tool list
(** Filter tools by tags *)

val tool_count : tool_manager -> int
(** Get tool count *)

val execute_tool :
  tool_manager -> string -> execution_context -> json -> content_type list Lwt.t
(** Execute a tool *)

val enable_tool : tool_manager -> string -> bool
(** Enable a tool *)

val disable_tool : tool_manager -> string -> bool
(** Disable a tool *)

val is_tool_enabled : tool_manager -> string -> bool
(** Check if a tool is enabled *)

val update_tool_tags : tool_manager -> string -> string list -> bool
(** Update tool tags *)

val add_tool_tags : tool_manager -> string -> string list -> bool
(** Add tags to a tool *)

val remove_tool_tags : tool_manager -> string -> string list -> bool
(** Remove tags from a tool *)

val create_calculator_tool : unit -> tool
(** Helper: Create a simple calculator tool for testing *)

val create_text_processor_tool : unit -> tool
(** Helper: Create a text processing tool *)

val clear_tools : tool_manager -> unit
(** Clear all tools from manager *)

type tool_stats = {
  total_tools : int;
  enabled_tools : int;
  disabled_tools : int;
  tags_used : string list;
}
(** Tool statistics *)

val get_tool_stats : tool_manager -> tool_stats
(** Get tool statistics *)

val add_transformed_tool : tool_manager -> transformed -> unit
(** Add transformed tool to manager *)
