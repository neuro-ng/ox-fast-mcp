open Tool_types
open Utilities.Types

(** Tool manager for managing multiple tools *)
type tool_manager = {
  mutable tools : (string, tool) Hashtbl.t;
  mutable duplicate_behavior : [ `Warn | `Error | `Replace | `Ignore ];
  mutable mask_error_details : bool;
}

(** Create a new tool manager *)
val create_manager : 
  ?duplicate_behavior:[ `Warn | `Error | `Replace | `Ignore ] ->
  ?mask_error_details:bool ->
  unit ->
  tool_manager

(** Create a new function tool *)
val create_tool :
  name:string ->
  description:string ->
  ?parameters:json ->
  ?enabled:bool ->
  ?tags:string list ->
  ?annotations:(string * json) list option ->
  tool_handler ->
  tool

(** Transform a tool with the specified parameters *)
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

(** Convert tool to MCP tool definition *)
val to_mcp_tool : tool -> json

(** Validate JSON schema for tool parameters *)
val validate_schema : json -> bool

(** Register a tool with the manager *)
val register_tool : tool_manager -> tool -> unit

(** Remove a tool from the manager *)
val remove_tool : tool_manager -> string -> unit

(** Get a tool by name *)
val get_tool : tool_manager -> string -> tool option

(** Get all tools *)
val get_all_tools : tool_manager -> tool list

(** Get enabled tools only *)
val get_enabled_tools : tool_manager -> tool list

(** Filter tools by tags *)
val filter_tools_by_tags : tool_manager -> string list -> tool list

(** Get tool count *)
val tool_count : tool_manager -> int

(** Execute a tool *)
val execute_tool : tool_manager -> string -> execution_context -> json -> content_type list Lwt.t

(** Enable a tool *)
val enable_tool : tool_manager -> string -> bool

(** Disable a tool *)
val disable_tool : tool_manager -> string -> bool

(** Check if a tool is enabled *)
val is_tool_enabled : tool_manager -> string -> bool

(** Update tool tags *)
val update_tool_tags : tool_manager -> string -> string list -> bool

(** Add tags to a tool *)
val add_tool_tags : tool_manager -> string -> string list -> bool

(** Remove tags from a tool *)
val remove_tool_tags : tool_manager -> string -> string list -> bool

(** Helper: Create a simple calculator tool for testing *)
val create_calculator_tool : unit -> tool

(** Helper: Create a text processing tool *)
val create_text_processor_tool : unit -> tool

(** Clear all tools from manager *)
val clear_tools : tool_manager -> unit

(** Tool statistics *)
type tool_stats = {
  total_tools : int;
  enabled_tools : int;
  disabled_tools : int;
  tags_used : string list;
}

(** Get tool statistics *)
val get_tool_stats : tool_manager -> tool_stats

(** Add transformed tool to manager *)
val add_transformed_tool : tool_manager -> transformed -> unit 