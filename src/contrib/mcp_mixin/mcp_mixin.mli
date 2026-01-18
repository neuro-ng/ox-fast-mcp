(** MCP Mixin for OxFastMCP

    Provides a mixin pattern for registering class methods as tools, resources,
    and prompts with an OxFastMCP server instance.

    This is the OCaml translation of Python's fastmcp.contrib.mcp_mixin module.
    Since OCaml doesn't have decorators, registration is done via explicit item
    lists rather than attribute-based discovery. *)

open! Core
open! Async

(** {1 JSON Type} *)

type json = Yojson.Safe.t
(** JSON type alias for Yojson.Safe.t *)

val yojson_of_json : json -> Yojson.Safe.t
val json_of_yojson : Yojson.Safe.t -> json
val compare_json : json -> json -> int
val sexp_of_json : json -> Sexp.t

(** {1 Separators} *)

val default_tool_separator : string
(** Default separator for tool names when using prefix *)

val default_resource_separator : string
(** Default separator for resource names/URIs when using prefix *)

val default_prompt_separator : string
(** Default separator for prompt names when using prefix *)

(** {1 Registration Info Types}

    These types store metadata for registered items, similar to what Python's
    decorators capture in setattr. *)

type tool_registration_info = {
  name : string;
  description : string option;
  tags : string list option;
  annotations : (string * json) list option;
  exclude_args : string list option;
  meta : (string * json) list option;
  enabled : bool option;
}
[@@deriving yojson, compare, sexp]
(** Tool registration metadata *)

type resource_registration_info = {
  uri : string;
  name : string;
  title : string option;
  description : string option;
  mime_type : string option;
  tags : string list option;
  annotations : (string * json) list option;
  meta : (string * json) list option;
  enabled : bool option;
}
[@@deriving yojson, compare, sexp]
(** Resource registration metadata *)

type prompt_registration_info = {
  name : string;
  title : string option;
  description : string option;
  tags : string list option;
  meta : (string * json) list option;
  enabled : bool option;
}
[@@deriving yojson, compare, sexp]
(** Prompt registration metadata *)

(** {1 Registered Item Types}

    These bind a handler function with its registration metadata. *)

type registered_tool = {
  handler :
    Tool_types.execution_context ->
    json ->
    Fmcp_types.content_type list Deferred.t;
  info : tool_registration_info;
}
(** Registered tool - handler function paired with registration info *)

type registered_resource = {
  reader :
    unit ->
    ( Resources.Resource_types.content,
      Ox_fast_mcp.Exceptions.error_data )
    Deferred.Result.t;
  info : resource_registration_info;
}
(** Registered resource - reader function paired with registration info *)

type registered_prompt = {
  handler :
    (string * json) list ->
    ( Prompts.Prompt_types.prompt_message list,
      Ox_fast_mcp.Exceptions.error_data )
    Deferred.Result.t;
  info : prompt_registration_info;
}
(** Registered prompt - handler function paired with registration info *)

(** {1 Helper Functions} *)

val apply_prefix : prefix:string option -> separator:string -> string -> string
(** Apply prefix to a name using the specified separator *)

(** {1 Registration Functions} *)

val register_tools :
  tools:registered_tool list ->
  manager:Tool.tool_manager ->
  ?prefix:string ->
  ?separator:string ->
  unit ->
  unit
(** Register all tools from a list with the tool manager *)

val register_resources :
  resources:registered_resource list ->
  manager:Resources.Resource_manager.t ->
  ?prefix:string ->
  ?separator:string ->
  unit ->
  unit
(** Register all resources from a list with the resource manager *)

val register_prompts :
  prompts:registered_prompt list ->
  manager:Prompts.Prompt_manager.t ->
  ?prefix:string ->
  ?separator:string ->
  unit ->
  unit
(** Register all prompts from a list with the prompt manager *)

val register_all :
  tools:registered_tool list ->
  resources:registered_resource list ->
  prompts:registered_prompt list ->
  tool_manager:Tool.tool_manager ->
  resource_manager:Resources.Resource_manager.t ->
  prompt_manager:Prompts.Prompt_manager.t ->
  ?prefix:string ->
  ?tool_separator:string ->
  ?resource_separator:string ->
  ?prompt_separator:string ->
  unit ->
  unit
(** Register all items with their respective managers *)

(** {1 Mixin Module Type} *)

module type S = sig
  type t
  (** The mixin instance type *)

  val get_tools : t -> registered_tool list
  (** Get all registered tools from this mixin *)

  val get_resources : t -> registered_resource list
  (** Get all registered resources from this mixin *)

  val get_prompts : t -> registered_prompt list
  (** Get all registered prompts from this mixin *)

  val register_all :
    t ->
    tool_manager:Tool.tool_manager ->
    resource_manager:Resources.Resource_manager.t ->
    prompt_manager:Prompts.Prompt_manager.t ->
    ?prefix:string ->
    ?tool_separator:string ->
    ?resource_separator:string ->
    ?prompt_separator:string ->
    unit ->
    unit
  (** Register all items with the server managers *)
end

(** {1 Convenience Constructors} *)

val make_tool_info :
  name:string ->
  ?description:string ->
  ?tags:string list ->
  ?annotations:(string * json) list ->
  ?exclude_args:string list ->
  ?meta:(string * json) list ->
  ?enabled:bool ->
  unit ->
  tool_registration_info
(** Create a tool registration info record *)

val make_resource_info :
  uri:string ->
  name:string ->
  ?title:string ->
  ?description:string ->
  ?mime_type:string ->
  ?tags:string list ->
  ?annotations:(string * json) list ->
  ?meta:(string * json) list ->
  ?enabled:bool ->
  unit ->
  resource_registration_info
(** Create a resource registration info record *)

val make_prompt_info :
  name:string ->
  ?title:string ->
  ?description:string ->
  ?tags:string list ->
  ?meta:(string * json) list ->
  ?enabled:bool ->
  unit ->
  prompt_registration_info
(** Create a prompt registration info record *)

(** {1 Bulk Tool Execution}

    Re-exports from Bulk_tool_caller for convenient access to bulk tool calling
    functionality. Use these to execute registered tools in bulk. *)

type call_tool_request = Bulk_tool_caller.call_tool_request = {
  tool : string;
  arguments : json;
}
(** Bulk tool request - tool name and arguments *)

type call_tool_request_result = Bulk_tool_caller.call_tool_request_result = {
  tool : string;
  arguments : json;
  is_error : bool;
  content : json list;
}
(** Bulk tool result - includes request info and result *)

val create_bulk_caller : unit -> Bulk_tool_caller.Bulk_tool_caller.t
(** Create a bulk tool caller instance *)

val call_tools_bulk :
  Bulk_tool_caller.Bulk_tool_caller.t ->
  tool_calls:call_tool_request list ->
  ?continue_on_error:bool ->
  call_tool:
    (name:string -> arguments:json -> Mcp.Types.call_tool_result Deferred.t) ->
  unit ->
  call_tool_request_result list Deferred.t
(** Call multiple different tools in a single batch *)

val call_tool_bulk :
  Bulk_tool_caller.Bulk_tool_caller.t ->
  tool:string ->
  tool_arguments:json list ->
  ?continue_on_error:bool ->
  call_tool:
    (name:string -> arguments:json -> Mcp.Types.call_tool_result Deferred.t) ->
  unit ->
  call_tool_request_result list Deferred.t
(** Call a single tool multiple times with different arguments *)

val call_tool_request_result_from_call_tool_result :
  result:Mcp.Types.call_tool_result ->
  tool:string ->
  arguments:json ->
  call_tool_request_result
(** Convert a call_tool_result to a call_tool_request_result *)
