open Async
open Tools.Tool_types
open Utilities.Types

type capabilities = {
  tools : bool;
  resources : bool;
  prompts : bool;
}
(** Server capabilities *)

type tool_list = function_tool list
(** List of tools *)

type resource_list = string list
(** List of resources *)

type resource_template_list = string list
(** List of resource templates *)

type tool_response = content_type list
(** Tool execution response *)

type prompt_list = string list
(** List of prompts *)

type prompt_response = content_type list
(** Prompt execution response *)

type resource_response = content_type list
(** Resource read response *)

type t = {
  name : string;
  instructions : string option;
  mutable tool_count : int;
  mutable resource_count : int;
  mutable prompt_count : int;
  tools : function_tool list;
  resources : string list;
  resource_templates : string list;
  prompts : string list;
}
(** FastMCP server state *)

(** Create a new FastMCP server *)
val create : ?name:string -> ?instructions:string option -> unit -> t

(** Get server name *)
val get_name : t -> string

(** Get server instructions *)
val get_instructions : t -> string option

(** Get tool count *)
val tool_count : t -> int

(** Get resource count *)
val resource_count : t -> int

(** Get prompt count *)
val prompt_count : t -> int

(** Register a tool *)
val register_tool : t -> name:string -> description:string -> func:('a -> 'b) -> unit

(** Register a tool with context *)
val register_tool_with_context : t -> name:string -> description:string -> func:('a -> 'b) -> unit

(** Register a resource *)
val register_resource : t -> uri:Uri.t -> name:string -> description:string -> func:(unit -> 'a) -> unit

(** Register a resource template *)
val register_resource_template : t -> uri_pattern:string -> name:string -> description:string -> func:(user_id:string -> 'a) -> unit

(** Register a prompt *)
val register_prompt : t -> name:string -> description:string -> func:('a -> string Deferred.t) -> unit

(** List all tools *)
val list_tools : t -> function_tool list Deferred.t

(** List all resources *)
val list_resources : t -> string list Deferred.t

(** List all prompts *)
val list_prompts : t -> string list Deferred.t

(** List all resource templates *)
val list_resource_templates : t -> string list Deferred.t

(** Get server capabilities *)
val get_capabilities : t -> capabilities

(** Add a tool to the server *)
val add_tool : t -> function_tool -> t

(** Add a resource to the server *)
val add_resource : t -> string -> t

(** Add a resource template to the server *)
val add_resource_template : t -> string -> t

(** Add a prompt to the server *)
val add_prompt : t -> string -> t

(** List all tools in MCP format *)
val list_tools_mcp : t -> tool_list Deferred.t

(** List all resources in MCP format *)
val list_resources_mcp : t -> resource_list Deferred.t

(** List all resource templates in MCP format *)
val list_resource_templates_mcp : t -> resource_template_list Deferred.t

(** Call a tool with the given name and arguments *)
val call_tool_mcp : t -> name:string -> arguments:'a -> unit -> tool_response Deferred.t

(** List all prompts in MCP format *)
val list_prompts_mcp : t -> prompt_list Deferred.t

(** Get a prompt with the given name and arguments *)
val get_prompt : t -> name:string -> arguments:'a -> unit -> prompt_response Deferred.t

(** Read a resource from the given URI *)
val read_resource : t -> Uri.t -> resource_response Deferred.t

(** Get all tools *)
val get_tools : t -> function_tool list

(** Get a tool by name *)
val get_tool : t -> string -> function_tool option 