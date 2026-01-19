(** OxFastMCP Server Module

    The main server module providing the OxFastMCP server implementation. This
    is a more ergonomic interface for MCP servers with tool, resource, and
    prompt management, middleware support, and transport handling. *)

open! Core
open! Async

(** {1 Types} *)

(** Transport protocols for server communication *)
module Transport : sig
  type t = Stdio | Http | Sse | Streamable_http
  [@@deriving sexp, compare, equal, enumerate]

  val to_string : t -> string
  val of_string : string -> t
end

(** Behavior when encountering duplicate components *)
module Duplicate_behavior : sig
  type t = Warn | Error | Replace | Ignore
  [@@deriving sexp, compare, equal, enumerate]

  val to_string : t -> string
  val of_string : string -> t
end

(** Resource prefix format for mounted servers *)
module Resource_prefix_format : sig
  type t = Protocol | Path [@@deriving sexp, compare, equal, enumerate]

  val to_string : t -> string
  val of_string : string -> t
end

(** {1 Component Types} *)

(** Tool representation *)
module Tool : sig
  type t = {
    name : string;
    key : string;
    description : string option;
    parameters : Yojson.Safe.t;
    annotations : Yojson.Safe.t option;
    output_schema : Yojson.Safe.t option;
    meta : Yojson.Safe.t option;
    tags : String.Set.t;
    handler : Yojson.Safe.t -> Yojson.Safe.t Deferred.t;
  }

  val create :
    name:string ->
    ?description:string ->
    ?parameters:Yojson.Safe.t ->
    ?annotations:Yojson.Safe.t ->
    ?output_schema:Yojson.Safe.t ->
    ?meta:Yojson.Safe.t ->
    ?tags:String.Set.t ->
    handler:(Yojson.Safe.t -> Yojson.Safe.t Deferred.t) ->
    unit ->
    t

  val to_mcp_tool : ?include_fastmcp_meta:bool -> t -> Yojson.Safe.t
end

(** Resource representation *)
module Resource : sig
  type t = {
    uri : string;
    key : string;
    name : string;
    description : string option;
    mime_type : string;
    meta : Yojson.Safe.t option;
    tags : String.Set.t;
    reader : unit -> string Deferred.t;
  }

  val create :
    uri:string ->
    name:string ->
    ?description:string ->
    ?mime_type:string ->
    ?meta:Yojson.Safe.t ->
    ?tags:String.Set.t ->
    reader:(unit -> string Deferred.t) ->
    unit ->
    t

  val to_mcp_resource : ?include_fastmcp_meta:bool -> t -> Yojson.Safe.t
end

(** Resource template representation *)
module Resource_template : sig
  type t = {
    uri_template : string;
    key : string;
    name : string;
    description : string option;
    mime_type : string;
    parameters : Yojson.Safe.t;
    meta : Yojson.Safe.t option;
    tags : String.Set.t;
    create_resource : params:(string * string) list -> Resource.t Deferred.t;
  }

  val create :
    uri_template:string ->
    name:string ->
    ?description:string ->
    ?mime_type:string ->
    ?parameters:Yojson.Safe.t ->
    ?meta:Yojson.Safe.t ->
    ?tags:String.Set.t ->
    create_resource:(params:(string * string) list -> Resource.t Deferred.t) ->
    unit ->
    t
end

(** Prompt representation *)
module Prompt : sig
  type argument = {
    name : string;
    description : string option;
    required : bool;
  }

  type t = {
    name : string;
    key : string;
    description : string option;
    arguments : argument list;
    meta : Yojson.Safe.t option;
    tags : String.Set.t;
    render : Yojson.Safe.t -> Yojson.Safe.t Deferred.t;
  }

  val create :
    name:string ->
    ?description:string ->
    ?arguments:argument list ->
    ?meta:Yojson.Safe.t ->
    ?tags:String.Set.t ->
    render:(Yojson.Safe.t -> Yojson.Safe.t Deferred.t) ->
    unit ->
    t
end

(** {1 Middleware} *)

module Middleware : sig
  type context = {
    message : Yojson.Safe.t;
    source : string;
    type_ : string;
    method_ : string;
  }

  type next = context -> Yojson.Safe.t Deferred.t
  type t = context -> next:next -> Yojson.Safe.t Deferred.t

  val identity : t
  val compose : t list -> t
end

(** {1 Protocol Handlers} *)

module Protocol : sig
  type handler = Context.t -> Yojson.Safe.t Deferred.t
  type method_map = (string, handler) Hashtbl.t

  val create_method_map : unit -> method_map
end

(** {1 Lifespan Management} *)

(** Context passed to lifespan hooks *)
type lifespan_context = {
  mutable startup_complete : bool;
  mutable shutdown_requested : bool;
}

(** {1 Main Server} *)

module Ox_fast_mcp : sig
  type t
  
  (** Lifespan hook function type *)
  type lifespan_hook = lifespan_context -> unit Deferred.t

  val generate_name : unit -> string

  (** {2 Validation Helpers} *)

  val validate_tool_name : string -> (unit, string) Result.t
  (** Validate a tool name. Returns Ok() if valid, or Error with a helpful
      message. *)

  val validate_resource_uri : string -> (unit, string) Result.t
  (** Validate a resource URI. Returns Ok() if valid, or Error with a helpful
      message. *)

  val validate_prompt_name : string -> (unit, string) Result.t
  (** Validate a prompt name. Returns Ok() if valid, or Error with a helpful
      message. *)

  val validate_template_uri : string -> (unit, string) Result.t
  (** Validate a template URI. Returns Ok() if valid, or Error with a helpful
      message. *)

  (** {2 Name Normalization} *)

  val normalize_tool_name : string -> string
  (** Normalize a tool name to valid format. Converts to lowercase, replaces
      spaces and invalid chars with underscores, ensures starts with letter or
      underscore. *)

  (** {2 Boolean Validation Helpers} *)

  val is_valid_tool_name : string -> bool
  (** Check if a tool name is valid *)

  val is_valid_uri : string -> bool
  (** Check if a URI is valid *)

  val is_valid_prompt_name : string -> bool
  (** Check if a prompt name is valid *)

  val is_valid_template_uri : string -> bool
  (** Check if a template URI is valid *)

  val create :
    ?name:string ->
    ?version:string ->
    ?instructions:string ->
    ?website_url:string ->
    ?icons:Yojson.Safe.t list ->
    ?resource_prefix_format:Resource_prefix_format.t ->
    ?include_tags:String.Set.t ->
    ?exclude_tags:String.Set.t ->
    ?strict_input_validation:bool ->
    ?include_fastmcp_meta:bool ->
    ?middleware:Middleware.t list ->
    ?on_duplicate_tools:Duplicate_behavior.t ->
    ?on_duplicate_resources:Duplicate_behavior.t ->
    ?on_duplicate_prompts:Duplicate_behavior.t ->
    ?tools:Tool.t list ->
    ?resources:Resource.t list ->
    ?prompts:Prompt.t list ->
    unit ->
    t

  val name : t -> string
  (** Accessors *)

  val version : t -> string option
  val instructions : t -> string option
  val website_url : t -> string option
  val icons : t -> Yojson.Safe.t list

  val set_instructions : t -> string -> unit
  (** Set server instructions (mutable) **)

  val server_info : t -> Yojson.Safe.t
  (** Get server info for MCP protocol **)

  val capabilities : t -> Yojson.Safe.t
  (** Get server capabilities for MCP protocol **)

  val get_stats : t -> Yojson.Safe.t
  (** Get server statistics (counts of tools, resources, prompts, templates,
      mounted servers) **)

  (** {2 Component Manager Accessors} *)

  val get_tool_manager : t -> Tool_manager.t
  (** Get a Tool_manager instance backed by server's tool storage.
      Note: Creates a new manager that mirrors current server tools. *)

  val get_prompt_manager : t -> Prompts.Prompt_manager.t
  (** Get a Prompt_manager instance backed by server's prompt storage.
      Note: Creates a new manager that mirrors current server prompts. *)

  val get_resource_manager : t -> Resources.Resource_manager.t
  (** Get a Resource_manager instance backed by server's resource storage.
      Note: Creates a new manager that mirrors current server resources. *)

  val get_server_tool_adapter : t -> Tool.t Server_tool_adapter.t
  (** Get a Server_tool_adapter instance backed by server's tool storage.
      This provides a manager-like interface over the server's actual tools. *)

  val add_tool : t -> Tool.t -> unit
  (** Tool management *)

  val remove_tool : t -> name:string -> unit
  
  (** {2 Tool Transformation Management} *)

  val add_tool_transformation : t -> name:string -> config:Tool_transform_config.t -> unit
  (** Add a tool transformation configuration *)

  val remove_tool_transformation : t -> name:string -> unit
  (** Remove a tool transformation configuration *)

  val apply_tool_transformations : t -> Tool.t -> Tool.t
  (** Apply all transformations to a tool if any exist *)
  
  val get_tools : t -> (string, Tool.t) Hashtbl.t
  val get_tool : t -> key:string -> Tool.t Deferred.t
  val list_tools_mcp : t -> Yojson.Safe.t list

  val add_resource : t -> Resource.t -> unit
  (** Resource management *)

  val get_resources : t -> (string, Resource.t) Hashtbl.t
  val get_resource : t -> key:string -> Resource.t Deferred.t
  val list_resources_mcp : t -> Yojson.Safe.t list
  val remove_resource : t -> uri:string -> unit

  val add_template : t -> Resource_template.t -> unit
  (** Template management *)

  val get_templates : t -> (string, Resource_template.t) Hashtbl.t
  val get_template : t -> key:string -> Resource_template.t Deferred.t
  val list_templates_mcp : t -> Yojson.Safe.t list
  val remove_template : t -> uri_template:string -> unit

  val add_prompt : t -> Prompt.t -> unit
  (** Prompt management *)

  val get_prompts : t -> (string, Prompt.t) Hashtbl.t
  val get_prompt_component : t -> key:string -> Prompt.t Deferred.t
  val list_prompts_mcp : t -> Yojson.Safe.t list
  val remove_prompt : t -> name:string -> unit

  val add_simple_resource :
    ?description:string ->
    ?mime_type:string ->
    ?meta:Yojson.Safe.t ->
    ?tags:String.Set.t ->
    uri:string ->
    name:string ->
    reader:(unit -> string Deferred.t) ->
    t ->
    unit
  (** Simple helper to add a resource with just uri, name, and handler *)

  val add_simple_tool :
    ?description:string ->
    ?parameters:Yojson.Safe.t ->
    ?annotations:Yojson.Safe.t ->
    ?output_schema:Yojson.Safe.t ->
    ?meta:Yojson.Safe.t ->
    ?tags:String.Set.t ->
    name:string ->
    handler:(Yojson.Safe.t -> Yojson.Safe.t Deferred.t) ->
    t ->
    unit
  (** Simple helper to add a tool with just name and handler *)

  val add_simple_prompt :
    ?description:string ->
    ?arguments:Prompt.argument list ->
    ?meta:Yojson.Safe.t ->
    ?tags:String.Set.t ->
    name:string ->
    render:(Yojson.Safe.t -> Yojson.Safe.t Deferred.t) ->
    t ->
    unit
  (** Simple helper to add a prompt with name and render function *)

  (** {2 Batch Operation Helpers} *)

  val add_tools : t -> Tool.t list -> unit
  (** Add multiple tools at once *)

  val add_resources : t -> Resource.t list -> unit
  (** Add multiple resources at once *)

  val add_prompts : t -> Prompt.t list -> unit
  (** Add multiple prompts at once *)

  val add_templates : t -> Resource_template.t list -> unit
  (** Add multiple templates at once **)

  (** {2 Server Inspection & Discovery} *)

  val describe_server : t -> Yojson.Safe.t
  (** Get comprehensive server description including all components and
      configuration *)

  val find_tools_by_tag : t -> tag:string -> Tool.t list
  (** Find all tools with a specific tag *)

  val find_resources_by_scheme : t -> scheme:string -> Resource.t list
  (** Find resources by URI scheme (e.g., "file", "http") *)

  val find_prompts_by_tag : t -> tag:string -> Prompt.t list
  (** Find prompts with a specific tag *)

  val suggest_similar_names : string -> string list -> string list
  (** Suggest similar component names using edit distance. Returns up to 5
      suggestions. *)

  val validate_server : t -> (unit, string list) Result.t
  (** Validate server configuration. Returns Ok() if valid, or Error with list
      of issues. *)

  (** {2 Statistics & Debug Utilities} *)

  val list_all_component_names : t -> Yojson.Safe.t
  (** Get all component names organized by type *)

  val component_count_by_tag : t -> (string * int) list
  (** Count components grouped by tag, sorted by count descending *)

  val get_tool_stats : t -> (string * int) list
  (** Get tool call statistics, sorted by call count descending *)

  val get_resource_stats : t -> (string * int) list
  (** Get resource access statistics, sorted by access count descending *)

  val reset_stats : t -> unit
  (** Reset all statistics counters *)

  val health_check : t -> (Yojson.Safe.t, string) Result.t
  (** Comprehensive health check returning server status and validation results *)

  (** {2 Lifespan Hook Management} *)

  val add_startup_hook : t -> lifespan_hook -> unit
  (** Add a startup hook - runs when server starts *)

  val add_shutdown_hook : t -> lifespan_hook -> unit
  (** Add a shutdown hook - runs when server stops *)

  val run_startup : t -> unit Deferred.t
  (** Run all startup hooks in order *)

  val run_shutdown : t -> unit Deferred.t
  (** Run all shutdown hooks in reverse order *)

  val with_lifespan : t -> f:(unit -> 'a Deferred.t) -> 'a Deferred.t
  (** Run a function within the server lifespan - runs startup, executes f, then shutdown *)

  val call_tool :
    t -> name:string -> arguments:Yojson.Safe.t -> Yojson.Safe.t Deferred.t
  (** Operations *)

  val read_resource : t -> uri:string -> string Deferred.t

  val get_prompt :
    t -> name:string -> arguments:Yojson.Safe.t -> Yojson.Safe.t Deferred.t

  val add_middleware : t -> Middleware.t -> unit
  (** Middleware *)

  val import_server :
    t ->
    server:t ->
    ?prefix:string ->
    ?resource_prefix_format:Resource_prefix_format.t ->
    unit ->
    unit
  (** Server mounting - import tools/resources/prompts from another server *)

  (** {1 Protocol Handlers} *)

  val setup_handlers : t -> Protocol.method_map
  (** Setup protocol handler mapping for this server *)

  val handle_stdio_message :
    t -> Protocol.method_map -> Yojson.Safe.t -> Yojson.Safe.t Deferred.t
  (** Handle a single JSON-RPC message and return response *)

  (** {1 Transport} *)

  val run_async :
    t ->
    ?transport:Transport.t ->
    ?host:string ->
    ?port:int ->
    ?log_level:string ->
    unit ->
    unit Deferred.t
  (** Running the server *)
end

(** {1 Helper Functions} *)

val add_resource_prefix :
  uri:string -> prefix:string -> format:Resource_prefix_format.t -> string

val has_resource_prefix :
  uri:string -> prefix:string -> format:Resource_prefix_format.t -> bool

val remove_resource_prefix :
  uri:string -> prefix:string -> format:Resource_prefix_format.t -> string
