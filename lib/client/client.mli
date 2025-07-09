open Core
open Async

(** MCP client that delegates connection management to a Transport instance *)

exception Tool_error of string
(** Exception raised when a tool call fails *)

exception Closed_resource_error of string
(** Exception raised when server session is closed *)

exception Init_error of string
(** Exception raised when initialization fails *)

type 'transport t
(** Client type parameterized by transport type *)

val create :
  ?roots:Roots.t ->
  ?sampling_handler:Sampling.handler ->
  ?log_handler:Logging.handler ->
  ?message_handler:Messages.handler ->
  ?progress_handler:Progress.handler ->
  ?timeout:Time.Span.t ->
  ?init_timeout:Time.Span.t ->
  ?client_info:Types.implementation ->
  ?auth:string ->
  'transport ->
  'transport t
(** Create a new client *)

val session : 'transport t -> Types.client_session
(** Get the current active session. Raises if not connected *)

val initialize_result : 'transport t -> Types.initialize_result
(** Get the result of the initialization request *)

val set_roots : 'transport t -> Roots.t -> unit
(** Set the roots for the client *)

val set_sampling_callback : 'transport t -> Sampling.handler -> unit
(** Set the sampling callback *)

val is_connected : 'transport t -> bool
(** Check if client is connected *)

val connect : 'transport t -> unit Deferred.t
(** Connect to the server *)

val disconnect : 'transport t -> force:bool -> unit Deferred.t
(** Disconnect from the server *)

val close : 'transport t -> unit Deferred.t
(** Close the client *)

val with_client : 'transport t -> (unit -> 'a Deferred.t) -> 'a Deferred.t
(** Execute a function with a connected client, handling cleanup *)

val with_error_handling :
  'transport t -> (unit -> 'a Deferred.t) -> 'a Deferred.t
(** Execute a function with error handling *)

val ping : 'transport t -> bool Deferred.t
(** Send a ping request *)

val cancel :
  'transport t -> request_id:string -> ?reason:string -> unit -> unit Deferred.t
(** Send a cancellation notification *)

val progress :
  'transport t ->
  progress_token:string ->
  progress:float ->
  ?total:float ->
  ?message:string ->
  unit ->
  unit Deferred.t
(** Send a progress notification *)

val set_logging_level : 'transport t -> Types.logging_level -> unit Deferred.t
(** Set logging level *)

val send_roots_list_changed : 'transport t -> unit Deferred.t
(** Send roots list changed notification *)

val list_resources_mcp : 'transport t -> Types.list_resources_result Deferred.t
(** List resources and return full MCP protocol result *)

val list_resources : 'transport t -> Types.resource list Deferred.t
(** List resources *)

val list_resource_templates_mcp :
  'transport t -> Types.list_resource_templates_result Deferred.t
(** List resource templates and return full MCP protocol result *)

val list_resource_templates :
  'transport t -> Types.resource_template list Deferred.t
(** List resource templates *)

val read_resource_mcp :
  'transport t -> uri:string -> Types.read_resource_result Deferred.t
(** Read resource and return full MCP protocol result *)

val read_resource :
  'transport t ->
  uri:string ->
  (Types.text_resource_contents, Types.blob_resource_contents) Either.t list
  Deferred.t
(** Read resource *)

val list_prompts_mcp : 'transport t -> Types.list_prompts_result Deferred.t
(** List prompts and return full MCP protocol result *)

val list_prompts : 'transport t -> Types.prompt list Deferred.t
(** List prompts *)

val get_prompt_mcp :
  'transport t ->
  name:string ->
  ?arguments:(string * Yojson.Safe.t) list ->
  unit ->
  Types.get_prompt_result Deferred.t
(** Get prompt and return full MCP protocol result *)

val get_prompt :
  'transport t ->
  name:string ->
  ?arguments:(string * Yojson.Safe.t) list ->
  unit ->
  Types.get_prompt_result Deferred.t
(** Get prompt *)

val list_tools_mcp : 'transport t -> Types.list_tools_result Deferred.t
(** List tools and return full MCP protocol result *)

val list_tools : 'transport t -> Types.tool list Deferred.t
(** List tools *)

val call_tool_mcp :
  'transport t ->
  name:string ->
  ?arguments:(string * Yojson.Safe.t) list ->
  ?timeout:Time.Span.t ->
  ?progress_handler:Progress.handler ->
  unit ->
  Types.call_tool_result Deferred.t
(** Call tool and return full MCP protocol result *)

val call_tool :
  'transport t ->
  name:string ->
  ?arguments:(string * Yojson.Safe.t) list ->
  ?timeout:Time.Span.t ->
  ?progress_handler:Progress.handler ->
  unit ->
  Content_block.t list Deferred.t
(** Call tool *)

val complete_mcp :
  'transport t ->
  ref:Types.reference ->
  argument:(string * string) list ->
  Types.complete_result Deferred.t
(** Complete and return full MCP protocol result *)

val complete :
  'transport t ->
  ref:Types.reference ->
  argument:(string * string) list ->
  Types.completion Deferred.t
(** Complete *)

val create_resource_reference : uri:string -> Types.reference
(** Create a resource reference *)

val create_prompt_reference :
  name:string ->
  arguments:(string * Yojson.Safe.t) list option ->
  Types.reference
(** Create a prompt reference *)

val complete_resource :
  'transport t ->
  uri:string ->
  argument:(string * string) list ->
  Types.completion Deferred.t
(** Complete with resource *)

val complete_prompt :
  'transport t ->
  name:string ->
  ?arguments:(string * Yojson.Safe.t) list ->
  argument:(string * string) list ->
  unit ->
  Types.completion Deferred.t
(** Complete with prompt *)

val subscribe_resource : 'transport t -> uri:string -> unit Deferred.t
(** Subscribe to resource updates *)

val unsubscribe_resource : 'transport t -> uri:string -> unit Deferred.t
(** Unsubscribe from resource updates *)
