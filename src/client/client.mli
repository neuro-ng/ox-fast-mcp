open Core
open Async

(** MCP client that delegates connection management to a Transport instance *)

module Types = Mcp.Types
(** Module type aliases for MCP types **)

(** Roots configuration *)
module Roots : sig
  include module type of Roots

  type t = [ `Static of roots_list list | `Handler of roots_handler ]
end

(** Sampling handler *)
module Sampling : sig
  include module type of Sampling

  type handler = sampling_handler
end

(** Logging handler *)
module Logging : sig
  include module type of Logging

  type handler = log_handler
end

(** Message handler *)
module Messages : sig
  include module type of Messages

  type handler = t
end

module Progress : module type of Progress
(** Progress handler *)

(** Content block *)

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
  ?message_handler:Mcp_client.Session.message_handler ->
  ?progress_handler:Progress.handler ->
  ?timeout:Time_ns.Span.t ->
  ?init_timeout:Time_ns.Span.t ->
  ?client_info:Types.implementation ->
  ?auth:string ->
  Transports.t ->
  Transports.t t
(** Create a new client *)

val session : Transports.t t -> Mcp_client.Session.t
(** Get the current active session. Raises if not connected *)

val initialize_result : Transports.t t -> Types.initialize_result
(** Get the result of the initialization request *)

val set_roots : Transports.t t -> Roots.t -> unit
(** Set the roots for the client *)

val set_sampling_callback : Transports.t t -> Sampling.handler -> unit
(** Set the sampling callback *)

val is_connected : Transports.t t -> bool
(** Check if client is connected *)

val connect : Transports.t t -> unit Deferred.t
(** Connect to the server *)

val disconnect : ?force:bool -> Transports.t t -> unit Deferred.t
(** Disconnect from the server *)

val close : Transports.t t -> unit Deferred.t
(** Close the client *)

val with_client : Transports.t t -> (unit -> 'a Deferred.t) -> 'a Deferred.t
(** Execute a function with a connected client, handling cleanup *)

val with_error_handling :
  Transports.t t -> (unit -> 'a Deferred.t) -> 'a Deferred.t
(** Execute a function with error handling *)

val ping : Transports.t t -> bool Deferred.t
(** Send a ping request *)

val cancel :
  Transports.t t ->
  request_id:Types.request_id ->
  ?reason:string ->
  unit ->
  unit Deferred.t
(** Send a cancellation notification *)

val progress :
  Transports.t t ->
  progress_token:Types.progress_token ->
  progress:float ->
  ?total:float ->
  ?message:string ->
  unit ->
  unit Deferred.t
(** Send a progress notification *)

val set_logging_level : Transports.t t -> Types.logging_level -> unit Deferred.t
(** Set logging level *)

val send_roots_list_changed : Transports.t t -> unit Deferred.t
(** Send roots list changed notification *)

val list_resources_mcp :
  Transports.t t -> Types.list_resources_result Deferred.t
(** List resources and return full MCP protocol result *)

val list_resources : Transports.t t -> Types.resource list Deferred.t
(** List resources *)

val list_resource_templates_mcp :
  Transports.t t -> Types.list_resource_templates_result Deferred.t
(** List resource templates and return full MCP protocol result *)

val list_resource_templates :
  Transports.t t -> Types.resource_template list Deferred.t
(** List resource templates *)

val read_resource_mcp :
  Transports.t t -> uri:string -> Types.read_resource_result Deferred.t
(** Read resource and return full MCP protocol result *)

val read_resource :
  Transports.t t ->
  uri:string ->
  [ `Text of Types.text_resource_contents
  | `Blob of Types.blob_resource_contents ]
  list
  Deferred.t
(** Read resource *)

val list_prompts_mcp : Transports.t t -> Types.list_prompts_result Deferred.t
(** List prompts and return full MCP protocol result *)

val list_prompts : Transports.t t -> Types.prompt list Deferred.t
(** List prompts *)

val get_prompt_mcp :
  Transports.t t ->
  name:string ->
  ?arguments:(string * Yojson.Safe.t) list ->
  unit ->
  Types.get_prompt_result Deferred.t
(** Get prompt and return full MCP protocol result *)

val get_prompt :
  Transports.t t ->
  name:string ->
  ?arguments:(string * Yojson.Safe.t) list ->
  unit ->
  Types.get_prompt_result Deferred.t
(** Get prompt *)

val list_tools_mcp : Transports.t t -> Types.list_tools_result Deferred.t
(** List tools and return full MCP protocol result *)

val list_tools : Transports.t t -> Types.tool list Deferred.t
(** List tools *)

val call_tool_mcp :
  Transports.t t ->
  name:string ->
  ?arguments:(string * Yojson.Safe.t) list ->
  ?timeout:Time_ns.Span.t ->
  ?progress_handler:Progress.handler ->
  unit ->
  Types.call_tool_result Deferred.t
(** Call tool and return full MCP protocol result *)

val call_tool :
  Transports.t t ->
  name:string ->
  ?arguments:(string * Yojson.Safe.t) list ->
  ?timeout:Time_ns.Span.t ->
  ?progress_handler:Progress.handler ->
  unit ->
  Types.content_block list Deferred.t
(** Call tool *)

type reference =
  [ `Resource of Types.resource_template_reference
  | `Prompt of Types.prompt_reference ]

val complete_mcp :
  Transports.t t ->
  ref:reference ->
  argument:(string * string) list ->
  Types.complete_result Deferred.t
(** Complete and return full MCP protocol result *)

val complete :
  Transports.t t ->
  ref:reference ->
  argument:(string * string) list ->
  Types.completion Deferred.t
(** Complete *)

val create_resource_reference : uri:string -> reference
(** Create a resource reference *)

val create_prompt_reference :
  name:string -> arguments:(string * Yojson.Safe.t) list option -> reference
(** Create a prompt reference *)

val complete_resource :
  Transports.t t ->
  uri:string ->
  argument:(string * string) list ->
  Types.completion Deferred.t
(** Complete with resource *)

val complete_prompt :
  Transports.t t ->
  name:string ->
  ?arguments:(string * Yojson.Safe.t) list ->
  argument:(string * string) list ->
  unit ->
  Types.completion Deferred.t
(** Complete with prompt *)

val subscribe_resource : Transports.t t -> uri:string -> unit Deferred.t
(** Subscribe to resource updates *)

val unsubscribe_resource : Transports.t t -> uri:string -> unit Deferred.t
(** Unsubscribe from resource updates *)
