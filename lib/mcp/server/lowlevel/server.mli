(**
   MCP Server Module

   This module provides a framework for creating an MCP (Model Context Protocol) server.
   It allows you to easily define and handle various types of requests and notifications
   in an asynchronous manner.
*)

open Mcp.Types

(** Types for tool call results *)
type unstructured_content = content_block list
type structured_content = (string * Yojson.Safe.t) list
type combination_content = unstructured_content * structured_content

type tool_result = [
  | `Unstructured of unstructured_content
  | `Structured of structured_content
  | `Combined of combination_content
]

(** Notification options for server capabilities *)
type notification_options = {
  prompts_changed: bool;
  resources_changed: bool;
  tools_changed: bool;
}

(** Default notification options with all flags set to false *)
val default_notification_options : notification_options

(** Server type parameterized by lifespan context type *)
type 'lifespan_context t

(** Create a new server instance *)
val create :
  ?version:string option ->
  ?instructions:string option ->
  ?lifespan_start:(unit -> 'lifespan_context Lwt.t) ->
  ?lifespan_end:('lifespan_context -> unit Lwt.t) ->
  name:string ->
  unit ->
  'lifespan_context t

(** Get the current request context if available *)
val get_request_context :
  'lifespan_context t ->
  (Mcp.Types.request_id * 'lifespan_context * Mcp.Server.Session.t) option

(** Create initialization options for the server *)
val create_initialization_options :
  'lifespan_context t ->
  ?notification_options:notification_options ->
  ?experimental_capabilities:(string, Yojson.Safe.t) Base.Map.t ->
  unit ->
  Mcp.Server.Models.initialization_options

(** Get server capabilities *)
val get_capabilities :
  'lifespan_context t ->
  notification_options ->
  (string, Yojson.Safe.t) Base.Map.t ->
  server_capabilities

(** Register a handler for list prompts request *)
val register_list_prompts :
  'lifespan_context t ->
  (unit -> prompt list Lwt.t) ->
  unit

(** Register a handler for get prompt request *)
val register_get_prompt :
  'lifespan_context t ->
  (name:string -> ?arguments:(string, string) Base.Map.t -> unit -> get_prompt_result Lwt.t) ->
  unit

(** Register a handler for list resources request *)
val register_list_resources :
  'lifespan_context t ->
  (unit -> resource list Lwt.t) ->
  unit

(** Register a handler for list resource templates request *)
val register_list_resource_templates :
  'lifespan_context t ->
  (unit -> resource_template list Lwt.t) ->
  unit

(** Register a handler for read resource request *)
val register_read_resource :
  'lifespan_context t ->
  (Uri.t -> ([> `Single of [> `Text of string | `Blob of string ] * string option
            | `Multiple of Helper_types.read_resource_contents list ]) Lwt.t) ->
  unit

(** Register a handler for set logging level request *)
val register_set_logging_level :
  'lifespan_context t ->
  (logging_level -> unit Lwt.t) ->
  unit

(** Register a handler for subscribe resource request *)
val register_subscribe_resource :
  'lifespan_context t ->
  (Uri.t -> unit Lwt.t) ->
  unit

(** Register a handler for unsubscribe resource request *)
val register_unsubscribe_resource :
  'lifespan_context t ->
  (Uri.t -> unit Lwt.t) ->
  unit

(** Register a handler for call tool request *)
val register_call_tool :
  ?validate_input:bool ->
  'lifespan_context t ->
  (name:string -> arguments:(string, Yojson.Safe.t) Base.Map.t -> unit -> tool_result Lwt.t) ->
  unit

(** Register a handler for progress notification *)
val register_progress_notification :
  'lifespan_context t ->
  (progress_token:string -> progress:float -> ?total:float -> ?message:string -> unit -> unit Lwt.t) ->
  unit

(** Register a handler for completion request *)
val register_completion :
  'lifespan_context t ->
  (prompt_reference -> completion_argument -> completion_context option -> completion option Lwt.t) ->
  unit

(** Run the server *)
val run :
  'lifespan_context t ->
  read_stream:(Mcp.Shared.Message.session_message, [> `Error of exn ]) result Lwt_stream.t ->
  write_stream:(Mcp.Shared.Message.session_message -> unit Lwt.t) ->
  init_options:Mcp.Server.Models.initialization_options ->
  ?raise_exceptions:bool ->
  ?stateless:bool ->
  unit ->
  unit Lwt.t 