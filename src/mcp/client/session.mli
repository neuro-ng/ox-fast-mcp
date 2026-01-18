open Core
open Async
open Mcp.Types

(** Client session for MCP *)

type t
(** Client session type - opaque *)

type error =
  [ `Timeout of string
  | `InvalidMessage of string
  | `RequestFailed of string
  | `ValidationError of string
  | `ProtocolError of string ]
(** Error type for session operations *)

type ('req, 'res) request_responder
type request_context

type sampling_fn =
  request_context ->
  create_message_request_params ->
  (create_message_result, error_data) Result.t Deferred.t

type elicitation_fn =
  request_context ->
  elicit_request_params ->
  (elicit_result, error_data) Result.t Deferred.t

type list_roots_fn =
  request_context -> (list_roots_result, error_data) Result.t Deferred.t

type logging_fn = logging_message_notification_params -> unit Deferred.t

type message_handler =
  [ `Exception of exn
  | `Notification of server_notification
  | `Request of (server_request, client_result) request_responder ] ->
  unit Deferred.t

type progress_fn = float -> float option -> string option -> unit Deferred.t

val default_client_info : implementation
(** Default client info *)

val default_logging_callback : logging_fn
(** Default logging callback that does nothing *)

val default_message_handler : message_handler
(** Default message handler that does nothing *)

val error_to_string : error -> string

val create_from_pipes :
  read_stream:Mcp_shared.Message.session_message Pipe.Reader.t ->
  write_stream:Mcp_shared.Message.session_message Pipe.Writer.t ->
  ?read_timeout:Time_ns.Span.t ->
  ?sampling_callback:sampling_fn ->
  ?elicitation_callback:elicitation_fn ->
  ?list_roots_callback:list_roots_fn ->
  ?logging_callback:logging_fn ->
  ?message_handler:message_handler ->
  ?client_info:implementation ->
  unit ->
  t
(** Create a session from existing pipes (for use by session_group) *)

val initialize : t -> initialize_result Deferred.t
(** Initialize the session *)

val send_ping : t -> empty_result Deferred.t
(** Send a ping request *)

val set_logging_level : t -> logging_level -> empty_result Deferred.t
(** Set logging level *)

val list_resources :
  t -> ?cursor:string -> unit -> list_resources_result Deferred.t
(** List resources *)

val list_prompts : t -> ?cursor:string -> unit -> list_prompts_result Deferred.t
(** List prompts *)

val list_resource_templates :
  t -> ?cursor:string -> unit -> list_resource_templates_result Deferred.t
(** List resource templates *)

val read_resource : t -> string -> read_resource_result Deferred.t
(** Read resource by URI string *)

val subscribe_resource : t -> string -> empty_result Deferred.t
(** Subscribe to resource by URI string *)

val unsubscribe_resource : t -> string -> empty_result Deferred.t
(** Unsubscribe from resource by URI string *)

val call_tool :
  t ->
  string ->
  ?arguments:Yojson.Safe.t ->
  ?read_timeout:int ->
  ?progress_callback:progress_fn ->
  unit ->
  call_tool_result Deferred.t
(** Call tool *)

val list_tools : t -> ?cursor:string -> unit -> list_tools_result Deferred.t
(** List tools *)

val send_roots_list_changed : t -> unit Deferred.t
(** Send roots list changed notification *)

val send_request :
  t ->
  method_name:string ->
  params:Yojson.Safe.t option ->
  result_decoder:(Yojson.Safe.t -> ('a, string) Result.t) ->
  unit ->
  'a Deferred.t
(** Send a custom JSON-RPC request. Used by experimental APIs.
    @param method_name The JSON-RPC method name
    @param params Optional parameters
    @param result_decoder Function to decode the result JSON
    @return The decoded result *)
