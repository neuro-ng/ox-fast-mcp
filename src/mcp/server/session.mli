open Core
open Lwt.Syntax

(** Server session initialization state *)
type initialization_state = Not_initialized | Initializing | Initialized

type t = {
  mutable initialization_state : initialization_state;
  mutable client_params : Mcp.Types.initialize_request_params option;
  init_options : Models.initialization_options;
  incoming_message_stream :
    ( [ `Request of
        ( Mcp.Types.client_request,
          Mcp.Types.server_result )
        Shared.Session.Request_responder.t
      | `Notification of Mcp.Types.client_notification
      | `Error of exn ],
      [ `Closed ] )
    Lwt_stream.t;
}
[@@deriving sexp]
(** Server session type *)

val create :
  read_stream:
    (Shared.Message.session_message, [> `Msg of string ]) result Lwt_stream.t ->
  write_stream:(Shared.Message.session_message -> unit Lwt.t) ->
  init_options:Models.initialization_options ->
  ?stateless:bool ->
  unit ->
  t
(** Create a new server session *)

val check_client_capability : t -> Mcp.Types.client_capabilities -> bool
(** Check if client supports a specific capability *)

val send_log_message :
  t ->
  level:Mcp.Types.logging_level ->
  data:Yojson.Safe.t ->
  ?logger:string ->
  ?related_request_id:Mcp.Types.request_id ->
  unit ->
  unit Lwt.t
(** Send a log message notification *)

val send_resource_updated : t -> uri:Uri.t -> unit Lwt.t
(** Send a resource updated notification *)

val create_message :
  t ->
  messages:Mcp.Types.sampling_message list ->
  max_tokens:int ->
  ?system_prompt:string ->
  ?include_context:Mcp.Types.include_context ->
  ?temperature:float ->
  ?stop_sequences:string list ->
  ?metadata:Yojson.Safe.t ->
  ?model_preferences:Mcp.Types.model_preferences ->
  ?related_request_id:Mcp.Types.request_id ->
  unit ->
  Mcp.Types.create_message_result Lwt.t
(** Send a create message request *)

val list_roots : t -> Mcp.Types.list_roots_result Lwt.t
(** Send a list roots request *)

val elicit :
  t ->
  message:string ->
  requested_schema:Mcp.Types.elicit_requested_schema ->
  ?related_request_id:Mcp.Types.request_id ->
  unit ->
  Mcp.Types.elicit_result Lwt.t
(** Send an elicit request *)

val send_ping : t -> Mcp.Types.empty_result Lwt.t
(** Send a ping request *)

val send_progress_notification :
  t ->
  progress_token:Mcp.Types.request_id ->
  progress:float ->
  ?total:float ->
  ?message:string ->
  ?related_request_id:Mcp.Types.request_id ->
  unit ->
  unit Lwt.t
(** Send a progress notification *)

val send_resource_list_changed : t -> unit Lwt.t
(** Send a resource list changed notification *)

val send_tool_list_changed : t -> unit Lwt.t
(** Send a tool list changed notification *)

val send_prompt_list_changed : t -> unit Lwt.t
(** Send a prompt list changed notification *)

val incoming_messages :
  t ->
  ( [ `Request of
      ( Mcp.Types.client_request,
        Mcp.Types.server_result )
      Shared.Session.Request_responder.t
    | `Notification of Mcp.Types.client_notification
    | `Error of exn ],
    [ `Closed ] )
  Lwt_stream.t
(** Get the incoming messages stream *)
