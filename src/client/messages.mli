open Mcp.Types
open Mcp.Shared
open Lwt.Syntax

type t
(** Message handler class for handling MCP messages sent to the client *)

val create : unit -> t
(** Create a new message handler *)

val dispatch :
  t ->
  [ `Request of (server_request, client_result) request_responder
  | `Notification of server_notification
  | `Exception of exn ] ->
  unit Lwt.t
(** Dispatch a message to appropriate handlers *)

val handle_message :
  t ->
  [ `Request of (server_request, client_result) request_responder
  | `Notification of server_notification
  | `Exception of exn ] ->
  unit Lwt.t
(** Handle a message *)

val handle_request :
  t -> (server_request, client_result) request_responder -> unit Lwt.t
(** Handle a request *)

val handle_notification : t -> server_notification -> unit Lwt.t
(** Handle a notification *)

val handle_exception : t -> exn -> unit Lwt.t
(** Handle an exception *)

val handle_ping : t -> ping_request -> unit Lwt.t
(** Handle a ping request *)

val handle_list_roots : t -> list_roots_request -> unit Lwt.t
(** Handle a list roots request *)

val handle_create_message : t -> create_message_request_params -> unit Lwt.t
(** Handle a create message request *)

val handle_progress : t -> progress_notification_params -> unit Lwt.t
(** Handle a progress notification *)

val handle_logging_message :
  t -> logging_message_notification_params -> unit Lwt.t
(** Handle a logging message notification *)

val handle_tool_list_changed : t -> tool_list_changed_notification -> unit Lwt.t
(** Handle a tool list changed notification *)

val handle_resource_list_changed :
  t -> resource_list_changed_notification -> unit Lwt.t
(** Handle a resource list changed notification *)

val handle_prompt_list_changed :
  t -> prompt_list_changed_notification -> unit Lwt.t
(** Handle a prompt list changed notification *)

val handle_resource_updated :
  t -> resource_updated_notification_params -> unit Lwt.t
(** Handle a resource updated notification *)

val handle_cancelled : t -> cancelled_notification_params -> unit Lwt.t
(** Handle a cancelled notification *)
