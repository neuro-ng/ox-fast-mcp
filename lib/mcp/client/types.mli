open Mcp_types
open Mcp_shared

(** Type for sampling callback functions *)
type sampling_fn = request_context -> create_message_request_params -> (create_message_result, error_data) result Lwt.t

(** Type for elicitation callback functions *)
type elicitation_fn = request_context -> elicit_request_params -> (elicit_result, error_data) result Lwt.t

(** Type for list roots callback functions *)
type list_roots_fn = request_context -> (list_roots_result, error_data) result Lwt.t

(** Type for logging callback functions *)
type logging_fn = logging_message_notification_params -> unit Lwt.t

(** Type for message handler functions *)
type message_handler = [
  | `Request of (server_request, client_result) request_responder
  | `Notification of server_notification
  | `Exception of exn
] -> unit Lwt.t

(** Default message handler *)
val default_message_handler : message_handler

(** Default sampling callback *)
val default_sampling_callback : sampling_fn

(** Default elicitation callback *)
val default_elicitation_callback : elicitation_fn

(** Default list roots callback *)
val default_list_roots_callback : list_roots_fn

(** Default logging callback *)
val default_logging_callback : logging_fn 