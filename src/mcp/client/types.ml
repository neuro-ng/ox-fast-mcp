open Lwt.Syntax
open Mcp_types
open Mcp_shared

type sampling_fn =
  request_context ->
  create_message_request_params ->
  (create_message_result, error_data) result Lwt.t

type elicitation_fn =
  request_context ->
  elicit_request_params ->
  (elicit_result, error_data) result Lwt.t

type list_roots_fn =
  request_context -> (list_roots_result, error_data) result Lwt.t

type logging_fn = logging_message_notification_params -> unit Lwt.t

type message_handler =
  [ `Request of (server_request, client_result) request_responder
  | `Notification of server_notification
  | `Exception of exn ] ->
  unit Lwt.t

let default_message_handler _ = Lwt.return_unit

let default_sampling_callback _ctx _params =
  Lwt.return
    (Error { code = invalid_request; message = "Sampling not supported" })

let default_elicitation_callback _ctx _params =
  Lwt.return
    (Error { code = invalid_request; message = "Elicitation not supported" })

let default_list_roots_callback _ctx =
  Lwt.return
    (Error { code = invalid_request; message = "List roots not supported" })

let default_logging_callback _params = Lwt.return_unit
