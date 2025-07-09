open Mcp.Types
open Mcp.Shared

type sampling_handler =
  sampling_message list ->
  create_message_request_params ->
  request_context ->
  (create_message_result, error_data) result Lwt.t
(** Type for sampling handler function that processes messages and returns a
    response *)

val create_sampling_callback :
  sampling_handler ->
  (request_context ->
  create_message_request_params ->
  (create_message_result, error_data) result Lwt.t)
(** Create a sampling callback function from a handler
    @param sampling_handler
      The handler function that processes sampling requests
    @return
      A function that takes a request context and params and returns a result *)
