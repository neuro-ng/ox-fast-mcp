open Core
open Mcp.Types
open Mcp.Shared
open Lwt.Syntax

type sampling_handler = 
  sampling_message list ->
  create_message_request_params ->
  request_context ->
  (create_message_result, error_data) result Lwt.t

let create_sampling_callback (handler: sampling_handler) =
  fun context params ->
    try%lwt
      let* result = handler params.messages params context in
      Lwt.return result
    with exn ->
      Lwt.return (Error {
        code = internal_error;
        message = Exn.to_string exn;
        data = None;
        meta = None;
      }) 