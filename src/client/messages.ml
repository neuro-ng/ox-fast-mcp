open Core
open Mcp.Types
open Mcp.Shared
open Lwt.Syntax

type t = {
  mutable message_handler :
    t ->
    [ `Request of (server_request, client_result) request_responder
    | `Notification of server_notification
    | `Exception of exn ] ->
    unit Lwt.t;
}

let create () = { message_handler = (fun _ _ -> Lwt.return_unit) }

(** Dispatch a message to appropriate handlers *)
let dispatch t message =
  let* () = handle_message t message in
  match message with
  | `Request responder -> (
    let* () = handle_request t responder in
    match responder.request.root with
    | `Ping params ->
      let* () = handle_ping t params in
      responder.respond (`Empty { meta = None })
    | `ListRoots params ->
      let* () = handle_list_roots t params in
      responder.respond (`Empty { meta = None })
    | `CreateMessage params ->
      let* () = handle_create_message t params in
      responder.respond (`Empty { meta = None })
    | `Elicit params ->
      let* () = Lwt.return_unit in
      (* Not implemented in Python version *)
      responder.respond (`Empty { meta = None }))
  | `Notification notif -> handle_notification t notif
  | `Exception exn -> handle_exception t exn

let handle_message t message = t.message_handler t message
let handle_request t request = Lwt.return_unit

let handle_notification t = function
  | `Cancelled (request_id, reason) ->
    handle_cancelled t { request_id; reason; meta = None }
  | `Progress params -> handle_progress t params
  | `LoggingMessage params -> handle_logging_message t params
  | `ResourceUpdated uri -> handle_resource_updated t { uri; meta = None }
  | `ResourceListChanged -> handle_resource_list_changed t { meta = None }
  | `ToolListChanged -> handle_tool_list_changed t { meta = None }
  | `PromptListChanged -> handle_prompt_list_changed t { meta = None }

let handle_exception t exn =
  let* () =
    Lwt_io.printf "Exception in message handler: %s\n" (Exn.to_string exn)
  in
  match exn with
  | Mcp.Shared.Exceptions.Mcp_error error ->
    Logs.err (fun m -> m "MCP error: %s (code: %d)" error.message error.code);
    Lwt.return_unit
  | _ ->
    Logs.err (fun m -> m "Unexpected error: %s" (Exn.to_string exn));
    Lwt.return_unit

let handle_ping _t _params = Lwt.return_unit
let handle_list_roots _t _params = Lwt.return_unit
let handle_create_message _t _params = Lwt.return_unit

let handle_progress _t params =
  let* () = Lwt_io.printf "Progress: %.2f" params.progress in
  match (params.total, params.message) with
  | Some total, Some msg -> Lwt_io.printf " / %.2f - %s\n" total msg
  | Some total, None -> Lwt_io.printf " / %.2f\n" total
  | None, Some msg -> Lwt_io.printf " - %s\n" msg
  | None, None -> Lwt_io.printf "\n"

let handle_logging_message _t params =
  let level_str =
    match params.level with
    | `Debug -> "DEBUG"
    | `Info -> "INFO"
    | `Notice -> "NOTICE"
    | `Warning -> "WARNING"
    | `Error -> "ERROR"
    | `Critical -> "CRITICAL"
    | `Alert -> "ALERT"
    | `Emergency -> "EMERGENCY"
  in
  let logger_str =
    match params.logger with
    | Some logger -> sprintf "[%s]" logger
    | None -> ""
  in
  Lwt_io.printf "%s%s: %s\n" level_str logger_str
    (Yojson.Safe.pretty_to_string params.data)

let handle_tool_list_changed _t _params = Lwt_io.printf "Tool list changed\n"

let handle_resource_list_changed _t _params =
  Lwt_io.printf "Resource list changed\n"

let handle_prompt_list_changed _t _params =
  Lwt_io.printf "Prompt list changed\n"

let handle_resource_updated _t params =
  Lwt_io.printf "Resource updated: %s\n" params.uri

let handle_cancelled _t params =
  let reason_str =
    match params.reason with
    | Some reason -> sprintf " (%s)" reason
    | None -> ""
  in
  Lwt_io.printf "Request cancelled%s\n" reason_str
