(** Session Bridge Module - Full Lwt/Async Integration

    Provides bidirectional conversion between Lwt and Async for integrating
    MCP ServerSession (Lwt-based) with OxFastMCP server (Async-based).
*)

open! Core
open! Async

(** Convert Lwt promise to Async Deferred 
    
    Uses thread-safe Ivar to bridge Lwt and Async event loops.
*)
let lwt_to_async (lwt_promise : 'a Lwt.t) : 'a Deferred.t =
  Deferred.create (fun ivar ->
      (* Schedule Lwt computation *)
      Lwt.async (fun () ->
          Lwt.bind lwt_promise (fun result ->
              (* Fill Ivar when Lwt completes - thread safe via block_on_async *)
              Thread_safe.block_on_async_exn (fun () ->
                  Ivar.fill_exn ivar result;
                  Deferred.unit);
              Lwt.return_unit)))

(** Convert Async Deferred to Lwt promise *)
let async_to_lwt (deferred : 'a Deferred.t) : 'a Lwt.t =
  let promise, resolver = Lwt.wait () in
  don't_wait_for
    (deferred >>| fun result ->
       Lwt.wakeup resolver result);
  promise

(** Async wrapper for ServerSession  
    
    Wraps the Lwt-based MCP ServerSession with Async-compatible interface.
    Uses the bridge functions above to convert between Lwt and Async.
*)
module Async_session = struct
  (* Wire to the actual MCP ServerSession *)
  type t = Mcp_server.Session.t

  (** Send log message (Async version) *)
  let send_log_message t ~level ~data ?logger ?related_request_id () =
    lwt_to_async 
      (Mcp_server.Session.send_log_message t ~level ~data ?logger ?related_request_id ())

  (** Send progress notification (Async version) *)
  let send_progress_notification t ~progress_token ~progress ?total
      ?message ?related_request_id () =
    lwt_to_async
      (Mcp_server.Session.send_progress_notification t ~progress_token ~progress ?total
         ?message ?related_request_id ())

  (** Send resource list changed (Async version) *)
  let send_resource_list_changed t =
    lwt_to_async (Mcp_server.Session.send_resource_list_changed t)

  (** Send tool list changed (Async version) *)
  let send_tool_list_changed t =
    lwt_to_async (Mcp_server.Session.send_tool_list_changed t)

  (** Send prompt list changed (Async version) *)
  let send_prompt_list_changed t =
    lwt_to_async (Mcp_server.Session.send_prompt_list_changed t)

  (** Send resource updated notification (Async version) *)
  let send_resource_updated t ~uri =
    lwt_to_async (Mcp_server.Session.send_resource_updated t ~uri)

  (** Create message / sampling (Async version) *)
  let create_message t ~messages ~max_tokens ?system_prompt
      ?include_context ?temperature ?stop_sequences ?metadata
      ?model_preferences ?related_request_id () =
    lwt_to_async
      (Mcp_server.Session.create_message t ~messages ~max_tokens ?system_prompt
         ?include_context ?temperature ?stop_sequences ?metadata
         ?model_preferences ?related_request_id ())

  (** Elicit (Async version) *)
  let elicit t ~message ~requested_schema ?related_request_id () =
    lwt_to_async
      (Mcp_server.Session.elicit t ~message ~requested_schema ?related_request_id ())

  (** List roots (Async version) *)
  let list_roots t =
    lwt_to_async (Mcp_server.Session.list_roots t)

  (** Send ping (Async version) *)
  let send_ping t =
    lwt_to_async (Mcp_server.Session.send_ping t)

  (** Check client capability (pure function, no async needed) *)
  let check_client_capability t capability =
    Mcp_server.Session.check_client_capability t capability

  (** Get incoming message stream *)
  let[@warning "-32"] incoming_messages t =
    Mcp_server.Session.incoming_messages t
end

(* Full Lwt-based ServerSession now wired through Async bridge! *)
