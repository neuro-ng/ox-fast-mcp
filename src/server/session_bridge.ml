(** Session Bridge Module - Now just a direct pass-through

    Since ServerSession has been migrated to Async, this module now simply
    re-exports the session functions without any Lwt/Async bridging. *)

open! Core
open! Async

(** Async wrapper for ServerSession

    Now that ServerSession is Async-native, this is just a direct pass-through. *)
module Async_session = struct
  (* Wire to the actual MCP ServerSession *)
  type t = Mcp_server.Session.t

  (** Send log message **)
  let send_log_message = Mcp_server.Session.send_log_message

  (** Send progress notification *)
  let send_progress_notification = Mcp_server.Session.send_progress_notification

  (** Send resource list changed *)
  let send_resource_list_changed = Mcp_server.Session.send_resource_list_changed

  (** Send tool list changed *)
  let send_tool_list_changed = Mcp_server.Session.send_tool_list_changed

  (** Send prompt list changed *)
  let send_prompt_list_changed = Mcp_server.Session.send_prompt_list_changed

  (** Send resource updated notification *)
  let send_resource_updated = Mcp_server.Session.send_resource_updated

  (** Create message / sampling *)
  let create_message = Mcp_server.Session.create_message

  (** Elicit *)
  let elicit = Mcp_server.Session.elicit

  (** List roots *)
  let list_roots = Mcp_server.Session.list_roots

  (** Send ping *)
  let send_ping = Mcp_server.Session.send_ping

  (** Check client capability *)
  let check_client_capability = Mcp_server.Session.check_client_capability

  (** Get incoming message stream *)
  let[@warning "-32"] incoming_messages = Mcp_server.Session.incoming_messages
end
