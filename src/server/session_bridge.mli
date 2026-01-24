(** Session Bridge Module - Now just a direct pass-through

    Since ServerSession has been migrated to Async, this module now simply
    re-exports the session functions without any Lwt/Async bridging. *)

open! Core
open! Async

(** Async wrapper for ServerSession

    Now that ServerSession is Async-native, this is just a direct pass-through. *)
module Async_session : sig
  type t = Mcp_server.Session.t
  (** Session type *)

  val send_log_message :
    t ->
    level:Mcp.Types.logging_level ->
    data:Yojson.Safe.t ->
    ?logger:string ->
    ?related_request_id:Mcp.Types.request_id ->
    unit ->
    unit Async.Deferred.t
  (** Send log message *)

  val send_progress_notification :
    t ->
    progress_token:Mcp.Types.request_id ->
    progress:float ->
    ?total:float ->
    ?message:string ->
    ?related_request_id:Mcp.Types.request_id ->
    unit ->
    unit Async.Deferred.t
  (** Send progress notification *)

  val send_resource_list_changed : t -> unit Async.Deferred.t
  (** Send resource list changed *)

  val send_tool_list_changed : t -> unit Async.Deferred.t
  (** Send tool list changed *)

  val send_prompt_list_changed : t -> unit Async.Deferred.t
  (** Send prompt list changed *)

  val send_resource_updated : t -> uri:Uri.t -> unit Async.Deferred.t
  (** Send resource updated notification *)

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
    Mcp.Types.client_request Async.Deferred.t
  (** Create message / sampling *)

  val elicit :
    t ->
    message:string ->
    requested_schema:Mcp.Types.elicit_requested_schema ->
    ?related_request_id:Mcp.Types.request_id ->
    unit ->
    Mcp.Types.client_request Async.Deferred.t
  (** Elicit *)

  val list_roots : t -> Mcp.Types.client_request Async.Deferred.t
  (** List roots *)

  val send_ping : t -> Mcp.Types.client_request Async.Deferred.t
  (** Send ping *)

  val check_client_capability : t -> Mcp.Types.client_capabilities -> bool
  (** Check client capability *)

  val incoming_messages :
    t ->
    [ `Request of
      ( Mcp.Types.client_request,
        Mcp.Types.server_result )
      Mcp_shared.Session.Request_responder.t
    | `Notification of Mcp.Types.client_notification
    | `Error of exn ]
    Async.Pipe.Reader.t
  (** Get incoming message stream *)
end
