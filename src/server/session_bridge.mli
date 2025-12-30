(** Session Bridge Module Interface

    Provides Lwt/Async conversion utilities and session management stubs.
*)

open! Core
open! Async

(** Convert Lwt promise to Async Deferred **)
val lwt_to_async : 'a Lwt.t -> 'a Deferred.t

(** Convert Async Deferred to Lwt promise *)
val async_to_lwt : 'a Deferred.t -> 'a Lwt.t

(** Async session wrapper - stub implementation *)
module Async_session : sig
  type t

  val send_log_message :
    t ->
    level:Mcp.Types.logging_level ->
    data:Yojson.Safe.t ->
    ?logger:string ->
    ?related_request_id:Mcp.Types.request_id ->
    unit ->
    unit Deferred.t

  val send_progress_notification :
    t ->
    progress_token:Mcp.Types.request_id ->
    progress:float ->
    ?total:float ->
    ?message:string ->
    ?related_request_id:Mcp.Types.request_id ->
    unit ->
    unit Deferred.t

  val send_resource_list_changed : t -> unit Deferred.t
  val send_tool_list_changed : t -> unit Deferred.t
  val send_prompt_list_changed : t -> unit Deferred.t
  val send_resource_updated : t -> uri:Uri.t -> unit Deferred.t

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
    Mcp.Types.client_request Deferred.t

  val elicit :
    t ->
    message:string ->
    requested_schema:Yojson.Safe.t ->
    ?related_request_id:Mcp.Types.request_id ->
    unit ->
    Mcp.Types.client_request Deferred.t

  val list_roots : t -> Mcp.Types.client_request Deferred.t
  val send_ping : t -> Mcp.Types.client_request Deferred.t
  val check_client_capability : t -> Mcp.Types.client_capabilities -> bool
end
