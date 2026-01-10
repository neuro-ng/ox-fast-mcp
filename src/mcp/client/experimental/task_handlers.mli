(** Experimental task handler protocols for server → client requests.

    This module provides handler types and default handlers for when servers
    send task-related requests to clients (the reverse of normal client → server
    flow).

    WARNING: These APIs are experimental and may change without notice.

    NOTE: This module currently has placeholder implementations due to missing
    task types in Mcp.Types. Full implementation will be available once task
    types are added to the MCP types module.

    Use cases:
    - Server sends task-augmented sampling/elicitation request to client
    - Client creates a local task, spawns background work, returns
      CreateTaskResult
    - Server polls client's task status via tasks/get, tasks/result, etc. *)

open Core
open Async
module Types = Mcp.Types
module Context = Mcp_shared.Context
module Session = Mcp_shared.Session

(* Placeholder types until task types are added to Mcp.Types *)
type task_metadata = { ttl : int option }
type create_task_result = { taskId : string }
type get_task_request_params = { taskId : string }
type get_task_result = Mcp_shared_experimental_tasks.Polling.get_task_result
type get_task_payload_request_params = { taskId : string }
type get_task_payload_result = { result : Yojson.Safe.t }
type paginated_request_params = { cursor : string option }

type list_tasks_result = {
  tasks : get_task_result list;
  nextCursor : string option;
}

type cancel_task_request_params = { taskId : string }
type cancel_task_result = get_task_result

type get_task_handler_fn =
  (unit, unit, unit) Context.t ->
  get_task_request_params ->
  (get_task_result, Types.error_data) Result.t Deferred.t
(** Handler for tasks/get requests from server.

    WARNING: This is experimental and may change without notice. *)

type get_task_result_handler_fn =
  (unit, unit, unit) Context.t ->
  get_task_payload_request_params ->
  (get_task_payload_result, Types.error_data) Result.t Deferred.t
(** Handler for tasks/result requests from server.

    WARNING: This is experimental and may change without notice. *)

type list_tasks_handler_fn =
  (unit, unit, unit) Context.t ->
  paginated_request_params option ->
  (list_tasks_result, Types.error_data) Result.t Deferred.t
(** Handler for tasks/list requests from server.

    WARNING: This is experimental and may change without notice. *)

type cancel_task_handler_fn =
  (unit, unit, unit) Context.t ->
  cancel_task_request_params ->
  (cancel_task_result, Types.error_data) Result.t Deferred.t
(** Handler for tasks/cancel requests from server.

    WARNING: This is experimental and may change without notice. *)

type task_augmented_sampling_fn =
  (unit, unit, unit) Context.t ->
  Types.create_message_request_params ->
  task_metadata ->
  (create_task_result, Types.error_data) Result.t Deferred.t
(** Handler for task-augmented sampling/createMessage requests from server.

    When server sends a CreateMessageRequest with task field, this callback is
    invoked. The callback should create a task, spawn background work, and
    return CreateTaskResult immediately.

    WARNING: This is experimental and may change without notice. *)

type task_augmented_elicitation_fn =
  (unit, unit, unit) Context.t ->
  Types.elicit_request_params ->
  task_metadata ->
  (create_task_result, Types.error_data) Result.t Deferred.t
(** Handler for task-augmented elicitation/create requests from server.

    When server sends an ElicitRequest with task field, this callback is
    invoked. The callback should create a task, spawn background work, and
    return CreateTaskResult immediately.

    WARNING: This is experimental and may change without notice. *)

type t = {
  get_task : get_task_handler_fn;
  get_task_result : get_task_result_handler_fn;
  list_tasks : list_tasks_handler_fn;
  cancel_task : cancel_task_handler_fn;
  augmented_sampling : task_augmented_sampling_fn;
  augmented_elicitation : task_augmented_elicitation_fn;
}
(** Container for experimental task handlers.

    Groups all task-related handlers that handle server → client requests. This
    includes both pure task requests (get, list, cancel, result) and
    task-augmented request handlers (sampling, elicitation with task field).

    WARNING: These APIs are experimental and may change without notice. *)

val default_get_task_handler : get_task_handler_fn
val default_get_task_result_handler : get_task_result_handler_fn
val default_list_tasks_handler : list_tasks_handler_fn
val default_cancel_task_handler : cancel_task_handler_fn
val default_task_augmented_sampling : task_augmented_sampling_fn
val default_task_augmented_elicitation : task_augmented_elicitation_fn

val create :
  ?get_task:get_task_handler_fn ->
  ?get_task_result:get_task_result_handler_fn ->
  ?list_tasks:list_tasks_handler_fn ->
  ?cancel_task:cancel_task_handler_fn ->
  ?augmented_sampling:task_augmented_sampling_fn ->
  ?augmented_elicitation:task_augmented_elicitation_fn ->
  unit ->
  t
(** Create experimental task handlers with optional overrides. *)

val build_capability : t -> unit option
(** Build ClientTasksCapability from the configured handlers.

    NOTE: Currently returns None - will return proper capability once
    ClientTasksCapability types are added to Mcp.Types.

    Returns a capability object that reflects which handlers are configured
    (i.e., not using the default "not supported" handlers). *)

val handles_request : Types.server_request -> bool
(** Check if this handler handles the given request type.

    NOTE: Currently returns false - will properly check once task request
    variants are added to Mcp.Types.server_request. *)

val handle_request :
  t ->
  (unit, unit, unit) Context.t ->
  (Types.server_request, Types.client_result) Session.Request_responder.t ->
  unit Deferred.t
(** Handle a task-related request from the server.

    NOTE: Currently does nothing - will dispatch to handlers once task request
    types are added to Mcp.Types.

    Call handles_request() first to check if this handler can handle the
    request. *)
