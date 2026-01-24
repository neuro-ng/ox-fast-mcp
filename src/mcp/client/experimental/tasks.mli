(** Experimental client-side task support.

    This module provides client methods for interacting with MCP tasks.

    WARNING: These APIs are experimental and may change without notice.

    NOTE: This module currently has placeholder implementations due to missing
    task types in Mcp.Types and incomplete Session.send_request. Full
    implementation will be available once these dependencies are complete.

    Example: (* Call a tool as a task *) let%bind result =
    Tasks.call_tool_as_task t "tool_name" ~arguments:(Some args) () in let
    task_id = result.taskId in

    (* Get task status *) let%bind status = Tasks.get_task t task_id in

    (* Poll task until completion *) let task_pipe = Tasks.poll_task t task_id
    in Pipe.iter task_pipe ~f:(fun status -> printf "Task status: %s\n"
    status.status; Deferred.unit ) ... *)

open Async
module Types = Mcp.Types
module Polling = Mcp_shared_experimental_tasks.Polling

(* Use polling module's get_task_result type *)
type get_task_result = Polling.get_task_result
type create_task_result = { taskId : string }

type list_tasks_result = {
  tasks : get_task_result list;
  nextCursor : string option;
}

type cancel_task_result = get_task_result

type t
(** Experimental client features for tasks and other experimental APIs.

    WARNING: These APIs are experimental and may change without notice.

    Access via session.experimental: let%bind status = Tasks.get_task
    experimental task_id in ... *)

val create : Mcp_client.Session.t -> t
(** Create experimental features accessor for a session. *)

val call_tool_as_task :
  t ->
  string ->
  ?arguments:Yojson.Safe.t ->
  ?ttl:int ->
  ?meta:Yojson.Safe.t ->
  unit ->
  create_task_result Deferred.t
(** Call a tool as a task, returning a CreateTaskResult for polling.

    This is a convenience method for calling tools that support task execution.
    The server will return a task reference instead of the immediate result,
    which can then be polled via [get_task] and retrieved via [get_task_result].

    Args: name: The tool name arguments: Tool arguments ttl: Task time-to-live
    in milliseconds (default: 60000 = 1 minute) meta: Optional metadata to
    include in the request

    Returns: CreateTaskResult containing the task reference *)

val get_task : t -> string -> get_task_result Deferred.t
(** Get the current status of a task.

    NOTE: Currently returns placeholder data - requires Session.send_request
    implementation.

    Args: task_id: The task identifier

    Returns: GetTaskResult containing the task status and metadata *)

val get_task_result : t -> string -> Yojson.Safe.t Deferred.t
(** Get the result of a completed task.

    The result type depends on the original request type:
    - tools/call tasks return CallToolResult
    - Other request types return their corresponding result type

    NOTE: Currently not implemented - requires Session.send_request.

    Args: task_id: The task identifier

    Returns: The task result as JSON *)

val list_tasks : t -> ?cursor:string -> unit -> list_tasks_result Deferred.t
(** List all tasks.

    NOTE: Currently returns empty list - requires Session.send_request
    implementation.

    Args: cursor: Optional pagination cursor

    Returns: ListTasksResult containing tasks and optional next cursor *)

val cancel_task : t -> string -> cancel_task_result Deferred.t
(** Cancel a running task.

    NOTE: Currently returns placeholder data - requires Session.send_request
    implementation.

    Args: task_id: The task identifier

    Returns: CancelTaskResult with the updated task state *)

val poll_task : t -> string -> get_task_result Pipe.Reader.t
(** Poll a task until it reaches a terminal status.

    Yields GetTaskResult for each poll, allowing the caller to react to status
    changes (e.g., handle input_required). Exits when task reaches a terminal
    status (completed, failed, cancelled).

    Respects the pollInterval hint from the server.

    NOTE: Uses placeholder get_task implementation - full functionality requires
    Session.send_request.

    Args: task_id: The task identifier

    Returns: Pipe reader that yields GetTaskResult for each poll *)
