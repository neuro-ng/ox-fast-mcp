(** Shared polling utilities for task operations.

    This module provides generic polling logic that works for both client→server
    and server→client task polling.

    WARNING: These APIs are experimental and may change without notice. *)

open Core
open Async
module Types = Mcp.Types

type get_task_result = {
  taskId : string;
  status : string;
      (* "working" | "input_required" | "completed" | "failed" | "cancelled" *)
  pollInterval : int option;
  createdAt : Time_ns.t;
  lastUpdatedAt : Time_ns.t;
}
[@@deriving sexp, compare]
(** Placeholder type for GetTaskResult - TODO: Move to Mcp.Types when task
    support is added *)

val poll_until_terminal :
  get_task:(string -> get_task_result Deferred.t) ->
  task_id:string ->
  ?default_interval_ms:int ->
  unit ->
  get_task_result Pipe.Reader.t
(** Poll a task until it reaches terminal status.

    This is a generic utility that works for both client→server and
    server→client polling. The caller provides the get_task function appropriate
    for their direction.

    Args: get_task: Async function that takes task_id and returns GetTaskResult
    task_id: The task to poll default_interval_ms: Fallback poll interval if
    server doesn't specify (default: 500)

    Returns: A pipe reader that yields GetTaskResult for each poll *)
