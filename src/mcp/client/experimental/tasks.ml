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

    (* Poll task *) let task_pipe = Tasks.poll_task t task_id in ... *)

open Core
open Async
module Types = Mcp.Types
module Session = Mcp_client.Session
module Polling = Mcp_shared_experimental_tasks.Polling

(* Use polling module's get_task_result type *)
type get_task_result = Polling.get_task_result
type create_task_result = { taskId : string }

type list_tasks_result = {
  tasks : get_task_result list;
  nextCursor : string option;
}

type cancel_task_result = get_task_result

type t = { session : Session.t }
(** Experimental client features for tasks and other experimental APIs.

    WARNING: These APIs are experimental and may change without notice.

    Access via session.experimental: let%bind status = Tasks.get_task
    experimental task_id in ... *)

let create session = { session }

let call_tool_as_task _t _name ?arguments:_ ?ttl:_ ?meta:_ () =
  (* TODO: Requires Session.call_tool to support task metadata *)
  (* And needs CreateTaskResult return type *)
  failwith
    "call_tool_as_task not implemented - requires Session support for task \
     metadata and CreateTaskResult return type"

let get_task _t task_id =
  (* TODO: Requires Session.send_request to be implemented with GetTaskRequest support *)
  (* Returning placeholder for now *)
  Deferred.return
    {
      Polling.taskId = task_id;
      status = "working";
      pollInterval = Some 500;
      createdAt = Time_ns.now ();
      lastUpdatedAt = Time_ns.now ();
    }

let get_task_result _t _task_id =
  (* TODO: Requires Session.send_request with GetTaskPayloadRequest support *)
  failwith "get_task_result not implemented - requires Session.send_request"

let list_tasks _t ?cursor:_ () =
  (* TODO: Requires Session.send_request with ListTasksRequest support *)
  Deferred.return { tasks = []; nextCursor = None }

let cancel_task _t task_id =
  (* TODO: Requires Session.send_request with CancelTaskRequest support *)
  Deferred.return
    {
      Polling.taskId = task_id;
      status = "cancelled";
      pollInterval = None;
      createdAt = Time_ns.now ();
      lastUpdatedAt = Time_ns.now ();
    }

let poll_task t task_id =
  Polling.poll_until_terminal ~get_task:(get_task t) ~task_id ()
