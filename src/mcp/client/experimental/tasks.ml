(** Experimental client-side task support.

    This module provides client methods for interacting with MCP tasks.

    WARNING: These APIs are experimental and may change without notice.

    Uses Session.send_request for JSON-RPC communication with task endpoints.

    Example: (* Get task status *) let%bind status = Tasks.get_task t task_id in

    (* List all tasks *) let%bind result = Tasks.list_tasks t () in

    (* Poll task until complete *) let task_pipe = Tasks.poll_task t task_id in
    ... *)

open Core
open! Async
open Ppx_yojson_conv_lib.Yojson_conv.Primitives
module Types = Mcp.Types
module Session = Mcp_client.Session
module Polling = Mcp_shared_experimental_tasks.Polling

(* Use polling module's get_task_result type and converters *)
type get_task_result = Polling.get_task_result

let yojson_of_get_task_result = Polling.yojson_of_get_task_result
let get_task_result_of_yojson = Polling.get_task_result_of_yojson

type create_task_result = { taskId : string } [@@deriving yojson]

type list_tasks_result = {
  tasks : get_task_result list;
  nextCursor : string option; [@yojson.option]
}
[@@deriving yojson]

type cancel_task_result = get_task_result

type t = { session : Session.t }
(** Experimental client features for tasks and other experimental APIs.

    WARNING: These APIs are experimental and may change without notice.

    Access via session.experimental: let%bind status = Tasks.get_task
    experimental task_id in ... *)

let create session = { session }

(** Call a tool and run it as a background task.

    Note: The server must support task-based tool execution for this to work. If
    not supported, the tool will execute synchronously and return immediately. *)
let call_tool_as_task t name ?arguments ?ttl ?meta () =
  let params =
    `Assoc
      ([ ("name", `String name) ]
      @ (match arguments with
        | Some args -> [ ("arguments", args) ]
        | None -> [])
      @ (match ttl with
        | Some t -> [ ("ttl", `Int t) ]
        | None -> [])
      @
      match meta with
      | Some m -> [ ("_meta", m) ]
      | None -> [])
  in
  Session.send_request t.session ~method_name:"tasks/call" ~params:(Some params)
    ~result_decoder:(fun json ->
      try Ok (create_task_result_of_yojson json)
      with exn -> Error (Exn.to_string exn))
    ()

(** Get the current status of a task by ID. *)
let get_task t task_id =
  let params = `Assoc [ ("taskId", `String task_id) ] in
  Session.send_request t.session ~method_name:"tasks/get" ~params:(Some params)
    ~result_decoder:(fun json ->
      try Ok (Polling.get_task_result_of_yojson json)
      with exn -> Error (Exn.to_string exn))
    ()

(** Get the result/payload of a completed task. *)
let get_task_result t task_id =
  let params = `Assoc [ ("taskId", `String task_id) ] in
  Session.send_request t.session ~method_name:"tasks/getResult"
    ~params:(Some params)
    ~result_decoder:(fun json -> Ok json)
    ()

(** List all tasks, optionally with cursor for pagination. *)
let list_tasks t ?cursor () =
  let params =
    match cursor with
    | Some c -> Some (`Assoc [ ("cursor", `String c) ])
    | None -> None
  in
  Session.send_request t.session ~method_name:"tasks/list" ~params
    ~result_decoder:(fun json ->
      try Ok (list_tasks_result_of_yojson json)
      with exn -> Error (Exn.to_string exn))
    ()

(** Cancel a running task. *)
let cancel_task t task_id =
  let params = `Assoc [ ("taskId", `String task_id) ] in
  Session.send_request t.session ~method_name:"tasks/cancel"
    ~params:(Some params)
    ~result_decoder:(fun json ->
      try Ok (Polling.get_task_result_of_yojson json)
      with exn -> Error (Exn.to_string exn))
    ()

(** Poll a task until it reaches a terminal state.

    Returns a pipe that emits task status updates until the task completes, is
    cancelled, or fails. *)
let poll_task t task_id =
  Polling.poll_until_terminal ~get_task:(get_task t) ~task_id ()
