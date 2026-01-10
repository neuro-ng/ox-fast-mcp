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
    support is added **)

let poll_until_terminal ~get_task ~task_id ?(default_interval_ms = 500) () =
  let reader, writer = Pipe.create () in

  let rec poll_loop () =
    let%bind status = get_task task_id in
    let%bind () = Pipe.write writer status in

    (* Check if terminal using helpers module *)
    if Helpers.is_terminal status.status then return ()
    else
      let interval_ms =
        match status.pollInterval with
        | Some interval -> interval
        | None -> default_interval_ms
      in
      let%bind () = Clock_ns.after (Time_ns.Span.of_int_ms interval_ms) in
      poll_loop ()
  in

  (* Start polling in background and close pipe when done *)
  don't_wait_for
    (let%bind () = poll_loop () in
     Pipe.close writer;
     return ());

  reader
