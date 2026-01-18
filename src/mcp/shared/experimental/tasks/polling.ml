(** Shared polling utilities for task operations.

    This module provides generic polling logic that works for both client→server
    and server→client task polling.

    WARNING: These APIs are experimental and may change without notice. *)

open Core
open Async
open Ppx_yojson_conv_lib.Yojson_conv.Primitives
module Types = Mcp.Types

(* Time_ns converters for yojson *)
type time_ns = Time_ns.t

let yojson_of_time_ns t = `Float (Time_ns.to_span_since_epoch t |> Time_ns.Span.to_sec)
let time_ns_of_yojson = function
  | `Float f -> Time_ns.of_span_since_epoch (Time_ns.Span.of_sec f)
  | `Int i -> Time_ns.of_span_since_epoch (Time_ns.Span.of_sec (Float.of_int i))
  | json -> raise (Yojson.Safe.Util.Type_error ("Expected float or int for time", json))

let sexp_of_time_ns = Time_ns.sexp_of_t
let time_ns_of_sexp = Time_ns.t_of_sexp
let compare_time_ns = Time_ns.compare

type get_task_result = {
  taskId : string;
  status : string;
      (* "working" | "input_required" | "completed" | "failed" | "cancelled" *)
  pollInterval : int option; [@yojson.option]
  createdAt : time_ns;
  lastUpdatedAt : time_ns;
}
[@@deriving sexp, compare, yojson]
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
