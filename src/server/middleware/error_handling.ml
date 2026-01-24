(** Error handling middleware for consistent error responses and tracking *)

open Core
open Async
open Middleware
open Logging
module Exceptions = Ox_fast_mcp.Exceptions

type error_callback = exn -> context -> unit
(** Type for error callback functions *)

type t = {
  middleware_logger : Logger.t;
  include_traceback : bool;
  error_callback : error_callback option;
  transform_errors : bool;
  mutable error_counts : int String.Map.t;
}
(** Error handling middleware type *)

let create ?(middleware_logger = Logger.get_logger "OxFastMCP.Errors")
    ?(include_traceback = false) ?(error_callback = None)
    ?(transform_errors = true) () =
  (* Enable backtrace recording if tracebacks are requested *)
  if include_traceback then Printexc.record_backtrace true;
  {
    middleware_logger;
    include_traceback;
    error_callback;
    transform_errors;
    error_counts = String.Map.empty;
  }

let log_error t error context =
  let error_type = Exn.to_string error |> String.split ~on:' ' |> List.hd_exn in
  let method_ = Option.value context.method_ ~default:"unknown" in
  let error_key = sprintf "%s:%s" error_type method_ in

  (* Update error counts *)
  let current_count =
    Map.find t.error_counts error_key |> Option.value ~default:0
  in
  t.error_counts <-
    Map.set t.error_counts ~key:error_key ~data:(current_count + 1);

  (* Get backtrace if available *)
  let backtrace =
    if Printexc.backtrace_status () then Printexc.get_backtrace () else ""
  in

  (* Log error using the logging module *)
  let base_message =
    sprintf "Error in %s: %s: %s" method_ error_type (Exn.to_string error)
  in

  if t.include_traceback && String.length backtrace > 0 then
    Logger.error t.middleware_logger
      (sprintf "%s\nTraceback:\n%s" base_message backtrace)
  else Logger.error t.middleware_logger base_message;

  (* Call error callback if provided *)
  match t.error_callback with
  | Some callback -> (
    try callback error context
    with exn ->
      Logger.warning t.middleware_logger
        (sprintf "Error callback failed: %s" (Exn.to_string exn)))
  | None -> ()

let transform_error t error =
  (* Helper to create structured error data *)
  let create_error_data ~code ~message ~error_type ~original_error () =
    let data_fields =
      [
        ("error_type", `String error_type);
        ("original_error", `String original_error);
      ]
    in
    let data_with_backtrace =
      if t.include_traceback && Printexc.backtrace_status () then
        let backtrace = Printexc.get_backtrace () in
        if String.length backtrace > 0 then
          ("backtrace", `String backtrace) :: data_fields
        else data_fields
      else data_fields
    in
    { Mcp.Types.code; message; data = Some (`Assoc data_with_backtrace) }
  in

  (* Check if already an MCP error *)
  match error with
  | Mcp_shared.Exceptions.Mcp_error _ -> error
  | _ when not t.transform_errors -> error
  | Invalid_argument msg ->
    (* ValueError/TypeError equivalent -> Invalid params *)
    Mcp_shared.Exceptions.Mcp_error
      (create_error_data ~code:(-32602)
         ~message:(sprintf "Invalid params: %s" msg)
         ~error_type:"Invalid_argument" ~original_error:msg ())
  | Exceptions.Not_found_error { message; _ } ->
    (* NotFoundError equivalent -> Resource not found *)
    Mcp_shared.Exceptions.Mcp_error
      (create_error_data ~code:(-32001)
         ~message:(sprintf "Resource not found: %s" message)
         ~error_type:"Not_found_error" ~original_error:message ())
  | Not_found_s sexp ->
    (* OCaml Not_found with sexp -> Resource not found *)
    let sexp_str = Sexp.to_string sexp in
    Mcp_shared.Exceptions.Mcp_error
      (create_error_data ~code:(-32001)
         ~message:(sprintf "Resource not found: %s" sexp_str)
         ~error_type:"Not_found_s" ~original_error:sexp_str ())
  | Core_unix.Unix_error (Core_unix.ENOENT, _, path) ->
    (* FileNotFoundError equivalent -> Resource not found *)
    Mcp_shared.Exceptions.Mcp_error
      (create_error_data ~code:(-32001)
         ~message:(sprintf "Resource not found: %s" path)
         ~error_type:"FileNotFoundError" ~original_error:path ())
  | Core_unix.Unix_error (Core_unix.EACCES, _, path) ->
    (* PermissionError equivalent -> Permission denied *)
    Mcp_shared.Exceptions.Mcp_error
      (create_error_data ~code:(-32000)
         ~message:(sprintf "Permission denied: %s" path)
         ~error_type:"PermissionError" ~original_error:path ())
  | Core_unix.Unix_error (Core_unix.ETIMEDOUT, _, _) ->
    (* TimeoutError equivalent -> Request timeout *)
    Mcp_shared.Exceptions.Mcp_error
      (create_error_data ~code:(-32000) ~message:"Request timeout"
         ~error_type:"TimeoutError" ~original_error:"Request timed out" ())
  | exn ->
    (* Generic error -> Internal error *)
    let exn_str = Exn.to_string exn in
    Mcp_shared.Exceptions.Mcp_error
      (create_error_data ~code:(-32603)
         ~message:(sprintf "Internal error: %s" exn_str)
         ~error_type:"InternalError" ~original_error:exn_str ())

let on_message t context call_next =
  Monitor.try_with (fun () -> call_next context) >>= function
  | Ok result -> return result
  | Error exn ->
    log_error t exn context;
    let transformed_error = transform_error t exn in
    raise transformed_error

let get_error_stats t = t.error_counts

(** Retry middleware for handling transient failures *)
module Retry = struct
  type t = {
    max_retries : int;
    base_delay : float;
    max_delay : float;
    backoff_multiplier : float;
    retry_exceptions : (exn -> bool) list;
    retry_logger : Logger.t;
  }

  let create ?(max_retries = 3) ?(base_delay = 1.0) ?(max_delay = 60.0)
      ?(backoff_multiplier = 2.0)
      ?(retry_exceptions =
        [
          (function
          | Unix.Unix_error (ECONNREFUSED, _, _)
          | Unix.Unix_error (ECONNRESET, _, _) -> true
          | _ -> false);
        ]) ?(retry_logger = Logger.get_logger "OxFastMCP.Retry") () =
    {
      max_retries;
      base_delay;
      max_delay;
      backoff_multiplier;
      retry_exceptions;
      retry_logger;
    }

  let should_retry t error =
    List.exists t.retry_exceptions ~f:(fun predicate -> predicate error)

  let calculate_delay t attempt =
    let delay =
      t.base_delay *. (t.backoff_multiplier ** Float.of_int attempt)
    in
    Float.min delay t.max_delay

  let on_request t context call_next =
    let rec try_request attempt =
      Monitor.try_with (fun () -> call_next context) >>= function
      | Ok result -> return result
      | Error error ->
        if attempt >= t.max_retries || not (should_retry t error) then
          raise error
        else
          let delay = calculate_delay t attempt in
          Logger.warning t.retry_logger
            (sprintf "Retry attempt %d after error: %s (delay: %.2fs)"
               (attempt + 1) (Exn.to_string error) delay);
          Clock.after (sec delay) >>= fun () -> try_request (attempt + 1)
    in
    try_request 0
end
