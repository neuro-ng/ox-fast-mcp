open Core
open Async
open Middleware
open Mcp.Types

(* Simple logger type for middleware *)
type logger_t = {
  name : string;
  level : [ `Debug | `Info | `Warning | `Error | `Critical ];
}

type error_callback = exn -> context -> unit

type t = {
  logger : logger_t;
  include_traceback : bool;
  error_callback : error_callback option;
  transform_errors : bool;
  mutable error_counts : int String.Map.t;
}

let create ?(logger = { name = "fastmcp.errors"; level = `Info })
    ?(include_traceback = false) ?(error_callback = None)
    ?(transform_errors = true) () =
  {
    logger;
    include_traceback;
    error_callback;
    transform_errors;
    error_counts = String.Map.empty;
  }

let log_error t error context =
  let error_type = Exn.to_string error |> String.split ~on:' ' |> List.hd_exn in
  let method_ = "unknown" in
  let error_key = sprintf "%s:%s" error_type method_ in

  (* Update error counts *)
  let current_count =
    Map.find t.error_counts error_key |> Option.value ~default:0
  in
  t.error_counts <-
    Map.set t.error_counts ~key:error_key ~data:(current_count + 1);

  let base_message =
    sprintf "Error in %s: %s: %s" method_ error_type (Exn.to_string error)
  in

  (* Log error - simplified for now *)
  ignore (t.logger.name, base_message, t.include_traceback);

  (* Call error callback if provided *)
  match t.error_callback with
  | Some callback -> (
    try callback error context with exn -> ignore (t.logger.name, exn))
  | None -> ()

let transform_error t error =
  (* Simplified - just return the original error for now *)
  ignore t;
  error

let on_message t context call_next =
  Monitor.try_with (fun () -> call_next context) >>= function
  | Ok result -> return result
  | Error exn ->
    log_error t exn context;
    let transformed_error = transform_error t exn in
    raise transformed_error

let get_error_stats t = t.error_counts

(* Retry middleware *)
module Retry = struct
  type t = {
    max_retries : int;
    base_delay : float;
    max_delay : float;
    backoff_multiplier : float;
    retry_exceptions : (exn -> bool) list;
    logger : logger_t;
  }

  let create ?(max_retries = 3) ?(base_delay = 1.0) ?(max_delay = 60.0)
      ?(backoff_multiplier = 2.0)
      ?(retry_exceptions =
        [
          (function
          | Unix.Unix_error (ECONNREFUSED, _, _)
          | Unix.Unix_error (ECONNRESET, _, _) -> true
          (* | Async_unix.Timeout -> true (* Timeout not available *) *)
          | _ -> false);
        ]) ?(logger = { name = "fastmcp.retry"; level = `Info }) () =
    {
      max_retries;
      base_delay;
      max_delay;
      backoff_multiplier;
      retry_exceptions;
      logger;
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
      | Error error as e ->
        if attempt = t.max_retries || not (should_retry t error) then
          Deferred.return e
        else
          let delay = calculate_delay t attempt in
          (* Log retry warning - simplified for now *)
          ignore (t.logger.name, error, attempt, delay);
          Clock.after (sec delay) >>= fun () -> try_request (attempt + 1)
    in
    try_request 0 >>= function
    | Ok result -> return result
    | Error exn -> raise exn
end
