open Core
open Async
open Middleware
open Ox_fast_mcp.Mcp.Mcp_error

type error_callback = exn -> context -> unit

type t = {
  logger : Logger.t;
  include_traceback : bool;
  error_callback : error_callback option;
  transform_errors : bool;
  error_counts : int String.Map.t;
}

let create ?(logger = Logger.create "fastmcp.errors")
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
  let error_type =
    Exn.to_string_hum error |> String.split ~on:' ' |> List.hd_exn
  in
  let method_ = Option.value context.method_ ~default:"unknown" in
  let error_key = sprintf "%s:%s" error_type method_ in

  (* Update error counts *)
  t.error_counts <-
    String.Map.update t.error_counts error_key ~f:(function
      | None -> 1
      | Some count -> count + 1);

  let base_message =
    sprintf "Error in %s: %s: %s" method_ error_type (Exn.to_string error)
  in

  if t.include_traceback then
    Logger.error t.logger (fun () ->
        sprintf "%s\n%s" base_message (Backtrace.to_string (Backtrace.get ())))
  else Logger.error t.logger (fun () -> base_message);

  (* Call error callback if provided *)
  match t.error_callback with
  | Some callback -> (
    try callback error context
    with exn ->
      Logger.error t.logger (fun () ->
          sprintf "Error in error callback: %s" (Exn.to_string exn)))
  | None -> ()

let transform_error t error =
  if not t.transform_errors then error
  else
    match error with
    | Mcp_error _ as e -> e
    | Invalid_argument msg | Arg.Bad msg ->
      Mcp_error
        {
          code = -32602;
          message = sprintf "Invalid params: %s" msg;
          details = None;
        }
    | Not_found | Caml.Not_found | Not_found_s _ ->
      Mcp_error
        {
          code = -32001;
          message = sprintf "Resource not found: %s" (Exn.to_string error);
          details = None;
        }
    | Unix.Unix_error (EACCES, _, _) ->
      Mcp_error
        {
          code = -32000;
          message = sprintf "Permission denied: %s" (Exn.to_string error);
          details = None;
        }
    | Async_unix.Timeout ->
      Mcp_error
        {
          code = -32000;
          message = sprintf "Request timeout: %s" (Exn.to_string error);
          details = None;
        }
    | exn ->
      Mcp_error
        {
          code = -32603;
          message = sprintf "Internal error: %s" (Exn.to_string exn);
          details = None;
        }

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
    logger : Logger.t;
  }

  let create ?(max_retries = 3) ?(base_delay = 1.0) ?(max_delay = 60.0)
      ?(backoff_multiplier = 2.0)
      ?(retry_exceptions =
        [
          (function
          | Unix.Unix_error (ECONNREFUSED, _, _)
          | Unix.Unix_error (ECONNRESET, _, _)
          | Async_unix.Timeout -> true
          | _ -> false);
        ]) ?(logger = Logger.create "fastmcp.retry") () =
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
          Logger.warning t.logger (fun () ->
              sprintf
                "Request %s failed (attempt %d/%d): %s: %s. Retrying in \
                 %.1fs..."
                (Option.value context.method_ ~default:"unknown")
                (attempt + 1) (t.max_retries + 1) (Exn.to_string_hum error)
                (Exn.to_string error) delay);
          Clock.after (Time.Span.of_sec delay) >>= fun () ->
          try_request (attempt + 1)
    in
    try_request 0 >>= function
    | Ok result -> return result
    | Error exn -> raise exn
end
