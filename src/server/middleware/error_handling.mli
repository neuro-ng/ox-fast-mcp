(** Error handling middleware for consistent error responses and tracking *)

open Core
open Async
open Middleware

(* Simple logger type for middleware *)
type logger_t = {
  name: string;
  level: [`Debug | `Info | `Warning | `Error | `Critical];
}

type error_callback = exn -> context -> unit
(** Type for error callback functions *)

type t = {
  logger : logger_t;
  include_traceback : bool;
  error_callback : error_callback option;
  transform_errors : bool;
  mutable error_counts : int String.Map.t;
}
(** Error handling middleware type *)

val create :
  ?logger:logger_t ->
  ?include_traceback:bool ->
  ?error_callback:error_callback option ->
  ?transform_errors:bool ->
  unit ->
  t
(** Create error handling middleware
    @param logger
      Logger instance for error logging. If None, uses 'fastmcp.errors'
    @param include_traceback Whether to include full traceback in error logs
    @param error_callback Optional callback function called for each error
    @param transform_errors Whether to transform non-MCP errors to McpError *)

val log_error : t -> exn -> context -> unit
(** Log error with appropriate detail level *)

val transform_error : t -> exn -> exn
(** Transform non-MCP errors to proper MCP errors *)

val on_message : t -> context -> 'a call_next -> 'a Deferred.t
(** Handle errors for all messages *)

val get_error_stats : t -> int String.Map.t
(** Get error statistics for monitoring *)

(** Retry middleware for handling transient failures *)
module Retry : sig
  type t = {
    max_retries : int;
    base_delay : float;
    max_delay : float;
    backoff_multiplier : float;
    retry_exceptions : (exn -> bool) list;
    logger : logger_t;
  }

  val create :
    ?max_retries:int ->
    ?base_delay:float ->
    ?max_delay:float ->
    ?backoff_multiplier:float ->
    ?retry_exceptions:(exn -> bool) list ->
    ?logger:logger_t ->
    unit ->
    t
  (** Create retry middleware
      @param max_retries Maximum number of retry attempts
      @param base_delay Initial delay between retries in seconds
      @param max_delay Maximum delay between retries in seconds
      @param backoff_multiplier Multiplier for exponential backoff
      @param retry_exceptions
        List of predicates that determine if an exception should trigger retries
      @param logger Logger for retry attempts *)

  val should_retry : t -> exn -> bool
  (** Determine if an error should trigger a retry *)

  val calculate_delay : t -> int -> float
  (** Calculate delay for the given attempt number *)

  val on_request : t -> context -> 'a call_next -> 'a Deferred.t
  (** Implement retry logic for requests *)
end
