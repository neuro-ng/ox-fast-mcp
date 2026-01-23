(** Rate limiting middleware for protecting MCP servers from excessive requests *)

open Core
open Async
open Middleware

(** Token bucket rate limiter *)
module TokenBucketRateLimiter : sig
  type t

  val create : capacity:int -> refill_rate:float -> t
  val consume : ?amount:int -> t -> (bool * float) Deferred.t
end

(** Sliding window rate limiter *)
module SlidingWindowRateLimiter : sig
  type t

  val create : max_requests:int -> window_seconds:int -> t
  val is_allowed : t -> (bool * int) Deferred.t
end

(** Rate limiting configuration *)
type rate_limit_config = {
  max_requests_per_second : float;
  burst_capacity : int;
  get_client_id : context -> string option;
  global_limit : bool;
  limiters : (string, TokenBucketRateLimiter.t) Hashtbl.t;
  global_limiter : TokenBucketRateLimiter.t option;
}

val create :
  ?max_requests_per_second:float ->
  ?burst_capacity:int option ->
  ?get_client_id:(context -> string option) ->
  ?global_limit:bool ->
  unit ->
  rate_limit_config
(** Create a new rate limiting configuration.
    
    @param max_requests_per_second Maximum requests per second per client (default: 10.0)
    @param burst_capacity Maximum burst size (default: 2x max_requests_per_second)
    @param get_client_id Function to extract client ID from context (default: None -> "global")
    @param global_limit Apply rate limit globally instead of per-client (default: false)
*)

val create_rate_limit_error :
  message:string -> retry_after_seconds:float -> unit -> exn
(** Create an MCP error for rate limiting with retry-after information *)

val calculate_retry_after : tokens:float -> refill_rate:float -> float
(** Calculate retry-after time in seconds based on current token count *)

val check_rate_limit :
  rate_limit_config -> context -> (bool * float * float) Deferred.t
(** Check if request is allowed under rate limit.
    Returns (allowed, current_tokens, refill_rate) *)

val on_message : rate_limit_config -> context -> 'a call_next -> 'a Deferred.t
(** Apply rate limiting to any message *)

(** Rate limiting middleware module implementing Middleware.S *)
module RateLimiting : Middleware.S
