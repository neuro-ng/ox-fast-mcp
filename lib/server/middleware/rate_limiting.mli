open Cohttp_lwt_unix

(** Rate limit error *)
exception Rate_limit_error of string

(** Token bucket rate limiter *)
module TokenBucketRateLimiter : sig
  type t

  val create : capacity:int -> refill_rate:float -> t
  val consume : ?amount:int -> t -> bool Lwt.t
end

(** Sliding window rate limiter *)
module SlidingWindowRateLimiter : sig
  type t

  val create : max_requests:int -> window_seconds:int -> t
  val is_allowed : t -> bool Lwt.t
end

(** Token bucket based rate limiting middleware *)
module RateLimitingMiddleware : sig
  type t

  val create : 
    ?max_requests_per_second:float ->
    ?burst_capacity:int option ->
    ?get_client_id:(Request.t -> string option) ->
    ?global_limit:bool ->
    unit -> t

  val middleware : 
    t -> 
    (Request.t -> Cohttp_lwt.Body.t -> (Response.t * Cohttp_lwt.Body.t) Lwt.t) ->
    Request.t -> 
    Cohttp_lwt.Body.t -> 
    (Response.t * Cohttp_lwt.Body.t) Lwt.t
end

(** Sliding window based rate limiting middleware *)
module SlidingWindowRateLimitingMiddleware : sig
  type t

  val create :
    max_requests:int ->
    window_minutes:int ->
    ?get_client_id:(Request.t -> string option) ->
    unit -> t

  val middleware :
    t ->
    (Request.t -> Cohttp_lwt.Body.t -> (Response.t * Cohttp_lwt.Body.t) Lwt.t) ->
    Request.t ->
    Cohttp_lwt.Body.t ->
    (Response.t * Cohttp_lwt.Body.t) Lwt.t
end 