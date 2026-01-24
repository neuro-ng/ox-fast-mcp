(** Rate limiting middleware for protecting MCP servers from excessive requests *)

open Core
open Async

(* Token bucket implementation *)
module TokenBucketRateLimiter = struct
  type t = {
    capacity : int;
    refill_rate : float;
    mutable tokens : float;
    mutable last_refill : float;
  }

  let create ~capacity ~refill_rate =
    {
      capacity;
      refill_rate;
      tokens = Float.of_int capacity;
      last_refill =
        Time_ns.to_span_since_epoch (Time_ns.now ()) |> Time_ns.Span.to_sec;
    }

  let consume ?(amount = 1) t =
    (* Note: Using mutable state without locking for simplicity. In production,
       consider using Async_kernel.Throttle or Mvar for true concurrency
       safety *)
    let now =
      Time_ns.to_span_since_epoch (Time_ns.now ()) |> Time_ns.Span.to_sec
    in
    let elapsed = now -. t.last_refill in

    (* Add tokens based on elapsed time *)
    t.tokens <-
      Float.min (Float.of_int t.capacity)
        (t.tokens +. (elapsed *. t.refill_rate));
    t.last_refill <- now;

    let allowed = Float.(t.tokens >= Float.of_int amount) in
    if allowed then t.tokens <- t.tokens -. Float.of_int amount;

    return (allowed, t.tokens)
end

(* Sliding window implementation *)
module SlidingWindowRateLimiter = struct
  type t = {
    max_requests : int;
    window_seconds : int;
    mutable requests : float Queue.t;
  }

  let create ~max_requests ~window_seconds =
    { max_requests; window_seconds; requests = Queue.create () }

  let is_allowed t =
    let now =
      Time_ns.to_span_since_epoch (Time_ns.now ()) |> Time_ns.Span.to_sec
    in
    let cutoff = now -. Float.of_int t.window_seconds in

    (* Remove old requests outside the window *)
    while
      (not (Queue.is_empty t.requests))
      && Float.(Queue.peek_exn t.requests < cutoff)
    do
      ignore (Queue.dequeue_exn t.requests)
    done;

    let current_count = Queue.length t.requests in
    let allowed = current_count < t.max_requests in
    if allowed then Queue.enqueue t.requests now;

    return (allowed, current_count)
end

(** Create a rate limit MCP error with retry-after information *)
let create_rate_limit_error ~message ~retry_after_seconds () =
  let data =
    `Assoc
      [
        ("retry_after_seconds", `Float retry_after_seconds);
        ("error_type", `String "RateLimitExceeded");
      ]
  in
  Mcp_shared.Exceptions.Mcp_error { code = -32000; message; data = Some data }

(** Calculate retry-after time based on current token count *)
let calculate_retry_after ~tokens ~refill_rate =
  if Float.(tokens >= 1.0) then 0.0
  else (* Time needed to refill 1 token *)
    (1.0 -. tokens) /. refill_rate

(* Rate limiting middleware using general middleware pattern *)
type rate_limit_config = {
  max_requests_per_second : float;
  burst_capacity : int;
  get_client_id : Middleware.context -> string option;
  global_limit : bool;
  limiters : (string, TokenBucketRateLimiter.t) Hashtbl.t;
  global_limiter : TokenBucketRateLimiter.t option;
}

let create ?(max_requests_per_second = 10.0) ?(burst_capacity = None)
    ?(get_client_id = fun _ -> None) ?(global_limit = false) () =
  let burst_capacity =
    Option.value burst_capacity
      ~default:(Int.of_float (max_requests_per_second *. 2.0))
  in
  {
    max_requests_per_second;
    burst_capacity;
    get_client_id;
    global_limit;
    limiters = Hashtbl.create (module String);
    global_limiter =
      (if global_limit then
         Some
           (TokenBucketRateLimiter.create ~capacity:burst_capacity
              ~refill_rate:max_requests_per_second)
       else None);
  }

let get_client_identifier config context =
  match config.get_client_id context with
  | Some id -> id
  | None -> "global"

let get_or_create_limiter config client_id =
  Hashtbl.find_or_add config.limiters client_id ~default:(fun () ->
      TokenBucketRateLimiter.create ~capacity:config.burst_capacity
        ~refill_rate:config.max_requests_per_second)

let check_rate_limit config context =
  if config.global_limit then
    match config.global_limiter with
    | Some limiter ->
      let%bind allowed, tokens = TokenBucketRateLimiter.consume limiter in
      return (allowed, tokens, config.max_requests_per_second)
    | None -> return (true, 0.0, 0.0)
  else
    let client_id = get_client_identifier config context in
    let limiter = get_or_create_limiter config client_id in
    let%bind allowed, tokens = TokenBucketRateLimiter.consume limiter in
    return (allowed, tokens, config.max_requests_per_second)

let on_message config context call_next =
  let%bind allowed, tokens, refill_rate = check_rate_limit config context in
  if allowed then call_next context
  else
    let retry_after = calculate_retry_after ~tokens ~refill_rate in
    let message =
      sprintf "Rate limit exceeded. Retry after %.2f seconds" retry_after
    in
    raise (create_rate_limit_error ~message ~retry_after_seconds:retry_after ())

(** Rate limiting middleware module implementing Middleware.S *)
module RateLimiting : Middleware.S = struct
  type t = rate_limit_config

  let create () =
    {
      max_requests_per_second = 10.0;
      burst_capacity = 20;
      get_client_id = (fun _ -> None);
      global_limit = false;
      limiters = Hashtbl.create (module String);
      global_limiter = None;
    }

  let on_message = on_message
  let on_request config context call_next = on_message config context call_next

  let on_notification config context call_next =
    on_message config context call_next

  let on_call_tool config context call_next =
    on_message config context call_next

  let on_read_resource config context call_next =
    on_message config context call_next

  let on_get_prompt config context call_next =
    on_message config context call_next

  let on_list_tools config context call_next =
    on_message config context call_next

  let on_list_resources config context call_next =
    on_message config context call_next

  let on_list_resource_templates config context call_next =
    on_message config context call_next

  let on_list_prompts config context call_next =
    on_message config context call_next

  let dispatch_handler _config _context call_next = return call_next

  let call config context call_next =
    let%bind handler = dispatch_handler config context call_next in
    on_message config context handler
end
