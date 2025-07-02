open Core
open Lwt.Syntax
open Cohttp_lwt_unix

(* Rate limit error *)
exception Rate_limit_error of string

(* Token bucket implementation *)
module TokenBucketRateLimiter = struct
  type t = {
    capacity : int;
    refill_rate : float;
    mutable tokens : float;
    mutable last_refill : float;
    mutex : Lwt_mutex.t;
  }

  let create ~capacity ~refill_rate = {
    capacity;
    refill_rate;
    tokens = float_of_int capacity;
    last_refill = Unix.gettimeofday ();
    mutex = Lwt_mutex.create ();
  }

  let consume ?(amount = 1) t =
    Lwt_mutex.with_lock t.mutex (fun () ->
      let now = Unix.gettimeofday () in
      let elapsed = now -. t.last_refill in
      
      (* Add tokens based on elapsed time *)
      t.tokens <- Float.min (float_of_int t.capacity) (t.tokens +. elapsed *. t.refill_rate);
      t.last_refill <- now;

      if t.tokens >= float_of_int amount then (
        t.tokens <- t.tokens -. float_of_int amount;
        Lwt.return true
      ) else
        Lwt.return false
    )
end

(* Sliding window implementation *)
module SlidingWindowRateLimiter = struct
  type t = {
    max_requests : int;
    window_seconds : int;
    mutable requests : float Queue.t;
    mutex : Lwt_mutex.t;
  }

  let create ~max_requests ~window_seconds = {
    max_requests;
    window_seconds;
    requests = Queue.create ();
    mutex = Lwt_mutex.create ();
  }

  let is_allowed t =
    Lwt_mutex.with_lock t.mutex (fun () ->
      let now = Unix.gettimeofday () in
      let cutoff = now -. float_of_int t.window_seconds in
      
      (* Remove old requests outside the window *)
      while not (Queue.is_empty t.requests) && Queue.peek_exn t.requests < cutoff do
        ignore (Queue.dequeue_exn t.requests)
      done;

      if Queue.length t.requests < t.max_requests then (
        Queue.enqueue t.requests now;
        Lwt.return true
      ) else
        Lwt.return false
    )
end

(* Rate limiting middleware *)
module RateLimitingMiddleware = struct
  type t = {
    max_requests_per_second : float;
    burst_capacity : int;
    get_client_id : Request.t -> string option;
    global_limit : bool;
    limiters : (string, TokenBucketRateLimiter.t) Hashtbl.t;
    global_limiter : TokenBucketRateLimiter.t option;
  }

  let create ?(max_requests_per_second = 10.0) ?(burst_capacity = None) 
      ?(get_client_id = None) ?(global_limit = false) () =
    let burst_capacity = Option.value burst_capacity 
      ~default:(Int.of_float (max_requests_per_second *. 2.0)) in
    {
      max_requests_per_second;
      burst_capacity;
      get_client_id = Option.value get_client_id ~default:(fun _ -> None);
      global_limit;
      limiters = Hashtbl.create (module String);
      global_limiter = if global_limit then 
        Some (TokenBucketRateLimiter.create ~capacity:burst_capacity ~refill_rate:max_requests_per_second)
      else None;
    }

  let get_client_identifier t req =
    match t.get_client_id req with
    | Some id -> id
    | None -> "global"

  let get_or_create_limiter t client_id =
    Hashtbl.find_or_add t.limiters client_id ~default:(fun () ->
      TokenBucketRateLimiter.create 
        ~capacity:t.burst_capacity 
        ~refill_rate:t.max_requests_per_second)

  let middleware t handler req body =
    let check_rate_limit () =
      if t.global_limit then
        match t.global_limiter with
        | Some limiter -> TokenBucketRateLimiter.consume limiter
        | None -> Lwt.return true
      else
        let client_id = get_client_identifier t req in
        let limiter = get_or_create_limiter t client_id in
        TokenBucketRateLimiter.consume limiter
    in
    
    let* allowed = check_rate_limit () in
    if allowed then
      handler req body
    else
      let message = "Rate limit exceeded" in
      Lwt.fail (Rate_limit_error message)
end

(* Sliding window middleware *)
module SlidingWindowRateLimitingMiddleware = struct
  type t = {
    max_requests : int;
    window_minutes : int;
    get_client_id : Request.t -> string option;
    limiters : (string, SlidingWindowRateLimiter.t) Hashtbl.t;
  }

  let create ~max_requests ~window_minutes ?(get_client_id = None) () = {
    max_requests;
    window_minutes;
    get_client_id = Option.value get_client_id ~default:(fun _ -> None);
    limiters = Hashtbl.create (module String);
  }

  let get_client_identifier t req =
    match t.get_client_id req with
    | Some id -> id
    | None -> "global"

  let get_or_create_limiter t client_id =
    Hashtbl.find_or_add t.limiters client_id ~default:(fun () ->
      SlidingWindowRateLimiter.create 
        ~max_requests:t.max_requests 
        ~window_seconds:(t.window_minutes * 60))

  let middleware t handler req body =
    let client_id = get_client_identifier t req in
    let limiter = get_or_create_limiter t client_id in
    
    let* allowed = SlidingWindowRateLimiter.is_allowed limiter in
    if allowed then
      handler req body
    else
      let message = Printf.sprintf 
        "Rate limit exceeded: %d requests per %d minutes for client: %s"
        t.max_requests t.window_minutes client_id
      in
      Lwt.fail (Rate_limit_error message)
end 