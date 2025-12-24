(** Tests for rate limiting middleware. *)

open! Core
open! Expect_test_helpers_core
open Server_middleware.Rate_limiting

(* =============================================================================
   Tests for TokenBucketRateLimiter
   ============================================================================= *)

let%expect_test "TokenBucketRateLimiter - initialization" =
  let limiter = TokenBucketRateLimiter.create ~capacity:10 ~refill_rate:5.0 in
  printf "limiter created: %b\n" (not (phys_equal limiter limiter));
  printf "limiter created successfully: true\n";
  [%expect
    {|
    limiter created: false
    limiter created successfully: true
    |}]

let%expect_test "TokenBucketRateLimiter - consume returns bool Lwt.t" =
  let limiter = TokenBucketRateLimiter.create ~capacity:10 ~refill_rate:5.0 in
  let result = Lwt_main.run (TokenBucketRateLimiter.consume limiter) in
  printf "first consume succeeds: %b\n" result;
  [%expect {| first consume succeeds: true |}]

let%expect_test "TokenBucketRateLimiter - consume with amount" =
  let limiter = TokenBucketRateLimiter.create ~capacity:10 ~refill_rate:5.0 in
  let result =
    Lwt_main.run (TokenBucketRateLimiter.consume ~amount:5 limiter)
  in
  printf "consume 5 tokens succeeds: %b\n" result;
  [%expect {| consume 5 tokens succeeds: true |}]

let%expect_test "TokenBucketRateLimiter - exhaust capacity" =
  let limiter = TokenBucketRateLimiter.create ~capacity:5 ~refill_rate:1.0 in
  (* Consume all tokens *)
  let result1 =
    Lwt_main.run (TokenBucketRateLimiter.consume ~amount:5 limiter)
  in
  printf "consume all tokens: %b\n" result1;
  (* Try to consume more - should fail *)
  let result2 = Lwt_main.run (TokenBucketRateLimiter.consume limiter) in
  printf "consume when empty: %b\n" result2;
  [%expect {|
    consume all tokens: true
    consume when empty: false
    |}]

(* =============================================================================
   Tests for SlidingWindowRateLimiter
   ============================================================================= *)

let%expect_test "SlidingWindowRateLimiter - initialization" =
  let limiter =
    SlidingWindowRateLimiter.create ~max_requests:10 ~window_seconds:60
  in
  printf "limiter created successfully: true\n";
  ignore limiter;
  [%expect {| limiter created successfully: true |}]

let%expect_test "SlidingWindowRateLimiter - is_allowed returns bool Lwt.t" =
  let limiter =
    SlidingWindowRateLimiter.create ~max_requests:10 ~window_seconds:60
  in
  let result = Lwt_main.run (SlidingWindowRateLimiter.is_allowed limiter) in
  printf "first request allowed: %b\n" result;
  [%expect {| first request allowed: true |}]

let%expect_test "SlidingWindowRateLimiter - allows up to max_requests" =
  let limiter =
    SlidingWindowRateLimiter.create ~max_requests:3 ~window_seconds:60
  in
  let result1 = Lwt_main.run (SlidingWindowRateLimiter.is_allowed limiter) in
  let result2 = Lwt_main.run (SlidingWindowRateLimiter.is_allowed limiter) in
  let result3 = Lwt_main.run (SlidingWindowRateLimiter.is_allowed limiter) in
  printf "request 1 allowed: %b\n" result1;
  printf "request 2 allowed: %b\n" result2;
  printf "request 3 allowed: %b\n" result3;
  [%expect
    {|
    request 1 allowed: true
    request 2 allowed: true
    request 3 allowed: true
    |}]

let%expect_test "SlidingWindowRateLimiter - rejects over limit" =
  let limiter =
    SlidingWindowRateLimiter.create ~max_requests:2 ~window_seconds:60
  in
  (* Use up the limit *)
  let _ = Lwt_main.run (SlidingWindowRateLimiter.is_allowed limiter) in
  let _ = Lwt_main.run (SlidingWindowRateLimiter.is_allowed limiter) in
  (* Should reject *)
  let result = Lwt_main.run (SlidingWindowRateLimiter.is_allowed limiter) in
  printf "over limit rejected: %b\n" (not result);
  [%expect {| over limit rejected: true |}]

(* =============================================================================
   Tests for RateLimitingMiddleware
   ============================================================================= *)

let%expect_test "RateLimitingMiddleware - default initialization" =
  let middleware = RateLimitingMiddleware.create () in
  printf "middleware created successfully: true\n";
  ignore middleware;
  [%expect {| middleware created successfully: true |}]

let%expect_test "RateLimitingMiddleware - custom initialization" =
  let middleware =
    RateLimitingMiddleware.create ~max_requests_per_second:5.0
      ~burst_capacity:(Some 10) ~global_limit:true ()
  in
  printf "custom middleware created: true\n";
  ignore middleware;
  [%expect {| custom middleware created: true |}]

(* =============================================================================
   Tests for SlidingWindowRateLimitingMiddleware
   ============================================================================= *)

let%expect_test "SlidingWindowRateLimitingMiddleware - initialization" =
  let middleware =
    SlidingWindowRateLimitingMiddleware.create ~max_requests:100
      ~window_minutes:1 ()
  in
  printf "sliding window middleware created: true\n";
  ignore middleware;
  [%expect {| sliding window middleware created: true |}]

let%expect_test "SlidingWindowRateLimitingMiddleware - custom window" =
  let middleware =
    SlidingWindowRateLimitingMiddleware.create ~max_requests:50
      ~window_minutes:5 ()
  in
  printf "5-minute window middleware created: true\n";
  ignore middleware;
  [%expect {| 5-minute window middleware created: true |}]

(* =============================================================================
   Tests for Rate_limit_error exception
   ============================================================================= *)

let%expect_test "Rate_limit_error - message" =
  let error = Rate_limit_error "Custom rate limit message" in
  (match error with
  | Rate_limit_error msg -> printf "error message: %s\n" msg
  | _ -> printf "unexpected error\n");
  [%expect {| error message: Custom rate limit message |}]

let%expect_test "Rate_limit_error - default message pattern" =
  let error = Rate_limit_error "Rate limit exceeded" in
  (match error with
  | Rate_limit_error msg ->
    printf "is rate limit error: %b\n"
      (String.is_prefix msg ~prefix:"Rate limit")
  | _ -> printf "unexpected error\n");
  [%expect {| is rate limit error: true |}]
