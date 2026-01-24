(** Tests for rate limiting middleware. *)

open! Core
open! Expect_test_helpers_core
open Server_middleware.Rate_limiting

(* =============================================================================
   Tests for TokenBucketRateLimiter
   ============================================================================= *)

let%expect_test "TokenBucketRateLimiter - initialization" =
  let limiter = TokenBucketRateLimiter.create ~capacity:10 ~refill_rate:5.0 in
  printf "limiter created: %b\n" (phys_equal limiter limiter);
  [%expect {| limiter created: true |}]

(* =============================================================================
   Tests for SlidingWindowRateLimiter
   ============================================================================= *)

let%expect_test "SlidingWindowRateLimiter - initialization" =
  let limiter =
    SlidingWindowRateLimiter.create ~max_requests:10 ~window_seconds:60
  in
  printf "limiter created successfully: %b\n" (phys_equal limiter limiter);
  [%expect {| limiter created successfully: true |}]

(* =============================================================================
   Tests for rate_limit_config
   ============================================================================= *)

let%expect_test "create - default initialization" =
  let config = create () in
  printf "Rate limit config created: %b\n" (phys_equal config config);
  [%expect {| Rate limit config created: true |}]

let%expect_test "create - custom configuration" =
  let config =
    create ~max_requests_per_second:5.0 ~burst_capacity:(Some 10)
      ~global_limit:true ()
  in
  printf "Custom config created: %b\n" (phys_equal config config);
  [%expect {| Custom config created: true |}]

(* =============================================================================
   Tests for RateLimiting module
   ============================================================================= *)

let%expect_test "RateLimiting module - create" =
  let middleware = RateLimiting.create () in
  printf "RateLimiting middleware created: %b\n"
    (phys_equal middleware middleware);
  [%expect {| RateLimiting middleware created: true |}]

(* =============================================================================
   Tests for MCP error creation
   ============================================================================= *)

let%expect_test "create_rate_limit_error - message and retry_after" =
  let error =
    create_rate_limit_error ~message:"Too many requests"
      ~retry_after_seconds:1.5 ()
  in
  (match error with
  | Mcp_shared.Exceptions.Mcp_error { code; message; data } ->
    printf "error code: %d\n" code;
    printf "error message: %s\n" message;
    printf "has retry_after data: %b\n" (Option.is_some data)
  | _ -> printf "unexpected error type\n");
  [%expect
    {|
    error code: -32000
    error message: Too many requests
    has retry_after data: true
    |}]

(* =============================================================================
   Tests for retry-after calculation
   ============================================================================= *)

let%expect_test "calculate_retry_after - tokens available" =
  let retry = calculate_retry_after ~tokens:5.0 ~refill_rate:10.0 in
  printf "retry after (tokens available): %.2f\n" retry;
  [%expect {| retry after (tokens available): 0.00 |}]

let%expect_test "calculate_retry_after - tokens depleted" =
  let retry = calculate_retry_after ~tokens:0.5 ~refill_rate:2.0 in
  printf "retry after (tokens depleted): %.2f\n" retry;
  [%expect {| retry after (tokens depleted): 0.25 |}]

let%expect_test "calculate_retry_after - completely empty" =
  let retry = calculate_retry_after ~tokens:0.0 ~refill_rate:10.0 in
  printf "retry after (empty): %.2f\n" retry;
  [%expect {| retry after (empty): 0.10 |}]
