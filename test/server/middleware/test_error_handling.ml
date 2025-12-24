(** Tests for error handling middleware. *)

open! Core
open! Expect_test_helpers_core
open Server_middleware.Error_handling

(* =============================================================================
   Tests for Error_handling - Default Initialization
   ============================================================================= *)

let%expect_test "Error_handling - default initialization" =
  let middleware = create () in
  printf "include_traceback: %b\n" middleware.include_traceback;
  printf "transform_errors: %b\n" middleware.transform_errors;
  printf "error_callback present: %b\n"
    (Option.is_some middleware.error_callback);
  printf "error_counts empty: %b\n" (Map.is_empty middleware.error_counts);
  [%expect
    {|
    include_traceback: false
    transform_errors: true
    error_callback present: false
    error_counts empty: true
    |}]

let%expect_test "Error_handling - custom initialization" =
  let callback _exn _ctx = () in
  let middleware =
    create ~include_traceback:true ~error_callback:(Some callback)
      ~transform_errors:false ()
  in
  printf "include_traceback: %b\n" middleware.include_traceback;
  printf "transform_errors: %b\n" middleware.transform_errors;
  printf "error_callback present: %b\n"
    (Option.is_some middleware.error_callback);
  [%expect
    {|
    include_traceback: true
    transform_errors: false
    error_callback present: true
    |}]

(* =============================================================================
   Tests for transform_error
   ============================================================================= *)

let%expect_test "transform_error - MCP error unchanged" =
  let middleware = create () in
  let error =
    Mcp_shared.Exceptions.Mcp_error
      { code = -32001; message = "test error"; data = None }
  in
  let result = transform_error middleware error in
  (match result with
  | Mcp_shared.Exceptions.Mcp_error { code; message; _ } ->
    printf "code: %d\n" code;
    printf "message: %s\n" message
  | _ -> printf "unexpected error type\n");
  [%expect {|
    code: -32001
    message: test error
    |}]

let%expect_test "transform_error - transform disabled" =
  let middleware = create ~transform_errors:false () in
  let error = Failure "test error" in
  let result = transform_error middleware error in
  (match result with
  | Failure msg -> printf "original Failure: %s\n" msg
  | _ -> printf "error was transformed\n");
  [%expect {| original Failure: test error |}]

let%expect_test "transform_error - Invalid_argument to invalid params" =
  let middleware = create () in
  let error = Invalid_argument "test error" in
  let result = transform_error middleware error in
  (match result with
  | Mcp_shared.Exceptions.Mcp_error { code; message; _ } ->
    printf "code: %d\n" code;
    printf "contains 'Invalid params': %b\n"
      (String.is_substring message ~substring:"Invalid params")
  | _ -> printf "unexpected error type\n");
  [%expect {|
    code: -32602
    contains 'Invalid params': true
    |}]

let%expect_test "transform_error - Not_found_s to resource not found" =
  let middleware = create () in
  let error = Not_found_s (Sexp.Atom "missing_key") in
  let result = transform_error middleware error in
  (match result with
  | Mcp_shared.Exceptions.Mcp_error { code; message; _ } ->
    printf "code: %d\n" code;
    printf "contains 'Resource not found': %b\n"
      (String.is_substring message ~substring:"Resource not found")
  | _ -> printf "unexpected error type\n");
  [%expect {|
    code: -32001
    contains 'Resource not found': true
    |}]

let%expect_test "transform_error - Unix ENOENT to resource not found" =
  let middleware = create () in
  let error =
    Core_unix.Unix_error (Core_unix.ENOENT, "open", "/missing/file")
  in
  let result = transform_error middleware error in
  (match result with
  | Mcp_shared.Exceptions.Mcp_error { code; message; _ } ->
    printf "code: %d\n" code;
    printf "contains 'Resource not found': %b\n"
      (String.is_substring message ~substring:"Resource not found")
  | _ -> printf "unexpected error type\n");
  [%expect {|
    code: -32001
    contains 'Resource not found': true
    |}]

let%expect_test "transform_error - Unix EACCES to permission denied" =
  let middleware = create () in
  let error =
    Core_unix.Unix_error (Core_unix.EACCES, "open", "/forbidden/file")
  in
  let result = transform_error middleware error in
  (match result with
  | Mcp_shared.Exceptions.Mcp_error { code; message; _ } ->
    printf "code: %d\n" code;
    printf "contains 'Permission denied': %b\n"
      (String.is_substring message ~substring:"Permission denied")
  | _ -> printf "unexpected error type\n");
  [%expect {|
    code: -32000
    contains 'Permission denied': true
    |}]

let%expect_test "transform_error - Unix ETIMEDOUT to request timeout" =
  let middleware = create () in
  let error = Core_unix.Unix_error (Core_unix.ETIMEDOUT, "connect", "server") in
  let result = transform_error middleware error in
  (match result with
  | Mcp_shared.Exceptions.Mcp_error { code; message; _ } ->
    printf "code: %d\n" code;
    printf "contains 'Request timeout': %b\n"
      (String.is_substring message ~substring:"Request timeout")
  | _ -> printf "unexpected error type\n");
  [%expect {|
    code: -32000
    contains 'Request timeout': true
    |}]

let%expect_test "transform_error - generic error to internal error" =
  let middleware = create () in
  let error = Failure "test error" in
  let result = transform_error middleware error in
  (match result with
  | Mcp_shared.Exceptions.Mcp_error { code; message; _ } ->
    printf "code: %d\n" code;
    printf "contains 'Internal error': %b\n"
      (String.is_substring message ~substring:"Internal error")
  | _ -> printf "unexpected error type\n");
  [%expect {|
    code: -32603
    contains 'Internal error': true
    |}]

(* =============================================================================
   Tests for get_error_stats
   ============================================================================= *)

let%expect_test "get_error_stats - empty initially" =
  let middleware = create () in
  let stats = get_error_stats middleware in
  printf "empty: %b\n" (Map.is_empty stats);
  [%expect {| empty: true |}]

(* =============================================================================
   Tests for Retry module - Default Initialization
   ============================================================================= *)

let%expect_test "Retry - default initialization" =
  let middleware = Retry.create () in
  printf "max_retries: %d\n" middleware.max_retries;
  printf "base_delay: %.1f\n" middleware.base_delay;
  printf "max_delay: %.1f\n" middleware.max_delay;
  printf "backoff_multiplier: %.1f\n" middleware.backoff_multiplier;
  [%expect
    {|
    max_retries: 3
    base_delay: 1.0
    max_delay: 60.0
    backoff_multiplier: 2.0
    |}]

let%expect_test "Retry - custom initialization" =
  let middleware =
    Retry.create ~max_retries:5 ~base_delay:2.0 ~max_delay:120.0
      ~backoff_multiplier:3.0 ()
  in
  printf "max_retries: %d\n" middleware.max_retries;
  printf "base_delay: %.1f\n" middleware.base_delay;
  printf "max_delay: %.1f\n" middleware.max_delay;
  printf "backoff_multiplier: %.1f\n" middleware.backoff_multiplier;
  [%expect
    {|
    max_retries: 5
    base_delay: 2.0
    max_delay: 120.0
    backoff_multiplier: 3.0
    |}]

(* =============================================================================
   Tests for Retry.should_retry
   ============================================================================= *)

let%expect_test "Retry.should_retry - connection refused" =
  let middleware = Retry.create () in
  let error =
    Core_unix.Unix_error (Core_unix.ECONNREFUSED, "connect", "server")
  in
  printf "should_retry: %b\n" (Retry.should_retry middleware error);
  [%expect {| should_retry: true |}]

let%expect_test "Retry.should_retry - connection reset" =
  let middleware = Retry.create () in
  let error = Core_unix.Unix_error (Core_unix.ECONNRESET, "read", "server") in
  printf "should_retry: %b\n" (Retry.should_retry middleware error);
  [%expect {| should_retry: true |}]

let%expect_test "Retry.should_retry - non-retryable error" =
  let middleware = Retry.create () in
  let error = Invalid_argument "bad arg" in
  printf "should_retry: %b\n" (Retry.should_retry middleware error);
  [%expect {| should_retry: false |}]

let%expect_test "Retry.should_retry - custom predicate" =
  let custom_predicate = function
    | Failure _ -> true
    | _ -> false
  in
  let middleware = Retry.create ~retry_exceptions:[ custom_predicate ] () in
  printf "failure should_retry: %b\n"
    (Retry.should_retry middleware (Failure "test"));
  printf "invalid_arg should_retry: %b\n"
    (Retry.should_retry middleware (Invalid_argument "test"));
  [%expect
    {|
    failure should_retry: true
    invalid_arg should_retry: false
    |}]

(* =============================================================================
   Tests for Retry.calculate_delay
   ============================================================================= *)

let%expect_test "Retry.calculate_delay - exponential backoff" =
  let middleware =
    Retry.create ~base_delay:1.0 ~backoff_multiplier:2.0 ~max_delay:10.0 ()
  in
  printf "attempt 0: %.1f\n" (Retry.calculate_delay middleware 0);
  printf "attempt 1: %.1f\n" (Retry.calculate_delay middleware 1);
  printf "attempt 2: %.1f\n" (Retry.calculate_delay middleware 2);
  printf "attempt 3: %.1f\n" (Retry.calculate_delay middleware 3);
  printf "attempt 4 (capped): %.1f\n" (Retry.calculate_delay middleware 4);
  [%expect
    {|
    attempt 0: 1.0
    attempt 1: 2.0
    attempt 2: 4.0
    attempt 3: 8.0
    attempt 4 (capped): 10.0
    |}]
