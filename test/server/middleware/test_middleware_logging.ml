(** Tests for logging middleware. *)

open! Core
open! Expect_test_helpers_core
open Server_middleware.Middleware_logging
open Server_middleware.Middleware

(** Helper to create a mock context *)
let create_mock_context ?(method_ = Some "test_method") ?(source = `Client)
    ?(type_ = `Request) ?(message = `Assoc [ ("test", `String "data") ]) () =
  {
    message;
    fastmcp_context = None;
    source;
    type_;
    method_;
    timestamp = Time_ns.now ();
    params = `Null;
    id = Some "test-id";
    resource = None;
  }

(* =============================================================================
   Tests for Logging_middleware - Initialization
   ============================================================================= *)

let%expect_test "Logging_middleware - default initialization" =
  let middleware = Logging_middleware.create () in
  printf "include_payloads: %b\n" middleware.include_payloads;
  printf "include_payload_length: %b\n" middleware.include_payload_length;
  printf "estimate_payload_tokens: %b\n" middleware.estimate_payload_tokens;
  printf "max_payload_length: %d\n" middleware.max_payload_length;
  printf "methods is None: %b\n" (Option.is_none middleware.methods);
  printf "payload_serializer is None: %b\n"
    (Option.is_none middleware.payload_serializer);
  [%expect
    {|
    include_payloads: false
    include_payload_length: false
    estimate_payload_tokens: false
    max_payload_length: 1000
    methods is None: true
    payload_serializer is None: true
    |}]

let%expect_test "Logging_middleware - custom initialization" =
  let middleware =
    Logging_middleware.create ~include_payloads:true
      ~include_payload_length:true ~estimate_payload_tokens:true
      ~max_payload_length:500
      ~methods:(Some [ "tools/call"; "resources/read" ])
      ()
  in
  printf "include_payloads: %b\n" middleware.include_payloads;
  printf "include_payload_length: %b\n" middleware.include_payload_length;
  printf "estimate_payload_tokens: %b\n" middleware.estimate_payload_tokens;
  printf "max_payload_length: %d\n" middleware.max_payload_length;
  printf "methods count: %d\n"
    (Option.value_map middleware.methods ~default:0 ~f:List.length);
  [%expect
    {|
    include_payloads: true
    include_payload_length: true
    estimate_payload_tokens: true
    max_payload_length: 500
    methods count: 2
    |}]

(* =============================================================================
   Tests for Logging_middleware - Message formatting
   ============================================================================= *)

let%expect_test "Logging_middleware - create_before_message basic" =
  let middleware = Logging_middleware.create () in
  let context = create_mock_context () in
  let message = Logging_middleware.create_before_message middleware context in
  printf "%s\n" message;
  [%expect {| event=request_start method=test_method source=client |}]

let%expect_test "Logging_middleware - create_before_message with payload length"
    =
  let middleware = Logging_middleware.create ~include_payload_length:true () in
  let context = create_mock_context () in
  let message = Logging_middleware.create_before_message middleware context in
  printf "contains payload_length: %b\n"
    (String.is_substring message ~substring:"payload_length=");
  [%expect {| contains payload_length: true |}]

let%expect_test "Logging_middleware - create_before_message with token \
                 estimation" =
  let middleware = Logging_middleware.create ~estimate_payload_tokens:true () in
  let context = create_mock_context () in
  let message = Logging_middleware.create_before_message middleware context in
  printf "contains payload_tokens: %b\n"
    (String.is_substring message ~substring:"payload_tokens=");
  [%expect {| contains payload_tokens: true |}]

let%expect_test "Logging_middleware - create_before_message with payloads" =
  let middleware = Logging_middleware.create ~include_payloads:true () in
  let context = create_mock_context () in
  let message = Logging_middleware.create_before_message middleware context in
  printf "contains payload=: %b\n"
    (String.is_substring message ~substring:"payload=");
  printf "contains payload_type=: %b\n"
    (String.is_substring message ~substring:"payload_type=");
  [%expect
    {|
    contains payload=: true
    contains payload_type=: true
    |}]

let%expect_test "Logging_middleware - create_before_message truncates long \
                 payloads" =
  let middleware =
    Logging_middleware.create ~include_payloads:true ~max_payload_length:10 ()
  in
  let context =
    create_mock_context
      ~message:(`Assoc [ ("long_data", `String "this is a very long string") ])
      ()
  in
  let message = Logging_middleware.create_before_message middleware context in
  printf "contains '...': %b\n" (String.is_substring message ~substring:"...");
  [%expect {| contains '...': true |}]

(* =============================================================================
   Tests for Structured_logging - Initialization
   ============================================================================= *)

let%expect_test "Structured_logging - default initialization" =
  let middleware = Structured_logging.create () in
  printf "include_payloads: %b\n" middleware.include_payloads;
  printf "include_payload_length: %b\n" middleware.include_payload_length;
  printf "estimate_payload_tokens: %b\n" middleware.estimate_payload_tokens;
  printf "methods is None: %b\n" (Option.is_none middleware.methods);
  [%expect
    {|
    include_payloads: false
    include_payload_length: false
    estimate_payload_tokens: false
    methods is None: true
    |}]

let%expect_test "Structured_logging - custom initialization" =
  let middleware =
    Structured_logging.create ~include_payloads:true
      ~include_payload_length:true ~estimate_payload_tokens:true
      ~log_level:Logging.Level.Debug ()
  in
  printf "include_payloads: %b\n" middleware.include_payloads;
  printf "include_payload_length: %b\n" middleware.include_payload_length;
  printf "estimate_payload_tokens: %b\n" middleware.estimate_payload_tokens;
  [%expect
    {|
    include_payloads: true
    include_payload_length: true
    estimate_payload_tokens: true
    |}]

(* =============================================================================
   Tests for Structured_logging - Message formatting
   ============================================================================= *)

let%expect_test "Structured_logging - create_before_message basic" =
  let middleware = Structured_logging.create () in
  let context = create_mock_context () in
  let message = Structured_logging.create_before_message middleware context in
  let json_str = Yojson.Safe.to_string message in
  printf "is valid JSON: %b\n" (String.is_prefix json_str ~prefix:"{");
  printf "contains event: %b\n"
    (String.is_substring json_str ~substring:"\"event\"");
  printf "contains method: %b\n"
    (String.is_substring json_str ~substring:"\"method\"");
  printf "contains source: %b\n"
    (String.is_substring json_str ~substring:"\"source\"");
  [%expect
    {|
    is valid JSON: true
    contains event: true
    contains method: true
    contains source: true
    |}]

let%expect_test "Structured_logging - create_before_message with payload length"
    =
  let middleware = Structured_logging.create ~include_payload_length:true () in
  let context = create_mock_context () in
  let message = Structured_logging.create_before_message middleware context in
  let json_str = Yojson.Safe.to_string message in
  printf "contains payload_length: %b\n"
    (String.is_substring json_str ~substring:"\"payload_length\"");
  [%expect {| contains payload_length: true |}]

let%expect_test "Structured_logging - create_before_message with token \
                 estimation" =
  let middleware = Structured_logging.create ~estimate_payload_tokens:true () in
  let context = create_mock_context () in
  let message = Structured_logging.create_before_message middleware context in
  let json_str = Yojson.Safe.to_string message in
  printf "contains payload_tokens: %b\n"
    (String.is_substring json_str ~substring:"\"payload_tokens\"");
  [%expect {| contains payload_tokens: true |}]

let%expect_test "Structured_logging - create_after_message has duration" =
  let context = create_mock_context () in
  let start_time = Time_ns.now () in
  let message = Structured_logging.create_after_message context start_time in
  let json_str = Yojson.Safe.to_string message in
  printf "contains duration_ms: %b\n"
    (String.is_substring json_str ~substring:"\"duration_ms\"");
  printf "contains event success: %b\n"
    (String.is_substring json_str ~substring:"request_success");
  [%expect
    {|
    contains duration_ms: true
    contains event success: true
    |}]

let%expect_test "Structured_logging - create_error_message has error" =
  let context = create_mock_context () in
  let start_time = Time_ns.now () in
  let error = Failure "test error" in
  let message =
    Structured_logging.create_error_message context start_time error
  in
  let json_str = Yojson.Safe.to_string message in
  printf "contains duration_ms: %b\n"
    (String.is_substring json_str ~substring:"\"duration_ms\"");
  printf "contains error: %b\n"
    (String.is_substring json_str ~substring:"\"error\"");
  printf "contains event error: %b\n"
    (String.is_substring json_str ~substring:"request_error");
  [%expect
    {|
    contains duration_ms: true
    contains error: true
    contains event error: true
    |}]

(* =============================================================================
   Tests for custom payload serializer
   ============================================================================= *)

let%expect_test "Logging_middleware - custom payload serializer" =
  let custom_serializer _ = "CUSTOM_PAYLOAD" in
  let middleware =
    Logging_middleware.create ~include_payloads:true
      ~payload_serializer:(Some custom_serializer) ()
  in
  let context = create_mock_context () in
  let message = Logging_middleware.create_before_message middleware context in
  printf "contains CUSTOM_PAYLOAD: %b\n"
    (String.is_substring message ~substring:"CUSTOM_PAYLOAD");
  [%expect {| contains CUSTOM_PAYLOAD: true |}]

let%expect_test "Structured_logging - custom payload serializer" =
  let custom_serializer _ = "CUSTOM_PAYLOAD" in
  let middleware =
    Structured_logging.create ~include_payloads:true
      ~payload_serializer:(Some custom_serializer) ()
  in
  let context = create_mock_context () in
  let message = Structured_logging.create_before_message middleware context in
  let json_str = Yojson.Safe.to_string message in
  printf "contains CUSTOM_PAYLOAD: %b\n"
    (String.is_substring json_str ~substring:"CUSTOM_PAYLOAD");
  [%expect {| contains CUSTOM_PAYLOAD: true |}]

(* =============================================================================
   Tests for get_duration_ms
   ============================================================================= *)

let%expect_test "get_duration_ms - returns non-negative" =
  let start_time = Time_ns.now () in
  let duration = get_duration_ms start_time in
  printf "duration >= 0: %b\n" Float.(duration >= 0.0);
  [%expect {| duration >= 0: true |}]

(* =============================================================================
   Tests for default_serializer
   ============================================================================= *)

let%expect_test "default_serializer - serializes JSON" =
  let payload = `Assoc [ ("key", `String "value"); ("num", `Int 42) ] in
  let result = default_serializer payload in
  printf "%s\n" result;
  [%expect {| {"key":"value","num":42} |}]
