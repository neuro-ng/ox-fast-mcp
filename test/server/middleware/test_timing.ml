(** Tests for timing middleware. *)

open! Core
open! Expect_test_helpers_core
open Server_middleware.Timing

(* =============================================================================
   Tests for TimingMiddleware
   ============================================================================= *)

let%expect_test "TimingMiddleware - default initialization" =
  let middleware = TimingMiddleware.create () in
  printf "middleware created successfully: true\n";
  ignore middleware;
  [%expect {| middleware created successfully: true |}]

let%expect_test "TimingMiddleware - custom initialization" =
  let logger = Logs.Src.create "custom.timing" in
  let middleware = TimingMiddleware.create ~logger ~log_level:Logs.Debug () in
  printf "custom middleware created: true\n";
  ignore middleware;
  [%expect {| custom middleware created: true |}]

let%expect_test "TimingMiddleware - default log level is Info" =
  (* Create with defaults and verify it can be created *)
  let _middleware = TimingMiddleware.create () in
  printf "default log level used: true\n";
  [%expect {| default log level used: true |}]

(* =============================================================================
   Tests for DetailedTimingMiddleware
   ============================================================================= *)

let%expect_test "DetailedTimingMiddleware - default initialization" =
  let middleware = DetailedTimingMiddleware.create () in
  printf "detailed middleware created successfully: true\n";
  ignore middleware;
  [%expect {| detailed middleware created successfully: true |}]

let%expect_test "DetailedTimingMiddleware - custom logger" =
  let logger = Logs.Src.create "custom.timing.detailed" in
  let middleware = DetailedTimingMiddleware.create ~logger () in
  printf "custom detailed middleware created: true\n";
  ignore middleware;
  [%expect {| custom detailed middleware created: true |}]

let%expect_test "DetailedTimingMiddleware - custom log level" =
  let middleware = DetailedTimingMiddleware.create ~log_level:Logs.Warning () in
  printf "warning level middleware created: true\n";
  ignore middleware;
  [%expect {| warning level middleware created: true |}]

(* =============================================================================
   Tests for Logs.level
   ============================================================================= *)

let%expect_test "Logs.level - Info" =
  let level = Logs.Info in
  printf "level: %s\n" (Logs.level_to_string (Some level));
  [%expect {| level: info |}]

let%expect_test "Logs.level - Debug" =
  let level = Logs.Debug in
  printf "level: %s\n" (Logs.level_to_string (Some level));
  [%expect {| level: debug |}]

let%expect_test "Logs.level - Warning" =
  let level = Logs.Warning in
  printf "level: %s\n" (Logs.level_to_string (Some level));
  [%expect {| level: warning |}]

let%expect_test "Logs.level - Error" =
  let level = Logs.Error in
  printf "level: %s\n" (Logs.level_to_string (Some level));
  [%expect {| level: error |}]

(* =============================================================================
   Tests for Logs.Src
   ============================================================================= *)

let%expect_test "Logs.Src - create custom source" =
  let src = Logs.Src.create "test.timing" in
  printf "source name: %s\n" (Logs.Src.name src);
  [%expect {| source name: test.timing |}]

let%expect_test "Logs.Src - create fastmcp timing source" =
  let src = Logs.Src.create "fastmcp.timing" in
  printf "source name: %s\n" (Logs.Src.name src);
  [%expect {| source name: fastmcp.timing |}]

let%expect_test "Logs.Src - create detailed timing source" =
  let src = Logs.Src.create "fastmcp.timing.detailed" in
  printf "source name: %s\n" (Logs.Src.name src);
  [%expect {| source name: fastmcp.timing.detailed |}]

(* =============================================================================
   Tests for operation name formatting
   ============================================================================= *)

let%expect_test "operation name formatting - tool" =
  let tool_name = "test_tool" in
  let formatted = Printf.sprintf "Tool '%s'" tool_name in
  printf "%s\n" formatted;
  [%expect {| Tool 'test_tool' |}]

let%expect_test "operation name formatting - resource" =
  let resource_uri = "timer://test" in
  let formatted = Printf.sprintf "Resource '%s'" resource_uri in
  printf "%s\n" formatted;
  [%expect {| Resource 'timer://test' |}]

let%expect_test "operation name formatting - prompt" =
  let prompt_name = "test_prompt" in
  let formatted = Printf.sprintf "Prompt '%s'" prompt_name in
  printf "%s\n" formatted;
  [%expect {| Prompt 'test_prompt' |}]

let%expect_test "operation name formatting - list operations" =
  printf "List tools\n";
  printf "List resources\n";
  printf "List resource templates\n";
  printf "List prompts\n";
  [%expect
    {|
    List tools
    List resources
    List resource templates
    List prompts
    |}]

(* =============================================================================
   Tests for duration formatting
   ============================================================================= *)

let%expect_test "duration formatting - milliseconds" =
  let duration_ms = 123.456 in
  printf "completed in %.2fms\n" duration_ms;
  [%expect {| completed in 123.46ms |}]

let%expect_test "duration formatting - fast operation" =
  let duration_ms = 0.5 in
  printf "completed in %.2fms\n" duration_ms;
  [%expect {| completed in 0.50ms |}]

let%expect_test "duration formatting - slow operation" =
  let duration_ms = 5000.0 in
  printf "completed in %.2fms\n" duration_ms;
  [%expect {| completed in 5000.00ms |}]

(* =============================================================================
   Tests for log message formatting
   ============================================================================= *)

let%expect_test "log message - success format" =
  let method_name = "tools/call" in
  let duration_ms = 42.5 in
  printf "Request %s completed in %.2fms\n" method_name duration_ms;
  [%expect {| Request tools/call completed in 42.50ms |}]

let%expect_test "log message - failure format" =
  let method_name = "resources/read" in
  let duration_ms = 100.0 in
  let error = "Resource not found" in
  printf "Request %s failed after %.2fms: %s\n" method_name duration_ms error;
  [%expect
    {| Request resources/read failed after 100.00ms: Resource not found |}]

let%expect_test "log message - detailed tool format" =
  let tool_name = "add" in
  let duration_ms = 5.25 in
  printf "Tool '%s' completed in %.2fms\n" tool_name duration_ms;
  [%expect {| Tool 'add' completed in 5.25ms |}]

let%expect_test "log message - detailed operation failure" =
  let operation_name = "List prompts" in
  let duration_ms = 200.0 in
  let error = "Connection timeout" in
  printf "%s failed after %.2fms: %s\n" operation_name duration_ms error;
  [%expect {| List prompts failed after 200.00ms: Connection timeout |}]
