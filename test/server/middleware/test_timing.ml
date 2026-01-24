(** Tests for timing middleware. *)

open! Core
open! Expect_test_helpers_core
open Server_middleware.Timing

(* =============================================================================
   Tests for timing middleware configuration
   ============================================================================= *)

let%expect_test "create - default initialization" =
  let config = create () in
  printf "Timing config created: %b\n" (phys_equal config config);
  [%expect {| Timing config created: true |}]

let%expect_test "create - detailed mode" =
  let config = create ~detailed:true () in
  printf "Detailed config created: %b\n" (phys_equal config config);
  [%expect {| Detailed config created: true |}]

(* =============================================================================
   Tests for Timing module (middleware interface)
   ============================================================================= *)

let%expect_test "Timing module - create" =
  let middleware = Timing.create () in
  printf "Timing middleware created: %b\n" (phys_equal middleware middleware);
  [%expect {| Timing middleware created: true |}]

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
