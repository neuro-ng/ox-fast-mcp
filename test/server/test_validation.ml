(** Tests for Validation Helpers *)

open! Core
open! Async
module Ox_fast_mcp = Server.Ox_fast_mcp

(** {1 Test: Tool Name Validation} *)

let%expect_test "validate_tool_name accepts valid names" =
  let result = Ox_fast_mcp.validate_tool_name "my_tool" in
  print_s [%sexp (result : (unit, string) Result.t)];
  [%expect {| (Ok ()) |}];

  let result2 = Ox_fast_mcp.validate_tool_name "Tool123" in
  print_s [%sexp (result2 : (unit, string) Result.t)];
  [%expect {| (Ok ()) |}];

  let result3 = Ox_fast_mcp.validate_tool_name "_private_tool" in
  print_s [%sexp (result3 : (unit, string) Result.t)];
  [%expect {| (Ok ()) |}];

  return ()

let%expect_test "validate_tool_name rejects empty names" =
  let result = Ox_fast_mcp.validate_tool_name "" in
  print_s [%sexp (result : (unit, string) Result.t)];
  [%expect {| (Error "Tool name cannot be empty") |}];
  return ()

let%expect_test "validate_tool_name rejects names with spaces" =
  let result = Ox_fast_mcp.validate_tool_name "my tool" in
  print_s [%sexp (result : (unit, string) Result.t)];
  [%expect {| (Error "Tool name cannot contain spaces") |}];
  return ()

let%expect_test "validate_tool_name rejects names starting with invalid \
                 characters" =
  let result = Ox_fast_mcp.validate_tool_name "-tool" in
  print_s [%sexp (result : (unit, string) Result.t)];
  [%expect
    {| (Error "Tool name must start with a letter, number, or underscore") |}];
  return ()

(** {1 Test: Resource URI Validation} *)

let%expect_test "validate_resource_uri accepts valid URIs" =
  let result = Ox_fast_mcp.validate_resource_uri "file:///path/to/file.txt" in
  print_s [%sexp (result : (unit, string) Result.t)];
  [%expect {| (Ok ()) |}];

  let result2 =
    Ox_fast_mcp.validate_resource_uri "http://example.com/resource"
  in
  print_s [%sexp (result2 : (unit, string) Result.t)];
  [%expect {| (Ok ()) |}];

  return ()

let%expect_test "validate_resource_uri rejects empty URIs" =
  let result = Ox_fast_mcp.validate_resource_uri "" in
  print_s [%sexp (result : (unit, string) Result.t)];
  [%expect {| (Error "Resource URI cannot be empty") |}];
  return ()

let%expect_test "validate_resource_uri rejects URIs without scheme" =
  let result = Ox_fast_mcp.validate_resource_uri "/path/to/file.txt" in
  print_s [%sexp (result : (unit, string) Result.t)];
  [%expect
    {| (Error "Resource URI must contain a scheme (e.g., 'file://', 'http://')") |}];
  return ()

let%expect_test "validate_resource_uri rejects invalid scheme format" =
  let result = Ox_fast_mcp.validate_resource_uri "://noscheme" in
  print_s [%sexp (result : (unit, string) Result.t)];
  [%expect
    {| (Error "Resource URI must have a valid scheme format (scheme://...)") |}];
  return ()

(** {1 Test: Prompt Name Validation} *)

let%expect_test "validate_prompt_name accepts valid names" =
  let result = Ox_fast_mcp.validate_prompt_name "my_prompt" in
  print_s [%sexp (result : (unit, string) Result.t)];
  [%expect {| (Ok ()) |}];

  let result2 = Ox_fast_mcp.validate_prompt_name "Prompt-123" in
  print_s [%sexp (result2 : (unit, string) Result.t)];
  [%expect {| (Ok ()) |}];

  return ()

let%expect_test "validate_prompt_name rejects empty names" =
  let result = Ox_fast_mcp.validate_prompt_name "" in
  print_s [%sexp (result : (unit, string) Result.t)];
  [%expect {| (Error "Prompt name cannot be empty") |}];
  return ()

let%expect_test "validate_prompt_name rejects names with spaces" =
  let result = Ox_fast_mcp.validate_prompt_name "my prompt" in
  print_s [%sexp (result : (unit, string) Result.t)];
  [%expect {| (Error "Prompt name cannot contain spaces") |}];
  return ()

(** {1 Test: Template URI Validation} *)

let%expect_test "validate_template_uri accepts valid template URIs" =
  let result = Ox_fast_mcp.validate_template_uri "file:///{path}" in
  print_s [%sexp (result : (unit, string) Result.t)];
  [%expect {| (Ok ()) |}];

  let result2 =
    Ox_fast_mcp.validate_template_uri "http://api.example.com/{resource}/{id}"
  in
  print_s [%sexp (result2 : (unit, string) Result.t)];
  [%expect {| (Ok ()) |}];

  return ()

let%expect_test "validate_template_uri rejects empty URIs" =
  let result = Ox_fast_mcp.validate_template_uri "" in
  print_s [%sexp (result : (unit, string) Result.t)];
  [%expect {| (Error "Template URI cannot be empty") |}];
  return ()

let%expect_test "validate_template_uri rejects URIs without parameters" =
  let result = Ox_fast_mcp.validate_template_uri "file:///path/to/file.txt" in
  print_s [%sexp (result : (unit, string) Result.t)];
  [%expect
    {|
    (Error
     "Template URI must contain at least one parameter placeholder (e.g., {param})")
    |}];
  return ()

let%expect_test "validate_template_uri rejects URIs without scheme" =
  let result = Ox_fast_mcp.validate_template_uri "/path/{file}" in
  print_s [%sexp (result : (unit, string) Result.t)];
  [%expect
    {| (Error "Template URI must contain a scheme (e.g., 'file://', 'http://')") |}];
  return ()
