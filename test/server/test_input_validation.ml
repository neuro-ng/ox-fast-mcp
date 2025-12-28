(** Input validation tests for OxFastMCP

    Tests for input validation behavior with strict_input_validation setting.
    This module tests the difference between strict JSON schema validation (when
    strict_input_validation=True) and lenient validation (when
    strict_input_validation=False, the default). *)

open Async

(** String-to-Integer Coercion Tests *)

let%expect_test "string_integer_with_strict_validation" =
  (* With strict validation, string integers should raise an error *)
  print_endline "TODO: Implement test_string_integer_with_strict_validation";
  [%expect {| TODO: Implement test_string_integer_with_strict_validation |}];
  return ()

let%expect_test "string_integer_without_strict_validation" =
  (* Without strict validation, string integers should be coerced *)
  print_endline "TODO: Implement test_string_integer_without_strict_validation";
  [%expect {| TODO: Implement test_string_integer_without_strict_validation |}];
  return ()

let%expect_test "default_is_not_strict" =
  (* By default, strict_input_validation should be False *)
  print_endline "TODO: Implement test_default_is_not_strict";
  [%expect {| TODO: Implement test_default_is_not_strict |}];
  return ()

let%expect_test "string_float_coercion" =
  (* Test that string floats are also coerced *)
  print_endline "TODO: Implement test_string_float_coercion";
  [%expect {| TODO: Implement test_string_float_coercion |}];
  return ()

let%expect_test "invalid_coercion_still_fails" =
  (* Even without strict validation, truly invalid inputs should fail *)
  print_endline "TODO: Implement test_invalid_coercion_still_fails";
  [%expect {| TODO: Implement test_invalid_coercion_still_fails |}];
  return ()

(** Validation Error Message Tests *)

let%expect_test "error_message_quality_strict" =
  (* Capture error message with strict validation *)
  print_endline "TODO: Implement test_error_message_quality_strict";
  [%expect {| TODO: Implement test_error_message_quality_strict |}];
  return ()

let%expect_test "error_message_quality_lenient" =
  (* Capture error message with lenient validation *)
  print_endline "TODO: Implement test_error_message_quality_lenient";
  [%expect {| TODO: Implement test_error_message_quality_lenient |}];
  return ()

let%expect_test "missing_required_field_error" =
  (* Test error message for missing required fields *)
  print_endline "TODO: Implement test_missing_required_field_error";
  [%expect {| TODO: Implement test_missing_required_field_error |}];
  return ()

(** Edge Cases Tests *)

let%expect_test "optional_parameters_with_coercion" =
  (* Optional parameters should work with coercion *)
  print_endline "TODO: Implement test_optional_parameters_with_coercion";
  [%expect {| TODO: Implement test_optional_parameters_with_coercion |}];
  return ()

let%expect_test "none_values" =
  (* Test handling of None values *)
  print_endline "TODO: Implement test_none_values";
  [%expect {| TODO: Implement test_none_values |}];
  return ()

let%expect_test "empty_string_to_int" =
  (* Empty strings should fail conversion to int *)
  print_endline "TODO: Implement test_empty_string_to_int";
  [%expect {| TODO: Implement test_empty_string_to_int |}];
  return ()

let%expect_test "boolean_coercion" =
  (* Test boolean value coercion *)
  print_endline "TODO: Implement test_boolean_coercion";
  [%expect {| TODO: Implement test_boolean_coercion |}];
  return ()

let%expect_test "list_of_integers_with_string_elements" =
  (* Test lists containing string representations of integers *)
  print_endline "TODO: Implement test_list_of_integers_with_string_elements";
  [%expect {| TODO: Implement test_list_of_integers_with_string_elements |}];
  return ()
