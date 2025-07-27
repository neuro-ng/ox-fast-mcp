open! Core
open! Async
open! Expect_test_helpers_core

(* Import the exceptions module - equivalent to 'from oxfastmcp import exceptions' *)
module Exceptions = Ox_fast_mcp.Exceptions
open Exceptions

let%expect_test "test error_data creation and serialization" =
  let error_data = { message = "Test error"; code = Some 500; data = None } in
  
  (* Test basic field access *)
  require_equal ~here:[%here] (module String) error_data.message "Test error";
  require_equal ~here:[%here] (module struct type t = int option [@@deriving compare, sexp_of, equal] end) error_data.code (Some 500);
  require ~here:[%here] (Option.is_none error_data.data);
  
  (* Test JSON serialization *)
  let json = yojson_of_error_data error_data in
  print_endline (Yojson.Safe.to_string json);
  [%expect {| {"message":"Test error","code":500} |}];
  
  (* Test JSON deserialization *)
  let deserialized = error_data_of_yojson json in
  require_equal ~here:[%here] (module String) deserialized.message "Test error";
  require_equal ~here:[%here] (module struct type t = int option [@@deriving compare, sexp_of, equal] end) deserialized.code (Some 500);
  return ()

let%expect_test "test error_data with data field" =
  let json_data = `Assoc [("key", `String "value")] in
  let error_data = { message = "Error with data"; code = None; data = Some json_data } in
  
  (* Test serialization with data *)
  let json = yojson_of_error_data error_data in
  print_endline (Yojson.Safe.to_string json);
  [%expect {| {"message":"Error with data","data":{"key":"value"}} |}];
  
  (* Test round-trip *)
  let deserialized = error_data_of_yojson json in
  require_equal ~here:[%here] (module String) deserialized.message "Error with data";
  require_equal ~here:[%here] (module struct type t = int option [@@deriving compare, sexp_of, equal] end) deserialized.code None;
  return ()

let%expect_test "test OxFastMCP_error exception" =
  let error_data = { message = "OxFastMCP test error"; code = Some 400; data = None } in
  let exception_val = OxFastMCP_error error_data in
  
  (* Test exception creation *)
  require ~here:[%here] (is_oxfastmcp_error exception_val);
  
  (* Test error data extraction *)
  let extracted_data = get_error_data exception_val in
  require_equal ~here:[%here] (module String) extracted_data.message "OxFastMCP test error";
  require_equal ~here:[%here] (module struct type t = int option [@@deriving compare, sexp_of, equal] end) extracted_data.code (Some 400);
  
  (* Test string conversion *)
  let error_string = to_string exception_val in
  require ~here:[%here] (String.is_substring error_string ~substring:"OxFastMCP_error");
  require ~here:[%here] (String.is_substring error_string ~substring:"OxFastMCP test error");
  print_s [%sexp (error_string : string)];
  [%expect {| "OxFastMCP_error: OxFastMCP test error" |}];
  return ()

let%expect_test "test all exception types" =
  let test_exception_type name create_fn exception_constructor =
    let error_data = { message = sprintf "%s test" name; code = Some 123; data = None } in
    let exception_val = exception_constructor error_data in
    
    (* Test that it's recognized as OxFastMCP error *)
    require ~here:[%here] (is_oxfastmcp_error exception_val);
    
    (* Test error data extraction *)
    let extracted = get_error_data exception_val in
    require_equal ~here:[%here] (module String) extracted.message (sprintf "%s test" name);
    
    (* Test string conversion *)
    let error_string = to_string exception_val in
    require ~here:[%here] (String.is_substring error_string ~substring:name);
    print_s [%sexp (error_string : string)];
    
    (* Test helper function *)
    let helper_exception = create_fn (sprintf "%s helper test" name) in
    require ~here:[%here] (is_oxfastmcp_error helper_exception);
    let helper_extracted = get_error_data helper_exception in
    require_equal ~here:[%here] (module String) helper_extracted.message (sprintf "%s helper test" name)
  in
  
  test_exception_type "OxFastMCP_error" create_oxfastmcp_error (fun d -> OxFastMCP_error d);
  [%expect {| "OxFastMCP_error: OxFastMCP_error test" |}];
  
  test_exception_type "Validation_error" create_validation_error (fun d -> Validation_error d);
  [%expect {| "Validation_error: Validation_error test" |}];
  
  test_exception_type "Resource_error" create_resource_error (fun d -> Resource_error d);
  [%expect {| "Resource_error: Resource_error test" |}];
  
  test_exception_type "Tool_error" create_tool_error (fun d -> Tool_error d);
  [%expect {| "Tool_error: Tool_error test" |}];
  
  test_exception_type "Prompt_error" create_prompt_error (fun d -> Prompt_error d);
  [%expect {| "Prompt_error: Prompt_error test" |}];
  
  test_exception_type "Invalid_signature" create_invalid_signature (fun d -> Invalid_signature d);
  [%expect {| "Invalid_signature: Invalid_signature test" |}];
  
  test_exception_type "Client_error" create_client_error (fun d -> Client_error d);
  [%expect {| "Client_error: Client_error test" |}];
  
  test_exception_type "Not_found_error" create_not_found_error (fun d -> Not_found_error d);
  [%expect {| "Not_found_error: Not_found_error test" |}];
  
  test_exception_type "Disabled_error" create_disabled_error (fun d -> Disabled_error d);
  [%expect {| "Disabled_error: Disabled_error test" |}];
  
  return ()

let%expect_test "test helper functions with optional parameters" =
  (* Test helper functions with default parameters *)
  let simple_error = create_validation_error "Simple validation error" in
  let extracted = get_error_data simple_error in
  require_equal ~here:[%here] (module String) extracted.message "Simple validation error";
  require_equal ~here:[%here] (module struct type t = int option [@@deriving compare, sexp_of, equal] end) extracted.code None;
  require ~here:[%here] (Option.is_none extracted.data);
  
  (* Test helper functions with optional code *)
  let error_with_code = create_resource_error ~code:(Some 404) "Resource not found" in
  let extracted_with_code = get_error_data error_with_code in
  require_equal ~here:[%here] (module String) extracted_with_code.message "Resource not found";
  require_equal ~here:[%here] (module struct type t = int option [@@deriving compare, sexp_of, equal] end) extracted_with_code.code (Some 404);
  
  (* Test helper functions with optional data *)
  let json_data = `Assoc [("details", `String "Additional info")] in
  let error_with_data = create_tool_error ~data:(Some json_data) "Tool execution failed" in
  let extracted_with_data = get_error_data error_with_data in
  require_equal ~here:[%here] (module String) extracted_with_data.message "Tool execution failed";
  require ~here:[%here] (Option.is_some extracted_with_data.data);
  
  (* Test helper functions with both optional parameters *)
  let full_error = create_client_error ~code:(Some 500) ~data:(Some json_data) "Client connection error" in
  let extracted_full = get_error_data full_error in
  require_equal ~here:[%here] (module String) extracted_full.message "Client connection error";
  require_equal ~here:[%here] (module struct type t = int option [@@deriving compare, sexp_of, equal] end) extracted_full.code (Some 500);
  require ~here:[%here] (Option.is_some extracted_full.data);
  
  return ()

let%expect_test "test exception pattern matching" =
  let test_pattern_matching exception_val expected_type =
    match exception_val with
    | OxFastMCP_error _ -> require_equal ~here:[%here] (module String) expected_type "OxFastMCP_error"
    | Validation_error _ -> require_equal ~here:[%here] (module String) expected_type "Validation_error"
    | Resource_error _ -> require_equal ~here:[%here] (module String) expected_type "Resource_error"
    | Tool_error _ -> require_equal ~here:[%here] (module String) expected_type "Tool_error"
    | Prompt_error _ -> require_equal ~here:[%here] (module String) expected_type "Prompt_error"
    | Invalid_signature _ -> require_equal ~here:[%here] (module String) expected_type "Invalid_signature"
    | Client_error _ -> require_equal ~here:[%here] (module String) expected_type "Client_error"
    | Not_found_error _ -> require_equal ~here:[%here] (module String) expected_type "Not_found_error"
    | Disabled_error _ -> require_equal ~here:[%here] (module String) expected_type "Disabled_error"
    | _ -> failwith "Unexpected exception type"
  in
  
  let error_data = { message = "test"; code = None; data = None } in
  
  test_pattern_matching (OxFastMCP_error error_data) "OxFastMCP_error";
  test_pattern_matching (Validation_error error_data) "Validation_error";
  test_pattern_matching (Resource_error error_data) "Resource_error";
  test_pattern_matching (Tool_error error_data) "Tool_error";
  test_pattern_matching (Prompt_error error_data) "Prompt_error";
  test_pattern_matching (Invalid_signature error_data) "Invalid_signature";
  test_pattern_matching (Client_error error_data) "Client_error";
  test_pattern_matching (Not_found_error error_data) "Not_found_error";
  test_pattern_matching (Disabled_error error_data) "Disabled_error";
  
  return ()

let%expect_test "test non-OxFastMCP exceptions" =
  (* Test with standard OCaml exceptions *)
  let std_exception = Failure "Standard failure" in
  require ~here:[%here] (not (is_oxfastmcp_error std_exception));
  
  let std_string = to_string std_exception in
  require ~here:[%here] (String.is_substring std_string ~substring:"Failure");
  print_s [%sexp (std_string : string)];
  [%expect {| "(Failure \"Standard failure\")" |}];
  
  (* Test get_error_data with non-OxFastMCP exception *)
  show_raise (fun () -> get_error_data std_exception);
  [%expect {| (raised (Failure "Not an OxFastMCP error")) |}];
  
  return ()

let%expect_test "test error_data sexp serialization" =
  let error_data = { message = "Sexp test"; code = Some 200; data = None } in
  
  (* Test sexp conversion *)
  let sexp = sexp_of_error_data error_data in
  print_s sexp;
  [%expect {|
    ((message "Sexp test") (code (200)) (data ())) |}];
  
  (* Test sexp deserialization *)
  let deserialized = error_data_of_sexp sexp in
  require_equal ~here:[%here] (module String) deserialized.message "Sexp test";
  require_equal ~here:[%here] (module struct type t = int option [@@deriving compare, sexp_of, equal] end) deserialized.code (Some 200);
  
  return ()

let%expect_test "test edge cases and error conditions" =
  (* Test empty message *)
  let empty_msg_error = create_oxfastmcp_error "" in
  let extracted = get_error_data empty_msg_error in
  require_equal ~here:[%here] (module String) extracted.message "";
  
  (* Test very long message *)
  let long_message = String.make 1000 'x' in
  let long_msg_error = create_validation_error long_message in
  let extracted_long = get_error_data long_msg_error in
  require_equal ~here:[%here] (module String) extracted_long.message long_message;
  
  (* Test zero code *)
  let zero_code_error = create_resource_error ~code:(Some 0) "Zero code error" in
  let extracted_zero = get_error_data zero_code_error in
  require_equal ~here:[%here] (module struct type t = int option [@@deriving compare, sexp_of, equal] end) extracted_zero.code (Some 0);
  
  (* Test negative code *)
  let neg_code_error = create_tool_error ~code:(Some (-1)) "Negative code error" in
  let extracted_neg = get_error_data neg_code_error in
  require_equal ~here:[%here] (module struct type t = int option [@@deriving compare, sexp_of, equal] end) extracted_neg.code (Some (-1));
  
  return ()

let%expect_test "test JSON edge cases" =
  (* Test with complex JSON data *)
  let complex_json = `Assoc [
    ("level1", `Assoc [
      ("level2", `List [`String "item1"; `String "item2"]);
      ("number", `Int 42);
      ("boolean", `Bool true);
      ("null_value", `Null)
    ])
  ] in
  
  let complex_error = create_prompt_error ~data:(Some complex_json) "Complex JSON error" in
  let extracted = get_error_data complex_error in
  require ~here:[%here] (Option.is_some extracted.data);
  
  (* Test JSON serialization round-trip with complex data *)
  let json = yojson_of_error_data extracted in
  let deserialized = error_data_of_yojson json in
  require_equal ~here:[%here] (module String) deserialized.message "Complex JSON error";
  require ~here:[%here] (Option.is_some deserialized.data);
  
  return ()

let%expect_test "test string representation consistency" =
  let test_cases = [
    ("OxFastMCP_error", create_oxfastmcp_error);
    ("Validation_error", create_validation_error);
    ("Resource_error", create_resource_error);
    ("Tool_error", create_tool_error);
    ("Prompt_error", create_prompt_error);
    ("Invalid_signature", create_invalid_signature);
    ("Client_error", create_client_error);
    ("Not_found_error", create_not_found_error);
    ("Disabled_error", create_disabled_error);
  ] in
  
  List.iter test_cases ~f:(fun (expected_prefix, create_fn) ->
    let error = create_fn "Test message" in
    let error_string = to_string error in
    require ~here:[%here] (String.is_prefix error_string ~prefix:expected_prefix);
    require ~here:[%here] (String.is_substring error_string ~substring:"Test message"));
  
  return () 