open Alcotest
open Utilities.Exceptions

(** Test error data creation and handling *)
let test_error_data () =
  let error = { code = 404; message = "Not Found" } in
  check int "error code" 404 error.code;
  check string "error message" "Not Found" error.message;
  
  (* Test JSON serialization *)
  let json = error_data_to_yojson error in
  match error_data_of_yojson json with
  | Ok decoded ->
    check int "decoded error code" error.code decoded.code;
    check string "decoded error message" error.message decoded.message
  | Error msg -> fail msg

(** Test exception group creation and iteration *)
let test_exception_group () =
  let nested_exc = create_exception_group "nested" [Failure "nested error"] in
  let group = create_exception_group "test" [
    Failure "test error";
    nested_exc.exceptions |> List.hd;
  ] in
  
  let exceptions = iter_exc group in
  check int "exception count" 2 (List.length exceptions);
  
  (* Verify we can extract all exceptions *)
  let messages = List.filter_map (function
    | Failure msg -> Some msg
    | _ -> None
  ) exceptions |> List.sort String.compare in
  let expected = ["nested error"; "test error"] in
  check (list string) "exception messages" expected messages

(** Test HTTP timeout handling *)
let test_http_timeout () =
  let caught = ref false in
  try
    let _never_returns : unit = handle_http_timeout () in
    fail "Expected timeout exception"
  with Failure msg ->
    caught := true;
    check bool "timeout message" true (Str.string_match (Str.regexp ".*timeout.*") msg 0);
    check bool "exception caught" true !caught

(** Test catch handlers configuration *)
let test_catch_handlers () =
  let handlers_with_first = get_catch_handlers ~raise_first_error:true in
  let handlers_without_first = get_catch_handlers ~raise_first_error:false in
  
  check bool "handlers with first error" true (List.length handlers_with_first > 0);
  check bool "handlers without first error" true (List.length handlers_without_first = 0)

(** Test MCP error creation *)
let test_mcp_error () =
  let code = 400 in
  let message = "Bad Request" in
  let exc = create_mcp_error code message in
  
  match exc with
  | Failure msg ->
    let contains s1 s2 =
      try
        let _ = Str.search_forward (Str.regexp_string s2) s1 0 in
        true
      with Not_found -> false
    in
    check bool "error message contains code" true (contains msg (string_of_int code));
    check bool "error message contains text" true (contains msg message)
  | _ -> fail "Expected Failure exception"

(** Test error data extraction *)
let test_error_data_extraction () =
  let message = "Test error" in
  let exc = Failure message in
  
  match error_data_of_exn exc with
  | Some error_data ->
    check int "extracted error code" 500 error_data.code;
    check string "extracted error message" message error_data.message
  | None -> fail "Expected Some error_data"

(** Main test suite *)
let () =
  run "Exceptions Module Tests" [
    ("Error Data", [
      test_case "Error data handling" `Quick test_error_data;
    ]);
    ("Exception Groups", [
      test_case "Exception group handling" `Quick test_exception_group;
    ]);
    ("HTTP Timeout", [
      test_case "HTTP timeout handling" `Quick test_http_timeout;
    ]);
    ("Catch Handlers", [
      test_case "Catch handlers configuration" `Quick test_catch_handlers;
    ]);
    ("MCP Errors", [
      test_case "MCP error creation" `Quick test_mcp_error;
      test_case "Error data extraction" `Quick test_error_data_extraction;
    ]);
  ] 