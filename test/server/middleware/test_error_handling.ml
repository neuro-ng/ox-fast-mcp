open Core
open Async
open OUnit2
open Ox_fast_mcp.Server.Middleware.Error_handling
open Ox_fast_mcp.Server.Middleware.Middleware
open Ox_fast_mcp.Mcp.Mcp_error

module Test_error_handling = struct
  (* Test fixtures *)
  let mock_context =
    { method_name = "test_method"; params = `Null; id = None; resource = None }

  let mock_call_next result _ctx = return result

  (* Test cases *)
  let test_init_default _ =
    let middleware = create () in
    assert_equal "fastmcp.errors" (Logger.name middleware.logger);
    assert_bool "include_traceback should be false"
      (not middleware.include_traceback);
    assert_bool "error_callback should be None"
      (Option.is_none middleware.error_callback);
    assert_bool "transform_errors should be true" middleware.transform_errors;
    assert_bool "error_counts should be empty"
      (Map.is_empty middleware.error_counts)

  let test_init_custom _ =
    let logger = Logger.create "custom" in
    let callback _err _ctx = () in
    let middleware =
      create ~logger ~include_traceback:true ~error_callback:callback
        ~transform_errors:false ()
    in
    assert_equal "custom" (Logger.name middleware.logger);
    assert_bool "include_traceback should be true" middleware.include_traceback;
    assert_bool "error_callback should be Some"
      (Option.is_some middleware.error_callback);
    assert_bool "transform_errors should be false"
      (not middleware.transform_errors)

  let test_log_error_basic _ =
    let middleware = create () in
    let error = Failure "test error" in
    let log_messages = ref [] in
    let logger =
      Logger.create
        ~on_log:(fun lvl msg -> log_messages := (lvl, msg) :: !log_messages)
        "test"
    in
    let middleware = { middleware with logger } in

    log_error middleware error mock_context;

    let has_error_message =
      List.exists !log_messages ~f:(fun (lvl, msg) ->
          lvl = Logger.Error
          && String.is_substring msg
               ~substring:"Error in test_method: Failure: test error")
    in
    assert_bool "Should log error message" has_error_message;
    assert_equal 1 (Map.find_exn middleware.error_counts "Failure:test_method")

  let test_transform_error_mcp_error _ =
    let middleware = create () in
    let error =
      Mcp_error { code = -32001; message = "test error"; details = None }
    in
    let result = transform_error middleware error in
    assert_equal error result

  let test_transform_error_value_error _ =
    let middleware = create () in
    let error = Invalid_argument "test error" in
    let result = transform_error middleware error in
    match result with
    | Mcp_error err ->
      assert_equal (-32602) err.code;
      assert_bool "Should contain error message"
        (String.is_substring err.message ~substring:"Invalid params: test error")
    | _ -> assert_failure "Should return MCP error"

  let test_transform_error_file_not_found _ =
    let middleware = create () in
    let error = Not_found_error "test error" in
    let result = transform_error middleware error in
    match result with
    | Mcp_error err ->
      assert_equal (-32001) err.code;
      assert_bool "Should contain error message"
        (String.is_substring err.message
           ~substring:"Resource not found: test error")
    | _ -> assert_failure "Should return MCP error"

  let test_transform_error_permission _ =
    let middleware = create () in
    let error = Permission_denied "test error" in
    let result = transform_error middleware error in
    match result with
    | Mcp_error err ->
      assert_equal (-32000) err.code;
      assert_bool "Should contain error message"
        (String.is_substring err.message
           ~substring:"Permission denied: test error")
    | _ -> assert_failure "Should return MCP error"

  let test_transform_error_timeout _ =
    let middleware = create () in
    let error = Timeout "test error" in
    let result = transform_error middleware error in
    match result with
    | Mcp_error err ->
      assert_equal (-32000) err.code;
      assert_bool "Should contain error message"
        (String.is_substring err.message
           ~substring:"Request timeout: test error")
    | _ -> assert_failure "Should return MCP error"

  let test_transform_error_generic _ =
    let middleware = create () in
    let error = Failure "test error" in
    let result = transform_error middleware error in
    match result with
    | Mcp_error err ->
      assert_equal (-32603) err.code;
      assert_bool "Should contain error message"
        (String.is_substring err.message ~substring:"Internal error: test error")
    | _ -> assert_failure "Should return MCP error"

  let test_on_message_success _ =
    let middleware = create () in
    let call_next = mock_call_next "test_result" in

    let%bind result = on_message middleware mock_context call_next in
    assert_equal "test_result" result;
    return ()

  let test_on_message_error_transform _ =
    let middleware = create () in
    let call_next _ctx = raise (Invalid_argument "test error") in
    let log_messages = ref [] in
    let logger =
      Logger.create
        ~on_log:(fun lvl msg -> log_messages := (lvl, msg) :: !log_messages)
        "test"
    in
    let middleware = { middleware with logger } in

    match%bind
      try_with (fun () -> on_message middleware mock_context call_next)
    with
    | Ok _ -> assert_failure "Should raise error"
    | Error (Mcp_error err) ->
      assert_equal (-32602) err.code;
      assert_bool "Should contain error message"
        (String.is_substring err.message ~substring:"Invalid params: test error");
      let has_error_log =
        List.exists !log_messages ~f:(fun (lvl, msg) ->
            lvl = Logger.Error
            && String.is_substring msg
                 ~substring:"Error in test_method: Invalid_argument: test error")
      in
      assert_bool "Should log error" has_error_log;
      return ()
    | Error e ->
      assert_failure (sprintf "Unexpected error: %s" (Exn.to_string e))

  let test_get_error_stats _ =
    let middleware = create () in
    log_error middleware (Invalid_argument "error1") mock_context;
    log_error middleware (Invalid_argument "error2") mock_context;
    log_error middleware (Failure "error3") mock_context;

    let stats = get_error_stats middleware in
    assert_equal 2 (Map.find_exn stats "Invalid_argument:test_method");
    assert_equal 1 (Map.find_exn stats "Failure:test_method")

  let suite =
    "Error handling middleware tests"
    >::: [
           "test init default" >:: test_init_default;
           "test init custom" >:: test_init_custom;
           "test log error basic" >:: test_log_error_basic;
           "test transform error mcp error" >:: test_transform_error_mcp_error;
           "test transform error value error"
           >:: test_transform_error_value_error;
           "test transform error file not found"
           >:: test_transform_error_file_not_found;
           "test transform error permission" >:: test_transform_error_permission;
           "test transform error timeout" >:: test_transform_error_timeout;
           "test transform error generic" >:: test_transform_error_generic;
           ( "test on message success" >:: fun _ ->
             Thread_safe.block_on_async_exn test_on_message_success );
           ( "test on message error transform" >:: fun _ ->
             Thread_safe.block_on_async_exn test_on_message_error_transform );
           "test get error stats" >:: test_get_error_stats;
         ]
end

let () = run_test_tt_main Test_error_handling.suite
