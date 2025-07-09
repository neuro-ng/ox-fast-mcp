open! Core
open! Async
open! Expect_test_helpers_core
open! Log_types
open! Ox_fast_mcp.Settings

let%expect_test "test log level conversion" =
  let test_cases =
    [
      ("DEBUG", Log_types.Level.Debug);
      ("INFO", Log_types.Level.Info);
      ("WARNING", Log_types.Level.Warning);
      ("ERROR", Log_types.Level.Error);
      ("CRITICAL", Log_types.Level.Critical);
    ]
  in

  List.iter test_cases ~f:(fun (str, expected) ->
      let expected_str = Log_types.Level.to_string expected in
      require ~here:[%here]
        (String.equal str expected_str)
        ~if_false_then_print_s:
          (lazy
            (let error_message_string =
               sprintf "Expected %s to convert to string %s but got %s"
                 (Log_types.Level.to_string expected)
                 str expected_str
             in
             [%sexp (error_message_string : string)])));

  List.iter test_cases ~f:(fun (_str, expected) ->
      let expected_str = Log_types.Level.to_string expected in
      print_s [%sexp (expected_str : string)]);
  [%expect {|
    DEBUG
    INFO
    WARNING
    ERROR
    CRITICAL
  |}];
  return ()

let%expect_test "test log invalid level comparison" =
  (* Test invalid input *)
  show_raise (fun () -> Log_types.Level.of_string "INVALID");
  [%expect {| (raised (Failure "Invalid log level: INVALID")) |}];
  return ()

let%expect_test "test duplicate behavior conversion" =
  let test_cases =
    [
      ("warn", Duplicate_behavior.Warn);
      ("error", Duplicate_behavior.Error);
      ("replace", Duplicate_behavior.Replace);
      ("ignore", Duplicate_behavior.Ignore);
    ]
  in
  List.iter test_cases ~f:(fun (str, expected) ->
      let expected_str = Duplicate_behavior.to_string expected in
      require ~here:[%here]
        (String.equal str expected_str)
        ~if_false_then_print_s:
          (lazy
            (let error_message_string =
               sprintf "Expected %s to convert to string %s but got %s"
                 (Duplicate_behavior.to_string expected)
                 str expected_str
             in
             [%sexp (error_message_string : string)])));

  List.iter test_cases ~f:(fun (_str, expected) ->
      let expected_str = Duplicate_behavior.to_string expected in
      print_s [%sexp (expected_str : string)]);
  [%expect {|
    warn
    error
    replace
    ignore
  |}];
  return ()

let%expect_test "test duplicate behavior invalid input" =
  (* Test invalid input *)
  show_raise (fun () -> Duplicate_behavior.of_string "INVALID");
  [%expect {| (raised (Failure "Invalid duplicate behavior: INVALID")) |}];
  return ()

let%expect_test "test settings creation" =
  let settings =
    Settings.create ~home:"/test/home" ~test_mode:true
      ~log_level:Log_level.Debug ~port:8080 ()
  in
  print_s [%sexp (settings : Settings.t)];
  [%expect
    {|
    ((home                                    /test/home)
     (test_mode                               true)
     (log_level                               Debug)
     (enable_rich_tracebacks                  true)
     (deprecation_warnings                    true)
     (client_raise_first_exceptiongroup_error true)
     (resource_prefix_format                  Path)
     (client_init_timeout ())
     (host                 127.0.0.1)
     (port                 8080)
     (sse_path             /sse/)
     (message_path         /messages/)
     (streamable_http_path /mcp/)
     (debug                false)
     (mask_error_details   false)
     (server_dependencies ())
     (json_response  false)
     (stateless_http false)
     (default_auth_provider ())
     (include_tags          ())
     (exclude_tags          ())) 
    |}];
  return ()

let%expect_test "test settings json serialization" =
  let settings =
    Settings.create ~home:"/test/home" ~test_mode:true
      ~log_level:Log_level.Debug ~port:8080 ()
  in
  let json = Settings.yojson_of_t settings in
  print_s [%sexp (Yojson.Safe.to_string json : string)];
  [%expect
    {| "{\"home\":\"/test/home\",\"test_mode\":true,\"log_level\":[\"Debug\"],\"enable_rich_tracebacks\":true,\"deprecation_warnings\":true,\"client_raise_first_exceptiongroup_error\":true,\"resource_prefix_format\":[\"Path\"],\"client_init_timeout\":null,\"host\":\"127.0.0.1\",\"port\":8080,\"sse_path\":\"/sse/\",\"message_path\":\"/messages/\",\"streamable_http_path\":\"/mcp/\",\"debug\":false,\"mask_error_details\":false,\"server_dependencies\":[],\"json_response\":false,\"stateless_http\":false,\"default_auth_provider\":null,\"include_tags\":null,\"exclude_tags\":null}" |}];

  return ()

let%expect_test "test settings validation empty home" =
  let settings = Settings.create ~home:"" () in
  show_raise (fun () -> Settings.validate settings);
  [%expect {| (raised (Failure "Home path cannot be empty")) |}];
  return ()

let%expect_test "test settings validation invalid port" =
  let settings = Settings.create ~port:(-1) () in
  show_raise (fun () -> Settings.validate settings);
  [%expect {| (raised (Failure "Invalid port number: -1")) |}];
  return ()

let%expect_test "test environment variable loading" =
  Unix.putenv ~key:"OXFASTMCP_HOME" ~data:"/env/home";
  Unix.putenv ~key:"OXFASTMCP_PORT" ~data:"9000";
  Unix.putenv ~key:"OXFASTMCP_TEST_MODE" ~data:"true";

  let initial_settings = Settings.create () in
  let loaded_settings_result = Settings.load_from_env initial_settings in
  let settings = Or_error.ok_exn loaded_settings_result in

  print_s [%sexp (settings : Settings.t)];
  [%expect
    {|
    ((home                                    /env/home)
     (test_mode                               true)
     (log_level                               Info)
     (enable_rich_tracebacks                  true)
     (deprecation_warnings                    true)
     (client_raise_first_exceptiongroup_error true)
     (resource_prefix_format                  Path)
     (client_init_timeout ())
     (host                 127.0.0.1)
     (port                 9000)
     (sse_path             /sse/)
     (message_path         /messages/)
     (streamable_http_path /mcp/)
     (debug                false)
     (mask_error_details   false)
     (server_dependencies ())
     (json_response  false)
     (stateless_http false)
     (default_auth_provider ())
     (include_tags          ())
     (exclude_tags          ())) 
    |}];
  return ()
