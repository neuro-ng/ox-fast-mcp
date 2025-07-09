open Core
open Async
open Expect_test_helpers_core
open Settings

let%expect_test "test log level conversion" =
  let test_cases =
    [
      ("DEBUG", Log_level.Debug);
      ("INFO", Log_level.Info);
      ("WARNING", Log_level.Warning);
      ("ERROR", Log_level.Error);
      ("CRITICAL", Log_level.Critical);
    ]
  in
  List.iter test_cases ~f:(fun (str, expected) ->
      match Log_level.of_string str with
      | Ok level ->
        require [%here] (level = expected)
          ~if_false:(sprintf "Expected %s to convert to correct log level" str);
        print_s [%sexp (level : Log_level.t)];
        [%expect {| Debug |}] (* Update with actual expected output *)
      | Error msg ->
        print_cr [%here] [%message "Unexpected error" (msg : string)];
        [%expect.unreachable]);
  (* Test invalid input *)
  match Log_level.of_string "INVALID" with
  | Ok _ -> [%expect.unreachable]
  | Error msg ->
    print_s [%sexp (msg : string)];
    [%expect {| "Invalid log level: INVALID" |}]

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
      match Duplicate_behavior.of_string str with
      | Ok behavior ->
        require [%here] (behavior = expected)
          ~if_false:(sprintf "Expected %s to convert to correct behavior" str);
        print_s [%sexp (behavior : Duplicate_behavior.t)];
        [%expect {| Warn |}] (* Update with actual expected output *)
      | Error msg ->
        print_cr [%here] [%message "Unexpected error" (msg : string)];
        [%expect.unreachable]);
  (* Test invalid input *)
  match Duplicate_behavior.of_string "INVALID" with
  | Ok _ -> [%expect.unreachable]
  | Error msg ->
    print_s [%sexp (msg : string)];
    [%expect {| "Invalid duplicate behavior: INVALID" |}]

let%expect_test "test settings creation" =
  let settings =
    Settings.create ~home:"/test/home" ~test_mode:true
      ~log_level:Log_level.Debug ~port:8080 ()
  in
  print_s [%sexp (settings : Settings.t)];
  [%expect
    {|
    ((home /test/home)
     (test_mode true)
     (log_level Debug)
     (enable_rich_tracebacks true)
     (deprecation_warnings true)
     (client_raise_first_exceptiongroup_error true)
     (resource_prefix_format Path)
     (client_init_timeout ())
     (host 127.0.0.1)
     (port 8080)
     (sse_path /sse/)
     (message_path /messages/)
     (streamable_http_path /mcp/)
     (debug false)
     (mask_error_details false)
     (server_dependencies ())
     (json_response false)
     (stateless_http false)
     (default_auth_provider None)
     (include_tags ())
     (exclude_tags ())) |}]

let%expect_test "test settings json serialization" =
  let settings =
    Settings.create ~home:"/test/home" ~test_mode:true
      ~log_level:Log_level.Debug ~port:8080 ()
  in
  let json = Settings.yojson_of_t settings in
  print_s [%sexp (Yojson.Safe.to_string json : string)];
  [%expect
    {|
    "{\"home\":\"/test/home\",\"test_mode\":true,\"log_level\":\"Debug\",\
      \"enable_rich_tracebacks\":true,\"deprecation_warnings\":true,\
      \"client_raise_first_exceptiongroup_error\":true,\
      \"resource_prefix_format\":\"Path\",\"client_init_timeout\":null,\
      \"host\":\"127.0.0.1\",\"port\":8080,\"sse_path\":\"/sse/\",\
      \"message_path\":\"/messages/\",\"streamable_http_path\":\"/mcp/\",\
      \"debug\":false,\"mask_error_details\":false,\"server_dependencies\":[],\
      \"json_response\":false,\"stateless_http\":false,\
      \"default_auth_provider\":\"None\",\"include_tags\":null,\
      \"exclude_tags\":null}" |}];
  match Settings.t_of_yojson json with
  | Ok deserialized ->
    require [%here] (settings = deserialized)
      ~if_false:"Deserialized settings should match original";
    [%expect {| |}]
  | Error msg ->
    print_cr [%here]
      [%message "Unexpected deserialization error" (msg : string)];
    [%expect.unreachable]

let%expect_test "test settings validation" =
  let settings =
    Settings.create ~home:"" (* Invalid: empty home path *)
      ~port:(-1) (* Invalid: negative port *)
      ()
  in
  match Settings.validate settings with
  | Ok () -> [%expect.unreachable]
  | Error errors ->
    print_s [%sexp (errors : Settings_error.t list)];
    [%expect
      {|
      ((Invalid_value ((field home) (message "Home path cannot be empty")))
       (Invalid_value ((field port) (message "Port must be between 0 and 65535")))) |}]

let%expect_test "test environment variable loading" =
  Unix.putenv ~key:"FASTMCP_HOME" ~data:"/env/home";
  Unix.putenv ~key:"FASTMCP_PORT" ~data:"9000";
  Unix.putenv ~key:"FASTMCP_TEST_MODE" ~data:"true";
  let settings = Settings.load_from_env () in
  print_s [%sexp (settings : Settings.t)];
  [%expect
    {|
    ((home /env/home)
     (test_mode true)
     (log_level Info)
     (enable_rich_tracebacks true)
     (deprecation_warnings true)
     (client_raise_first_exceptiongroup_error true)
     (resource_prefix_format Path)
     (client_init_timeout ())
     (host 127.0.0.1)
     (port 9000)
     (sse_path /sse/)
     (message_path /messages/)
     (streamable_http_path /mcp/)
     (debug false)
     (mask_error_details false)
     (server_dependencies ())
     (json_response false)
     (stateless_http false)
     (default_auth_provider None)
     (include_tags ())
     (exclude_tags ())) |}]
