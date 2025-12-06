open! Core
open! Async
open! Expect_test_helpers_core
open! Logging
open! Ox_fast_mcp.Settings

let%expect_test "test log level conversion" =
  let test_cases =
    [
      ("DEBUG", Logging.Level.Debug);
      ("INFO", Logging.Level.Info);
      ("WARNING", Logging.Level.Warning);
      ("ERROR", Logging.Level.Error);
      ("CRITICAL", Logging.Level.Critical);
    ]
  in

  List.iter test_cases ~f:(fun (str, expected) ->
      let expected_str = Logging.Level.to_string expected in
      require ~here:[%here]
        (String.equal str expected_str)
        ~if_false_then_print_s:
          (lazy
            (let error_message_string =
               sprintf "Expected %s to convert to string %s but got %s"
                 (Logging.Level.to_string expected)
                 str expected_str
             in
             [%sexp (error_message_string : string)])));

  List.iter test_cases ~f:(fun (_str, expected) ->
      let expected_str = Logging.Level.to_string expected in
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
  show_raise (fun () -> Logging.Level.of_string "INVALID");
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
  Unix.putenv ~key:"FASTMCP_HOME" ~data:"/env/home";
  Unix.putenv ~key:"FASTMCP_PORT" ~data:"9000";
  Unix.putenv ~key:"FASTMCP_TEST_MODE" ~data:"true";

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

let%expect_test "test dotenv file loading with basic key=value" =
  let test_env_content =
    "FASTMCP_HOME=/dotenv/home\n" ^ "FASTMCP_PORT=8888\n"
    ^ "FASTMCP_TEST_MODE=true\n" ^ "FASTMCP_DEBUG=true\n"
  in

  (* Create temporary .env file *)
  let%bind temp_dir = Unix.mkdtemp "test_dotenv" in
  let env_file = Filename.concat temp_dir ".env" in
  let%bind () = Writer.save env_file ~contents:test_env_content in

  (* Change to temp directory to test .env loading *)
  let%bind original_cwd = Unix.getcwd () in
  let%bind () = Unix.chdir temp_dir in

  let initial_settings = Settings.create () in
  let%bind loaded_settings_result =
    Settings.load_from_dotenv initial_settings
  in
  let settings = Or_error.ok_exn loaded_settings_result in

  print_s [%sexp (settings : Settings.t)];
  [%expect
    {|
    ((home                   /dotenv/home)
     (test_mode              true)
     (log_level              Info)
     (enable_rich_tracebacks true)
     (deprecation_warnings   true)
     (client_raise_first_exceptiongroup_error true)
     (resource_prefix_format                  Path)
     (client_init_timeout ())
     (host                 127.0.0.1)
     (port                 8888)
     (sse_path             /sse/)
     (message_path         /messages/)
     (streamable_http_path /mcp/)
     (debug                true)
     (mask_error_details   false)
     (server_dependencies ())
     (json_response  false)
     (stateless_http false)
     (default_auth_provider ())
     (include_tags          ())
     (exclude_tags          ()))
    |}];

  (* Cleanup *)
  let%bind () = Unix.chdir original_cwd in
  let%bind _output =
    Process.run_exn ~prog:"rm" ~args:[ "-rf"; temp_dir ]
      ~working_dir:original_cwd ()
  in
  return ()

let%expect_test "test dotenv file loading with quoted values" =
  let test_env_content =
    "FASTMCP_HOME=\"/quoted/home\"\n" ^ "FASTMCP_HOST='localhost'\n"
    ^ "FASTMCP_SSE_PATH=\"/quoted/sse/\"\n" ^ "FASTMCP_PORT=7777\n"
  in

  (* Create temporary .env file *)
  let%bind temp_dir = Unix.mkdtemp "test_dotenv_quotes" in
  let env_file = Filename.concat temp_dir ".env" in
  let%bind () = Writer.save env_file ~contents:test_env_content in

  (* Change to temp directory to test .env loading *)
  let%bind original_cwd = Unix.getcwd () in
  let%bind () = Unix.chdir temp_dir in

  let initial_settings = Settings.create () in
  let%bind loaded_settings_result =
    Settings.load_from_dotenv initial_settings
  in
  let settings = Or_error.ok_exn loaded_settings_result in

  print_s [%sexp (settings.home : string)];
  print_s [%sexp (settings.host : string)];
  print_s [%sexp (settings.sse_path : string)];
  print_s [%sexp (settings.port : int)];
  [%expect {|
    /quoted/home
    localhost
    /quoted/sse/
    7777
    |}];

  (* Cleanup *)
  let%bind () = Unix.chdir original_cwd in
  let%bind _output =
    Process.run_exn ~prog:"rm" ~args:[ "-rf"; temp_dir ]
      ~working_dir:original_cwd ()
  in
  return ()

let%expect_test "test dotenv file loading with comments and empty lines" =
  let test_env_content =
    "# This is a comment\n" ^ "\n" ^ "FASTMCP_HOME=/comment/home\n"
    ^ "# Another comment\n" ^ "\n" ^ "FASTMCP_PORT=6666\n"
    ^ "   # Indented comment\n" ^ "FASTMCP_TEST_MODE=false\n"
  in

  (* Create temporary .env file *)
  let%bind temp_dir = Unix.mkdtemp "test_dotenv_comments" in
  let env_file = Filename.concat temp_dir ".env" in
  let%bind () = Writer.save env_file ~contents:test_env_content in

  (* Change to temp directory to test .env loading *)
  let%bind original_cwd = Unix.getcwd () in
  let%bind () = Unix.chdir temp_dir in

  let initial_settings = Settings.create () in
  let%bind loaded_settings_result =
    Settings.load_from_dotenv initial_settings
  in
  let settings = Or_error.ok_exn loaded_settings_result in

  print_s [%sexp (settings.home : string)];
  print_s [%sexp (settings.port : int)];
  print_s [%sexp (settings.test_mode : bool)];
  [%expect {|
    /comment/home
    6666
    false
    |}];

  (* Cleanup *)
  let%bind () = Unix.chdir original_cwd in
  let%bind _output =
    Process.run_exn ~prog:"rm" ~args:[ "-rf"; temp_dir ]
      ~working_dir:original_cwd ()
  in
  return ()

let%expect_test "test dotenv file loading with values containing equals" =
  let test_env_content =
    "FASTMCP_HOME=/path/with=equals\n"
    ^ "FASTMCP_HOST=example.com:8080=admin\n" ^ "FASTMCP_PORT=5555\n"
  in

  (* Create temporary .env file *)
  let%bind temp_dir = Unix.mkdtemp "test_dotenv_equals" in
  let env_file = Filename.concat temp_dir ".env" in
  let%bind () = Writer.save env_file ~contents:test_env_content in

  (* Change to temp directory to test .env loading *)
  let%bind original_cwd = Unix.getcwd () in
  let%bind () = Unix.chdir temp_dir in

  let initial_settings = Settings.create () in
  let%bind loaded_settings_result =
    Settings.load_from_dotenv initial_settings
  in
  let settings = Or_error.ok_exn loaded_settings_result in

  print_s [%sexp (settings.home : string)];
  print_s [%sexp (settings.host : string)];
  [%expect {|
    /path/with=equals
    example.com:8080=admin
    |}];

  (* Cleanup *)
  let%bind () = Unix.chdir original_cwd in
  let%bind _output =
    Process.run_exn ~prog:"rm" ~args:[ "-rf"; temp_dir ]
      ~working_dir:original_cwd ()
  in
  return ()

let%expect_test "test dotenv file loading when file does not exist" =
  (* Change to a directory without .env file *)
  let%bind temp_dir = Unix.mkdtemp "test_no_dotenv" in
  let%bind original_cwd = Unix.getcwd () in
  let%bind () = Unix.chdir temp_dir in

  let initial_settings = Settings.create ~home:"/original/home" () in
  let%bind loaded_settings_result =
    Settings.load_from_dotenv initial_settings
  in
  let settings = Or_error.ok_exn loaded_settings_result in

  (* Should return original settings unchanged *)
  print_s [%sexp (settings.home : string)];
  [%expect {| /original/home |}];

  (* Cleanup *)
  let%bind () = Unix.chdir original_cwd in
  let%bind _output =
    Process.run_exn ~prog:"rm" ~args:[ "-rf"; temp_dir ]
      ~working_dir:original_cwd ()
  in
  return ()

let%expect_test "test file secrets loading" =
  let%bind temp_dir = Unix.mkdtemp "test_file_secrets" in
  let home_dir = temp_dir in
  let secrets_dir = Filename.concat home_dir "secrets" in

  (* Create secrets directory and files *)
  let%bind () = Unix.mkdir secrets_dir in
  let%bind () =
    Writer.save
      (Filename.concat secrets_dir "FASTMCP_HOME")
      ~contents:"/secret/home"
  in
  let%bind () =
    Writer.save (Filename.concat secrets_dir "FASTMCP_PORT") ~contents:"9999"
  in
  let%bind () =
    Writer.save
      (Filename.concat secrets_dir "FASTMCP_TEST_MODE")
      ~contents:"true"
  in
  let%bind () =
    Writer.save (Filename.concat secrets_dir "FASTMCP_DEBUG") ~contents:"true"
  in

  let initial_settings = Settings.create ~home:home_dir () in
  let loaded_settings_result =
    Settings.load_from_file_secrets initial_settings
  in
  let settings = Or_error.ok_exn loaded_settings_result in

  print_s [%sexp (settings.home : string)];
  print_s [%sexp (settings.port : int)];
  print_s [%sexp (settings.test_mode : bool)];
  print_s [%sexp (settings.debug : bool)];
  [%expect {|
    /secret/home
    9999
    true
    true
    |}];

  (* Cleanup *)
  let%bind current_dir = Unix.getcwd () in
  let%bind _output =
    Process.run_exn ~prog:"rm" ~args:[ "-rf"; temp_dir ]
      ~working_dir:current_dir ()
  in
  return ()

let%expect_test "test file secrets loading when secrets dir does not exist" =
  let%bind temp_dir = Unix.mkdtemp "test_no_secrets" in
  let home_dir = temp_dir in

  let initial_settings = Settings.create ~home:home_dir ~port:3333 () in
  let loaded_settings_result =
    Settings.load_from_file_secrets initial_settings
  in
  let settings = Or_error.ok_exn loaded_settings_result in

  (* Should return original settings unchanged *)
  let home_matches_temp_dir =
    String.is_prefix settings.home ~prefix:"test_no_secrets"
  in
  print_s [%sexp (home_matches_temp_dir : bool)];
  print_s [%sexp (settings.port : int)];
  [%expect {|
    true
    3333
    |}];

  (* Cleanup *)
  let%bind current_dir = Unix.getcwd () in
  let%bind _output =
    Process.run_exn ~prog:"rm" ~args:[ "-rf"; temp_dir ]
      ~working_dir:current_dir ()
  in
  return ()

let%expect_test "test file secrets loading with mixed files and directories" =
  let%bind temp_dir = Unix.mkdtemp "test_mixed_secrets" in
  let home_dir = temp_dir in
  let secrets_dir = Filename.concat home_dir "secrets" in

  (* Create secrets directory with files and subdirectory *)
  let%bind () = Unix.mkdir secrets_dir in
  let%bind () =
    Writer.save
      (Filename.concat secrets_dir "FASTMCP_HOME")
      ~contents:"/mixed/home"
  in
  let%bind () =
    Writer.save (Filename.concat secrets_dir "FASTMCP_PORT") ~contents:"4444"
  in
  let%bind () = Unix.mkdir (Filename.concat secrets_dir "subdir") in
  (* Should be skipped *)
  let%bind () =
    Writer.save
      (Filename.concat secrets_dir "FASTMCP_DEBUG")
      ~contents:"false"
  in

  let initial_settings = Settings.create ~home:home_dir () in
  let loaded_settings_result =
    Settings.load_from_file_secrets initial_settings
  in
  let settings = Or_error.ok_exn loaded_settings_result in

  print_s [%sexp (settings.home : string)];
  print_s [%sexp (settings.port : int)];
  print_s [%sexp (settings.debug : bool)];
  [%expect {|
    /mixed/home
    4444
    false
    |}];

  (* Cleanup *)
  let%bind current_dir = Unix.getcwd () in
  let%bind _output =
    Process.run_exn ~prog:"rm" ~args:[ "-rf"; temp_dir ]
      ~working_dir:current_dir ()
  in
  return ()

let%expect_test "test deprecated environment variable warning" =
  (* Set deprecated prefix variable *)
  Unix.putenv ~key:"OX_FASTMCP_SERVER_HOME" ~data:"/deprecated/home";

  let initial_settings = Settings.create () in
  let loaded_settings_result = Settings.load_from_env initial_settings in
  let settings = Or_error.ok_exn loaded_settings_result in

  (* Should load the value but the warning would be printed to stderr *)
  print_s [%sexp (settings.home : string)];
  [%expect {| /env/home |}];

  (* Clean up environment variable *)
  Unix.unsetenv "OX_FASTMCP_SERVER_HOME";
  return ()

let%expect_test "test settings loading priority order" =
  let%bind temp_dir = Unix.mkdtemp "test_priority" in
  let home_dir = temp_dir in
  let secrets_dir = Filename.concat home_dir "secrets" in

  (* Set environment variable *)
  Unix.putenv ~key:"FASTMCP_PORT" ~data:"1111";

  (* Create .env file *)
  let env_file = Filename.concat temp_dir ".env" in
  let%bind () = Writer.save env_file ~contents:"FASTMCP_PORT=2222\n" in

  (* Create secrets file *)
  let%bind () = Unix.mkdir secrets_dir in
  let%bind () =
    Writer.save (Filename.concat secrets_dir "FASTMCP_PORT") ~contents:"3333"
  in

  let%bind original_cwd = Unix.getcwd () in
  let%bind () = Unix.chdir temp_dir in

  let initial_settings = Settings.create ~home:home_dir ~port:4444 () in

  (* Test environment loading (should override initial) *)
  let env_loaded = Settings.load_from_env initial_settings |> Or_error.ok_exn in
  print_s [%sexp (env_loaded.port : int)];
  [%expect {| 1111 |}];

  (* Test dotenv loading (should override initial) *)
  let%bind dotenv_loaded =
    Settings.load_from_dotenv initial_settings >>| Or_error.ok_exn
  in
  print_s [%sexp (dotenv_loaded.port : int)];
  [%expect {| 2222 |}];

  (* Test secrets loading (should override initial) *)
  let secrets_loaded =
    Settings.load_from_file_secrets initial_settings |> Or_error.ok_exn
  in
  print_s [%sexp (secrets_loaded.port : int)];
  [%expect {| 4444 |}];

  (* Cleanup *)
  let%bind () = Unix.chdir original_cwd in
  Unix.unsetenv "FASTMCP_PORT";
  let%bind _output =
    Process.run_exn ~prog:"rm" ~args:[ "-rf"; temp_dir ]
      ~working_dir:original_cwd ()
  in
  return ()

let%expect_test "test settings merge functionality" =
  let base_settings =
    Settings.create ~home:"/base/home" ~port:8000 ~log_level:Log_level.Info
      ~resource_prefix_format:Resource_prefix_format.Path ()
  in

  let override_settings =
    Settings.create ~home:"/override/home" ~port:9000 ~log_level:Log_level.Debug
      ~resource_prefix_format:Resource_prefix_format.Protocol ()
  in

  let merged = Settings.merge base_settings override_settings in

  (* Test that merge applies override values for specific fields *)
  print_s [%sexp (merged.log_level : Log_level.t)];
  print_s [%sexp (merged.resource_prefix_format : Resource_prefix_format.t)];
  [%expect {|
    Debug
    Protocol
    |}];

  return ()

let%expect_test "test complex parsing scenarios" =
  let test_env_content =
    "# Complex .env file test\n" ^ "\n"
    ^ "FASTMCP_SERVER_DEPENDENCIES=dep1,dep2,dep3\n"
    ^ "FASTMCP_INCLUDE_TAGS=\"tag1, tag2, tag3\"\n"
    ^ "FASTMCP_EXCLUDE_TAGS='exclude1,exclude2'\n"
    ^ "FASTMCP_CLIENT_INIT_TIMEOUT=30.5\n" ^ "FASTMCP_LOG_LEVEL=DEBUG\n"
    ^ "FASTMCP_RESOURCE_PREFIX_FORMAT=protocol\n"
  in

  (* Create temporary .env file *)
  let%bind temp_dir = Unix.mkdtemp "test_complex_parsing" in
  let env_file = Filename.concat temp_dir ".env" in
  let%bind () = Writer.save env_file ~contents:test_env_content in

  (* Change to temp directory to test .env loading *)
  let%bind original_cwd = Unix.getcwd () in
  let%bind () = Unix.chdir temp_dir in

  let initial_settings = Settings.create () in
  let%bind loaded_settings_result =
    Settings.load_from_dotenv initial_settings
  in
  let settings = Or_error.ok_exn loaded_settings_result in

  print_s [%sexp (settings.server_dependencies : string list)];
  print_s [%sexp (settings.include_tags : string list option)];
  print_s [%sexp (settings.exclude_tags : string list option)];
  print_s [%sexp (settings.client_init_timeout : float option)];
  print_s [%sexp (settings.log_level : Log_level.t)];
  print_s [%sexp (settings.resource_prefix_format : Resource_prefix_format.t)];
  [%expect
    {|
    (dep1 dep2 dep3)
    ((tag1 tag2 tag3))
    ((exclude1 exclude2))
    (30.5)
    Debug
    Protocol
    |}];

  (* Cleanup *)
  let%bind () = Unix.chdir original_cwd in
  let%bind _output =
    Process.run_exn ~prog:"rm" ~args:[ "-rf"; temp_dir ]
      ~working_dir:original_cwd ()
  in
  return ()
