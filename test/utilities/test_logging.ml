open! Core
open! Async
open! Expect_test_helpers_core
open! Logging

module Test_handler : Log_types.Handler = struct
  type t = string Queue.t
end

let%expect_test "Log_level.of_string handles valid levels" =
  let test_level str =
    let level = Log_types.Level.of_string str in
    print_s [%sexp (Log_types.Level.to_string level : string)]
  in
  test_level "DEBUG";
  [%expect {| DEBUG |}];
  test_level "INFO";
  [%expect {| INFO |}];
  test_level "WARNING";
  [%expect {| WARNING |}];
  test_level "ERROR";
  [%expect {| ERROR |}];
  test_level "CRITICAL";
  [%expect {| CRITICAL |}];
  return ()

let%expect_test "Log_level.of_string handles invalid levels" =
  show_raise (fun () -> Log_types.Level.of_string "INVALID");
  [%expect {| (raised (Failure "Invalid log level: INVALID")) |}];
  return ()

let%expect_test "Log_level comparison works correctly" =
  let test_comparison l1 l2 =
    let level1 = Log_types.Level.of_string l1 in
    let level2 = Log_types.Level.of_string l2 in
    print_s
      [%sexp
        {
          level1 : string = l1;
          level2 : string = l2;
          equal = (Log_types.Level.equal level1 level2 : bool);
          less_than = (Log_types.Level.compare level1 level2 < 0 : bool);
          greater_than = (Log_types.Level.compare level1 level2 > 0 : bool);
        }]
  in
  test_comparison "DEBUG" "INFO";
  [%expect
    {|
    ((level1       DEBUG)
     (level2       INFO)
     (equal        false)
     (less_than    true)
     (greater_than false))
    |}];
  test_comparison "ERROR" "WARNING";
  [%expect
    {|
    ((level1       ERROR)
     (level2       WARNING)
     (equal        false)
     (less_than    false)
     (greater_than true))
    |}];
  return ()

let%expect_test "Logger.create sets correct defaults" =
  let logger = Logger.create "test" in
  print_s [%sexp (Log_types.Level.to_string (Logger.get_level logger) : string)];
  [%expect {| INFO |}];
  return ()

let%expect_test "Logger.get_logger creates nested loggers" =
  let logger = Logger.get_logger "test" in
  print_s [%sexp (Logger.get_name logger : string)];
  [%expect {| OxFastMCP.test |}];
  return ()

(* Helper function to create a test logger with captured output *)
let create_test_logger_with_capture level =
  let buffer = Queue.create () in
  let logger = Logger.create ~level "test" in
  Logger.add_handler logger (module Test_handler);
  let handler = List.hd_exn (Logger.get_handlers logger) in
  handler.instance.log <- (fun ~level ~msg -> 
    Queue.enqueue buffer (sprintf "[%s] %s" (Log_types.Level.to_string level) msg));
  (logger, buffer)

let%expect_test "Logger.debug logs debug messages when level permits" =
  let logger, buffer = create_test_logger_with_capture Log_types.Level.Debug in
  Logger.debug logger "debug message";
  print_s [%sexp (Queue.to_list buffer : string list)];
  [%expect {| ("[DEBUG] debug message") |}];
  return ()

let%expect_test "Logger.debug ignores messages when level too high" =
  let logger, buffer = create_test_logger_with_capture Log_types.Level.Info in
  Logger.debug logger "debug message";
  print_s [%sexp (Queue.to_list buffer : string list)];
  [%expect {| () |}];
  return ()

let%expect_test "Logger.info logs info messages when level permits" =
  let logger, buffer = create_test_logger_with_capture Log_types.Level.Debug in
  Logger.info logger "info message";
  print_s [%sexp (Queue.to_list buffer : string list)];
  [%expect {| ("[INFO] info message") |}];
  return ()

let%expect_test "Logger.info logs info messages at info level" =
  let logger, buffer = create_test_logger_with_capture Log_types.Level.Info in
  Logger.info logger "info message";
  print_s [%sexp (Queue.to_list buffer : string list)];
  [%expect {| ("[INFO] info message") |}];
  return ()

let%expect_test "Logger.info ignores messages when level too high" =
  let logger, buffer = create_test_logger_with_capture Log_types.Level.Warning in
  Logger.info logger "info message";
  print_s [%sexp (Queue.to_list buffer : string list)];
  [%expect {| () |}];
  return ()

let%expect_test "Logger.warning logs warning messages when level permits" =
  let logger, buffer = create_test_logger_with_capture Log_types.Level.Debug in
  Logger.warning logger "warning message";
  print_s [%sexp (Queue.to_list buffer : string list)];
  [%expect {| ("[WARNING] warning message") |}];
  return ()

let%expect_test "Logger.warning logs warning messages at warning level" =
  let logger, buffer = create_test_logger_with_capture Log_types.Level.Warning in
  Logger.warning logger "warning message";
  print_s [%sexp (Queue.to_list buffer : string list)];
  [%expect {| ("[WARNING] warning message") |}];
  return ()

let%expect_test "Logger.warning ignores messages when level too high" =
  let logger, buffer = create_test_logger_with_capture Log_types.Level.Error in
  Logger.warning logger "warning message";
  print_s [%sexp (Queue.to_list buffer : string list)];
  [%expect {| () |}];
  return ()

let%expect_test "Logger.error logs error messages when level permits" =
  let logger, buffer = create_test_logger_with_capture Log_types.Level.Debug in
  Logger.error logger "error message";
  print_s [%sexp (Queue.to_list buffer : string list)];
  [%expect {| ("[ERROR] error message") |}];
  return ()

let%expect_test "Logger.error logs error messages at error level" =
  let logger, buffer = create_test_logger_with_capture Log_types.Level.Error in
  Logger.error logger "error message";
  print_s [%sexp (Queue.to_list buffer : string list)];
  [%expect {| ("[ERROR] error message") |}];
  return ()

let%expect_test "Logger.error ignores messages when level too high" =
  let logger, buffer = create_test_logger_with_capture Log_types.Level.Critical in
  Logger.error logger "error message";
  print_s [%sexp (Queue.to_list buffer : string list)];
  [%expect {| () |}];
  return ()

let%expect_test "Logger.critical logs critical messages when level permits" =
  let logger, buffer = create_test_logger_with_capture Log_types.Level.Debug in
  Logger.critical logger "critical message";
  print_s [%sexp (Queue.to_list buffer : string list)];
  [%expect {| ("[CRITICAL] critical message") |}];
  return ()

let%expect_test "Logger.critical logs critical messages at critical level" =
  let logger, buffer = create_test_logger_with_capture Log_types.Level.Critical in
  Logger.critical logger "critical message";
  print_s [%sexp (Queue.to_list buffer : string list)];
  [%expect {| ("[CRITICAL] critical message") |}];
  return ()

let%expect_test "Logger methods with multiple messages" =
  let logger, buffer = create_test_logger_with_capture Log_types.Level.Debug in
  Logger.debug logger "debug 1";
  Logger.info logger "info 1";
  Logger.warning logger "warning 1";
  Logger.error logger "error 1";
  Logger.critical logger "critical 1";
  print_s [%sexp (Queue.to_list buffer : string list)];
  [%expect {|
    ("[DEBUG] debug 1"
     "[INFO] info 1"
     "[WARNING] warning 1"
     "[ERROR] error 1"
     "[CRITICAL] critical 1")
    |}];
  return ()

let%expect_test "Logger level filtering with multiple messages" =
  let logger, buffer = create_test_logger_with_capture Log_types.Level.Warning in
  Logger.debug logger "debug 1";
  Logger.info logger "info 1";
  Logger.warning logger "warning 1";
  Logger.error logger "error 1";
  Logger.critical logger "critical 1";
  print_s [%sexp (Queue.to_list buffer : string list)];
  [%expect {| ("[WARNING] warning 1" "[ERROR] error 1" "[CRITICAL] critical 1") |}];
  return ()

let%expect_test "Logger with custom level configuration" =
  let logger_debug = Logger.create ~level:Log_types.Level.Debug "debug_logger" in
  let logger_error = Logger.create ~level:Log_types.Level.Error "error_logger" in
  
  print_s [%sexp {
    debug_logger_level = (Log_types.Level.to_string (Logger.get_level logger_debug) : string);
    error_logger_level = (Log_types.Level.to_string (Logger.get_level logger_error) : string);
  }];
  [%expect {|
    ((debug_logger_level DEBUG)
     (error_logger_level ERROR))
  |}];
  return ()

let%expect_test "Logger.get_handlers returns correct handler list" =
  let logger = Logger.create "test" in
  let initial_handlers = Logger.get_handlers logger in
  Logger.add_handler logger (module Test_handler);
  let handlers_after_add = Logger.get_handlers logger in
  print_s [%sexp {
    initial_count = (List.length initial_handlers : int);
    after_add_count = (List.length handlers_after_add : int);
  }];
  [%expect {|
    ((initial_count   0)
     (after_add_count 1))
  |}];
  return ()

let%expect_test "Logger handles messages based on level" =
  let buffer = Queue.create () in
  let logger = Logger.create "test" in
  Logger.add_handler logger (module Test_handler);
  let handler = List.hd_exn (Logger.get_handlers logger) in
  handler.instance.log <- (fun ~level:_ ~msg -> Queue.enqueue buffer msg);
  Logger.info logger "test message";
  print_s [%sexp (Queue.to_list buffer : string list)];
  [%expect {| ("test message") |}];
  return ()

let%expect_test "Rich_handler formats messages correctly" =
  let handler = Rich_handler.create () in
  let formatted =
    Rich_handler.format handler ~level:Log_types.Level.Info ~msg:"test"
  in
  print_s [%sexp (formatted : string)];
  [%expect {| test |}];
  return ()

let%expect_test "configure_logging sets up logger correctly" =
  let logger = configure_logging () in
  print_s
    [%sexp
      {
        name = (Logger.get_name logger : string);
        level = (Log_types.Level.to_string (Logger.get_level logger) : string);
        has_handlers = (not (List.is_empty (Logger.get_handlers logger)) : bool);
      }];
  [%expect
    {|
    ((name         OxFastMCP)
     (level        INFO)
     (has_handlers true))
    |}];
  return ()

let%expect_test "Global logger functions work correctly" =
  (* Test that global functions execute without crashing *)
  (* Note: Global logger outputs to stderr with timestamps, so we only verify execution *)
  let test_result = ref "not started" in
  (try
     Global.debug "global debug";
     Global.info "global info";
     Global.warning "global warning";
     Global.error "global error";
     Global.critical "global critical";
     test_result := "success"
   with
   | exn -> test_result := sprintf "failed: %s" (Exn.to_string exn));
  printf "Global logging test result: %s\n" !test_result;
  [%expect {|
    DEBUG global debug
    INFO global info
    WARNING global warning
    ERROR global error
    CRITICAL global critical
    Global logging test result: success
    |}];
  return ()

let%expect_test "Global logger timestamp configuration works correctly" =
  (* Test that configuration functions execute without errors *)
  (try
     (* Test configuring without timestamp (default) *)
     Global.configure ~with_timestamp:false ();
     printf "✓ Configured without timestamp\n";
     
     (* Test configuring with timestamp *)
     Global.configure ~with_timestamp:true ();
     printf "✓ Configured with timestamp\n";
     
     (* Reset to default (without timestamp) for other tests *)
     Global.configure ~with_timestamp:false ();
     printf "✓ Reset to no timestamp\n";
     
     printf "✓ All timestamp configuration tests passed\n";
   with
   | exn -> printf "✗ Timestamp configuration failed: %s\n" (Exn.to_string exn));
  [%expect {|
    ✓ Configured without timestamp
    ✓ Configured with timestamp
    ✓ Reset to no timestamp
    ✓ All timestamp configuration tests passed
    |}];
  return ()
  