open Alcotest
open Utilities.Logging

(** Buffer to capture log output for testing *)
let log_buffer = Buffer.create 1024

(** Custom log reporter that writes to our test buffer *)
let test_reporter () =
  let report src _level ~over k msgf =
    let src_name = Logs.Src.name src in
    msgf @@ fun ?header:_ ?tags:_ fmt ->
      Format.kasprintf (fun msg ->
        Buffer.add_string log_buffer (Printf.sprintf "[%s] %s\n" src_name msg);
        over ();
        k ()
      ) fmt
  in
  { Logs.report }

(** Clear the log buffer *)
let clear_log_buffer () = Buffer.clear log_buffer

(** Get the current log buffer contents *)
let get_log_buffer () = Buffer.contents log_buffer

(** Check if a string is present in the log buffer *)
let log_contains str = 
  let log = get_log_buffer () in
  try 
    let _ = Str.search_forward (Str.regexp_string str) log 0 in
    true
  with Not_found -> false

(** Setup test environment with our custom reporter *)
let setup_test_logging () =
  Logs.set_reporter (test_reporter ());
  clear_log_buffer ()

(** Test that logging doesn't affect other loggers *)
let test_logging_doesnt_affect_other_loggers () =
  setup_test_logging ();
  
  (* Get original OxFastMCP logger level *)
  let oxfastmcp_src = Logs.Src.create "OxFastMCP" in
  let original_level = Logs.Src.level oxfastmcp_src in
  
  try
    (* Set global log level to Debug to capture all messages *)
    Logs.set_level (Some (to_logs_level Debug));
    
    (* Set OxFastMCP loggers to critical level (effectively disabling info/debug) *)
    Logs.Src.set_level oxfastmcp_src (Some (to_logs_level Critical));
    
    (* Create different loggers *)
    let _root_src = Logs.default in
    let app_src = Logs.Src.create "app" in
    let oxfastmcp_src = Logs.Src.create "OxFastMCP" in
    
    (* Clear buffer before testing *)
    clear_log_buffer ();
    
    (* Log messages from different sources *)
    Logs.info (fun m -> m "--ROOT--");
    Logs.info ~src:app_src (fun m -> m "--APP--");
    Logs.err ~src:oxfastmcp_src (fun m -> m "--OXFASTMCP--");  (* Use error level to ensure it gets through *)
    
    (* Log using OxFastMCP server logger *)
    let server_src = Logs.Src.create "OxFastMCP.server" in
    Logs.err ~src:server_src (fun m -> m "--OXFASTMCP SERVER--");  (* Use error level to ensure it gets through *)
    
    (* Assertions *)
    check bool "--ROOT-- present" true (log_contains "--ROOT--");
    check bool "--APP-- present" true (log_contains "--APP--");
    check bool "--OXFASTMCP-- present" true (log_contains "--OXFASTMCP--");
    check bool "--OXFASTMCP SERVER-- present" true (log_contains "--OXFASTMCP SERVER--");
    
  with
  | exn ->
    (* Restore original level in case of exception *)
    Logs.Src.set_level oxfastmcp_src original_level;
    raise exn

(** Test alternative implementation using string-based log level checking *)
let test_logging_doesnt_affect_other_loggers_alt () =
  setup_test_logging ();
  
  (* Store original log levels *)
  let oxfastmcp_src = Logs.Src.create "OxFastMCP" in
  let server_src = Logs.Src.create "OxFastMCP.server" in
  let original_oxfastmcp_level = Logs.Src.level oxfastmcp_src in
  let original_server_level = Logs.Src.level server_src in
  
  try
    (* Set OxFastMCP related loggers to error level (suppressing info) *)
    Logs.Src.set_level oxfastmcp_src (Some (to_logs_level Error));
    Logs.Src.set_level server_src (Some (to_logs_level Error));
    
    (* Create test sources *)
    let _root_src = Logs.default in
    let app_src = Logs.Src.create "app" in
    
    (* Clear log buffer *)
    clear_log_buffer ();
    
    (* Test info level logging *)
    Logs.info ~src:_root_src (fun m -> m "--ROOT--");
    Logs.info ~src:app_src (fun m -> m "--APP--");
    Logs.info ~src:oxfastmcp_src (fun m -> m "--OXFASTMCP--");
    Logs.info ~src:server_src (fun m -> m "--OXFASTMCP SERVER--");
    
    (* Verify that non-OxFastMCP loggers still work *)
    check bool "root logger works" true (log_contains "--ROOT--");
    check bool "app logger works" true (log_contains "--APP--");
    
    (* Verify that OxFastMCP loggers are suppressed *)
    check bool "oxfastmcp logger suppressed" false (log_contains "--OXFASTMCP--");
    check bool "oxfastmcp server logger suppressed" false (log_contains "--OXFASTMCP SERVER--");
    
  with
  | exn ->
    (* Restore original levels *)
    Logs.Src.set_level oxfastmcp_src original_oxfastmcp_level;
    Logs.Src.set_level server_src original_server_level;
    raise exn

(** Test using the get_logger function specifically *)
let test_get_logger_functionality () =
  setup_test_logging ();
  
  (* Test that get_logger returns a working logger *)
  let module Logger = (val get_logger "test_module") in
  clear_log_buffer ();
  
  (* Set log level to Debug to capture all messages *)
  Logs.set_level (Some (to_logs_level Debug));
  
  (* Test logging at different levels *)
  Logger.info (fun m -> m "Test info message");
  Logger.debug (fun m -> m "Test debug message");
  Logger.err (fun m -> m "Test error message");
  
  (* Check that all messages appear *)
  check bool "info message logged" true (log_contains "Test info message");
  check bool "debug message logged" true (log_contains "Test debug message");
  check bool "error message logged" true (log_contains "Test error message")

(** Helper function to convert log level to string for testing *)
let level_to_string = function
  | Debug -> "debug"
  | Info -> "info"
  | Warning -> "warning"
  | Error -> "error"
  | Critical -> "critical"

(** Test log level hierarchy *)
let test_log_level_hierarchy () =
  setup_test_logging ();
  
  let test_src = Logs.Src.create "test" in
  
  (* Test different log levels *)
  let test_level level expected_count =
    Logs.Src.set_level test_src (Some (to_logs_level level));
    clear_log_buffer ();
    
    (* Try logging at different levels *)
    Logs.debug ~src:test_src (fun m -> m "debug message");
    Logs.info ~src:test_src (fun m -> m "info message");
    Logs.warn ~src:test_src (fun m -> m "warn message");
    Logs.err ~src:test_src (fun m -> m "error message");
    
    let log_output = get_log_buffer () in
    let lines = String.split_on_char '\n' log_output in
    let non_empty_lines = List.filter (fun s -> String.length s > 0) lines in
    
    check int (Printf.sprintf "messages at level %s" (level_to_string level)) 
      expected_count (List.length non_empty_lines)
  in
  
  (* Test that higher levels filter out lower priority messages *)
  test_level Debug 4;  (* All messages *)
  test_level Info 3;   (* Info, warn, error *)
  test_level Warning 2; (* Warn, error *)
  test_level Error 1    (* Error only *)

(** Test that verifies OxFastMCP namespace isolation *)
let test_oxfastmcp_namespace_isolation () =
  setup_test_logging ();
  
  (* Create loggers in and outside OxFastMCP namespace *)
  let oxfastmcp_main = Logs.Src.create "OxFastMCP" in
  let oxfastmcp_server = Logs.Src.create "OxFastMCP.server" in
  let oxfastmcp_client = Logs.Src.create "OxFastMCP.client" in
  let other_logger = Logs.Src.create "other.app" in
  
  (* Set OxFastMCP namespace to error level *)
  Logs.Src.set_level oxfastmcp_main (Some (to_logs_level Error));
  Logs.Src.set_level oxfastmcp_server (Some (to_logs_level Error));
  Logs.Src.set_level oxfastmcp_client (Some (to_logs_level Error));
  
  (* Keep other logger at info level *)
  Logs.Src.set_level other_logger (Some (to_logs_level Info));
  
  clear_log_buffer ();
  
  (* Test info level logging *)
  Logs.info ~src:oxfastmcp_main (fun m -> m "oxfastmcp main");
  Logs.info ~src:oxfastmcp_server (fun m -> m "oxfastmcp server");
  Logs.info ~src:oxfastmcp_client (fun m -> m "oxfastmcp client");
  Logs.info ~src:other_logger (fun m -> m "other app");
  
  (* Only the non-OxFastMCP logger should have output *)
  check bool "oxfastmcp main suppressed" false (log_contains "oxfastmcp main");
  check bool "oxfastmcp server suppressed" false (log_contains "oxfastmcp server");
  check bool "oxfastmcp client suppressed" false (log_contains "oxfastmcp client");
  check bool "other app logged" true (log_contains "other app")

(** Test level conversion functions *)
let test_level_conversion () =
  (* Test level_of_string *)
  check bool "DEBUG level" true (level_of_string "DEBUG" = Debug);
  check bool "INFO level" true (level_of_string "INFO" = Info);
  check bool "WARNING level" true (level_of_string "WARNING" = Warning);
  check bool "ERROR level" true (level_of_string "ERROR" = Error);
  check bool "CRITICAL level" true (level_of_string "CRITICAL" = Critical);
  check bool "unknown level defaults to Info" true (level_of_string "UNKNOWN" = Info);
  
  (* Test to_logs_level *)
  check bool "Debug to Logs.Debug" true (to_logs_level Debug = Logs.Debug);
  check bool "Info to Logs.Info" true (to_logs_level Info = Logs.Info);
  check bool "Warning to Logs.Warning" true (to_logs_level Warning = Logs.Warning);
  check bool "Error to Logs.Error" true (to_logs_level Error = Logs.Error);
  check bool "Critical to Logs.Error" true (to_logs_level Critical = Logs.Error)

(** Test logger creation *)
let test_get_logger () =
  setup_test_logging ();
  let module Logger = (val get_logger "test") in
  clear_log_buffer ();
  
  (* Set log level to Debug to capture all messages *)
  Logs.set_level (Some (to_logs_level Debug));
  
  Logger.info (fun m -> m "test message");
  check bool "logger source name" true
    (log_contains "[OxFastMCP.test]")

(** Test logging configuration *)
let test_configure_logging () =
  (* Test with default settings *)
  configure_logging ();
  check bool "default level is Info" true
    (Logs.level () = Some (to_logs_level Info));
  
  (* Test with Debug level *)
  configure_logging ~level:"DEBUG" ();
  check bool "debug level set" true
    (Logs.level () = Some (to_logs_level Debug));
  
  (* Test with Warning level *)
  configure_logging ~level:"WARNING" ();
  check bool "warning level set" true
    (Logs.level () = Some (to_logs_level Warning));
  
  (* Test with Error level *)
  configure_logging ~level:"ERROR" ();
  check bool "error level set" true
    (Logs.level () = Some (to_logs_level Error));
  
  (* Test with Critical level (maps to Error) *)
  configure_logging ~level:"CRITICAL" ();
  check bool "critical level maps to error" true
    (Logs.level () = Some (to_logs_level Critical))

(** Test actual logging *)
let test_logging () =
  configure_logging ~level:"DEBUG" ();
  let module Logger = (val get_logger "test") in
  
  (* We can't easily test the output, but we can at least ensure these don't raise exceptions *)
  Logger.debug (fun m -> m "Debug message");
  Logger.info (fun m -> m "Info message");
  Logger.warn (fun m -> m "Warning message");
  Logger.err (fun m -> m "Error message");
  
  (* Test error handling *)
  let result = Result.error (`Msg "test error") in
  let handled = Logger.on_error_msg ~use:(fun () -> "handled") result in
  check string "error handled" "handled" handled

let () =
  run "Logging Tests" [
    ("Logging", [
      test_case "Level conversion" `Quick test_level_conversion;
      test_case "Logger creation" `Quick test_get_logger;
      test_case "Configure logging" `Quick test_configure_logging;
      test_case "Actual logging" `Quick test_logging;
      test_case "Log level hierarchy" `Quick test_log_level_hierarchy;
      test_case "OxFastMCP namespace isolation" `Quick test_oxfastmcp_namespace_isolation;
      test_case "Logging doesn't affect other loggers" `Quick test_logging_doesnt_affect_other_loggers;
      test_case "Alternative logger isolation test" `Quick test_logging_doesnt_affect_other_loggers_alt;
      test_case "Logger functionality" `Quick test_get_logger_functionality;
    ]);
  ] 