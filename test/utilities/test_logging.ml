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
