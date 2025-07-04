open! Core
open! Async
open! Expect_test_helpers_core
open Ox_fast_mcp.Utilities.Cache

let%expect_test "test_init" =
  let expiration = Time_float_unix.Span.of_sec 10.0 in
  let cache = TimedCache.create ~expiration in
  print_s [%sexp (TimedCache.get_expiration cache : Time_float_unix.Span.t)];
  print_s [%sexp (TimedCache.size cache : int)];
  [%expect {|
    10s
    0 |}]

let%expect_test "test_set_and_get" =
  let cache = TimedCache.create ~expiration:(Time_float_unix.Span.of_sec 10.0) in
  TimedCache.set cache ~key:"test_key" ~value:"test_value";
  print_s [%sexp (TimedCache.size cache : int)];
  print_s [%sexp (TimedCache.get cache ~key:"test_key" : string option)];
  [%expect {|
    1
    (test_value) |}]

let%expect_test "test_get_not_found" =
  let cache = TimedCache.create ~expiration:(Time_float_unix.Span.of_sec 10.0) in
  print_s [%sexp (TimedCache.get cache ~key:"nonexistent" : string option)];
  [%expect {| () |}]

let%expect_test "test_clear" =
  let cache = TimedCache.create ~expiration:(Time_float_unix.Span.of_sec 10.0) in
  TimedCache.set cache ~key:"key1" ~value:"value1";
  TimedCache.set cache ~key:"key2" ~value:"value2";
  print_s [%sexp (TimedCache.size cache : int)];
  TimedCache.clear cache;
  print_s [%sexp (TimedCache.size cache : int)];
  [%expect {|
    2
    0 |}]

let%expect_test "test_stats" =
  let cache = TimedCache.create ~expiration:(Time_float_unix.Span.of_sec 10.0) in
  print_s [%sexp (TimedCache.get_stats cache : cache_stats)];
  TimedCache.set cache ~key:"test" ~value:"value";
  print_s [%sexp (TimedCache.get_stats cache : cache_stats)];
  ignore (TimedCache.get cache ~key:"test");
  print_s [%sexp (TimedCache.get_stats cache : cache_stats)];
  ignore (TimedCache.get cache ~key:"nonexistent");
  print_s [%sexp (TimedCache.get_stats cache : cache_stats)];
  [%expect {|
    ((hits 0) (misses 0) (sets 0))
    ((hits 0) (misses 0) (sets 1))
    ((hits 1) (misses 0) (sets 1))
    ((hits 1) (misses 1) (sets 1)) |}]

let%expect_test "test_expiration" =
  let cache = TimedCache.create ~expiration:(Time_float_unix.Span.of_sec 1.0) in
  TimedCache.set cache ~key:"test" ~value:"value";
  print_s [%sexp (TimedCache.get cache ~key:"test" : string option)];
  (* Wait for expiration *)
  ignore (Unix.sleep 2);
  print_s [%sexp (TimedCache.get cache ~key:"test" : string option)];
  [%expect {|
    (value)
    () |}]

let%expect_test "test_overwrite_value" =
  let cache = TimedCache.create ~expiration:(Time_float_unix.Span.of_sec 10.0) in
  TimedCache.set cache ~key:"test" ~value:"initial";
  print_s [%sexp (TimedCache.get cache ~key:"test" : string option)];
  TimedCache.set cache ~key:"test" ~value:"updated";
  print_s [%sexp (TimedCache.get cache ~key:"test" : string option)];
  [%expect {|
    (initial)
    (updated) |}]

let%expect_test "test_many_items" =
  let cache = TimedCache.create ~expiration:(Time_float_unix.Span.of_sec 10.0) in
  List.iter (List.range 0 1000) ~f:(fun i ->
    TimedCache.set cache ~key:(Int.to_string i) ~value:(sprintf "value%d" i));
  print_s [%sexp (TimedCache.size cache : int)];
  List.iter [0; 123; 456; 789; 999] ~f:(fun i ->
    print_s [%sexp (TimedCache.get cache ~key:(Int.to_string i) : string option)]);
  [%expect {|
    1000
    (value0)
    (value123)
    (value456)
    (value789)
    (value999) |}] 