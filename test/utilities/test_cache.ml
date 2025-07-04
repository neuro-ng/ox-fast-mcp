open! Core
open! Async
open! Expect_test_helpers_core
open Cache

let%expect_test "test_init" =
  let%bind () = Clock.after (Time_float_unix.Span.of_sec 0.0) in
  let expiration = Time_float_unix.Span.of_sec 10.0 in
  let cache = TimedCache.create ~expiration in
  print_s [%sexp (TimedCache.get_expiration cache : Time_float_unix.Span.t)];
  print_s [%sexp (TimedCache.size cache : int)];
  [%expect {|
    10s
    0 |}];
  return ()

let%expect_test "test_set_and_get" =
  let%bind () = Clock.after (Time_float_unix.Span.of_sec 0.0) in
  let cache = TimedCache.create ~expiration:(Time_float_unix.Span.of_sec 10.0) in
  TimedCache.set cache ~key:"test_key" ~value:"test_value";
  print_s [%sexp (TimedCache.size cache : int)];
  print_s [%sexp (TimedCache.get cache ~key:"test_key" : string option)];
  [%expect {|
    1
    (test_value) |}];
  return ()

let%expect_test "test_get_not_found" =
  let%bind () = Clock.after (Time_float_unix.Span.of_sec 0.0) in
  let cache = TimedCache.create ~expiration:(Time_float_unix.Span.of_sec 10.0) in
  print_s [%sexp (TimedCache.get cache ~key:"nonexistent" : string option)];
  [%expect {| () |}];
  return ()

let%expect_test "test_clear" =
  let%bind () = Clock.after (Time_float_unix.Span.of_sec 0.0) in
  let cache = TimedCache.create ~expiration:(Time_float_unix.Span.of_sec 10.0) in
  TimedCache.set cache ~key:"key1" ~value:"value1";
  TimedCache.set cache ~key:"key2" ~value:"value2";
  print_s [%sexp (TimedCache.size cache : int)];
  TimedCache.clear cache;
  print_s [%sexp (TimedCache.size cache : int)];
  [%expect {|
    2
    0 |}];
  return ()

let%expect_test "test_stats" =
  let%bind () = Clock.after (Time_float_unix.Span.of_sec 0.0) in
  let cache = TimedCache.create ~expiration:(Time_float_unix.Span.of_sec 10.0) in
  print_s [%sexp (TimedCache.get_stats cache : cache_stats)];
  TimedCache.set cache ~key:"test" ~value:"value";
  print_s [%sexp (TimedCache.get_stats cache : cache_stats)];
  ignore (TimedCache.get cache ~key:"test");
  print_s [%sexp (TimedCache.get_stats cache : cache_stats)];
  ignore (TimedCache.get cache ~key:"nonexistent");
  print_s [%sexp (TimedCache.get_stats cache : cache_stats)];
  [%expect {|
    ((hits   0)
     (misses 0)
     (sets   0))
    ((hits   0)
     (misses 0)
     (sets   1))
    ((hits   1)
     (misses 0)
     (sets   1))
    ((hits   1)
     (misses 1)
     (sets   1))
    |}];
  return ()

let%expect_test "test_expiration" =
  let%bind () = Clock.after (Time_float_unix.Span.of_sec 0.0) in
  let cache = TimedCache.create ~expiration:(Time_float_unix.Span.of_sec 1.0) in
  TimedCache.set cache ~key:"test" ~value:"value";
  print_s [%sexp (TimedCache.get cache ~key:"test" : string option)];
  let%bind () = Clock.after (Time_float_unix.Span.of_sec 2.0) in
  print_s [%sexp (TimedCache.get cache ~key:"test" : string option)];
  [%expect {|
    (value)
    () |}];
  return ()

let%expect_test "test_overwrite_value" =
  let%bind () = Clock.after (Time_float_unix.Span.of_sec 0.0) in
  let cache = TimedCache.create ~expiration:(Time_float_unix.Span.of_sec 10.0) in
  TimedCache.set cache ~key:"test" ~value:"initial";
  print_s [%sexp (TimedCache.get cache ~key:"test" : string option)];
  TimedCache.set cache ~key:"test" ~value:"updated";
  print_s [%sexp (TimedCache.get cache ~key:"test" : string option)];
  [%expect {|
    (initial)
    (updated) |}];
  return ()

let%expect_test "test_many_items" =
  let%bind () = Clock.after (Time_float_unix.Span.of_sec 0.0) in
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
    (value999) |}];
  return () 