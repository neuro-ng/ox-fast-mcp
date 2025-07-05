open! Core
open! Async

let%expect_test "test_init" =
  let%bind () = Clock.after (Time_float_unix.Span.of_sec 0.0) in
  let expiration = Time_float_unix.Span.of_sec 10.0 in
  let cache = Cache.TimedCache.create ~expiration in
  print_s [%sexp (Cache.TimedCache.get_expiration cache : Time_float_unix.Span.t)];
  print_s [%sexp (Cache.TimedCache.size cache : int)];
  [%expect {|
    10s
    0 |}];
  return ()

let%expect_test "test_set_and_get" =
  let%bind () = Clock.after (Time_float_unix.Span.of_sec 0.0) in
  let cache = Cache.TimedCache.create ~expiration:(Time_float_unix.Span.of_sec 10.0) in
  Cache.TimedCache.set cache ~key:"test_key" ~value:"test_value";
  print_s [%sexp (Cache.TimedCache.size cache : int)];
  print_s [%sexp (Cache.TimedCache.get cache ~key:"test_key" : string option)];
  [%expect {|
    1
    (test_value) |}];
  return ()

let%expect_test "test_get_not_found" =
  let%bind () = Clock.after (Time_float_unix.Span.of_sec 0.0) in
  let cache = Cache.TimedCache.create ~expiration:(Time_float_unix.Span.of_sec 10.0) in
  print_s [%sexp (Cache.TimedCache.get cache ~key:"nonexistent" : string option)];
  [%expect {| () |}];
  return ()

let%expect_test "test_clear" =
  let%bind () = Clock.after (Time_float_unix.Span.of_sec 0.0) in
  let cache = Cache.TimedCache.create ~expiration:(Time_float_unix.Span.of_sec 10.0) in
  Cache.TimedCache.set cache ~key:"key1" ~value:"value1";
  Cache.TimedCache.set cache ~key:"key2" ~value:"value2";
  print_s [%sexp (Cache.TimedCache.size cache : int)];
  Cache.TimedCache.clear cache;
  print_s [%sexp (Cache.TimedCache.size cache : int)];
  [%expect {|
    2
    0 |}];
  return ()

let%expect_test "test_stats" =
  let%bind () = Clock.after (Time_float_unix.Span.of_sec 0.0) in
  let cache = Cache.TimedCache.create ~expiration:(Time_float_unix.Span.of_sec 10.0) in
  print_s [%sexp (Cache.TimedCache.get_stats cache : Cache.cache_stats)];
  Cache.TimedCache.set cache ~key:"test" ~value:"value";
  print_s [%sexp (Cache.TimedCache.get_stats cache : Cache.cache_stats)];
  ignore (Cache.TimedCache.get cache ~key:"test");
  print_s [%sexp (Cache.TimedCache.get_stats cache : Cache.cache_stats)];
  ignore (Cache.TimedCache.get cache ~key:"nonexistent");
  print_s [%sexp (Cache.TimedCache.get_stats cache : Cache.cache_stats)];
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
  let cache = Cache.TimedCache.create ~expiration:(Time_float_unix.Span.of_sec 1.0) in
  Cache.TimedCache.set cache ~key:"test" ~value:"value";
  print_s [%sexp (Cache.TimedCache.get cache ~key:"test" : string option)];
  let%bind () = Clock.after (Time_float_unix.Span.of_sec 2.0) in
  print_s [%sexp (Cache.TimedCache.get cache ~key:"test" : string option)];
  [%expect {|
    (value)
    () |}];
  return ()

let%expect_test "test_overwrite_value" =
  let%bind () = Clock.after (Time_float_unix.Span.of_sec 0.0) in
  let cache = Cache.TimedCache.create ~expiration:(Time_float_unix.Span.of_sec 10.0) in
  Cache.TimedCache.set cache ~key:"test" ~value:"initial";
  print_s [%sexp (Cache.TimedCache.get cache ~key:"test" : string option)];
  Cache.TimedCache.set cache ~key:"test" ~value:"updated";
  print_s [%sexp (Cache.TimedCache.get cache ~key:"test" : string option)];
  [%expect {|
    (initial)
    (updated) |}];
  return ()

let%expect_test "test_many_items" =
  let%bind () = Clock.after (Time_float_unix.Span.of_sec 0.0) in
  let cache = Cache.TimedCache.create ~expiration:(Time_float_unix.Span.of_sec 10.0) in
  List.iter (List.range 0 1000) ~f:(fun i ->
    Cache.TimedCache.set cache ~key:(Int.to_string i) ~value:(sprintf "value%d" i));
  print_s [%sexp (Cache.TimedCache.size cache : int)];
  List.iter [0; 123; 456; 789; 999] ~f:(fun i ->
    print_s [%sexp (Cache.TimedCache.get cache ~key:(Int.to_string i) : string option)]);
  [%expect {|
    1000
    (value0)
    (value123)
    (value456)
    (value789)
    (value999) |}];
  return ()

let%expect_test "test_extends_expiration_on_overwrite" =
  let%bind () = Clock.after (Time_float_unix.Span.of_sec 0.0) in
  let cache = Cache.TimedCache.create ~expiration:(Time_float_unix.Span.of_sec 10.0) in
  Cache.TimedCache.set cache ~key:"test" ~value:"initial";
  let%bind () = Clock.after (Time_float_unix.Span.of_sec 5.0) in
  Cache.TimedCache.set cache ~key:"test" ~value:"updated";
  let%bind () = Clock.after (Time_float_unix.Span.of_sec 3.0) in
  print_s [%sexp (Cache.TimedCache.get cache ~key:"test" : string option)];
  [%expect {| (updated) |}];
  let%bind () = Clock.after (Time_float_unix.Span.of_sec 8.0) in
  print_s [%sexp (Cache.TimedCache.get cache ~key:"test" : string option)];
  [%expect {| () |}];
  return ()

let%expect_test "test_different_key_types" =
  let%bind () = Clock.after (Time_float_unix.Span.of_sec 0.0) in
  let cache = Cache.TimedCache.create ~expiration:(Time_float_unix.Span.of_sec 10.0) in
  
  (* Test integer key *)
  Cache.TimedCache.set cache ~key:(Int.to_string 42) ~value:"int_value";
  print_s [%sexp (Cache.TimedCache.get cache ~key:(Int.to_string 42) : string option)];
  
  (* Test float key *)
  Cache.TimedCache.set cache ~key:(Float.to_string 3.14) ~value:"float_value";
  print_s [%sexp (Cache.TimedCache.get cache ~key:(Float.to_string 3.14) : string option)];
  
  (* Test tuple key *)
  let tuple_key = sprintf "(%d,%d)" 1 2 in
  Cache.TimedCache.set cache ~key:tuple_key ~value:"tuple_value";
  print_s [%sexp (Cache.TimedCache.get cache ~key:tuple_key : string option)];
  
  [%expect {|
    (int_value)
    (float_value)
    (tuple_value) |}];
  return ()

let%expect_test "test_none_value" =
  let%bind () = Clock.after (Time_float_unix.Span.of_sec 0.0) in
  let cache = Cache.TimedCache.create ~expiration:(Time_float_unix.Span.of_sec 10.0) in
  Cache.TimedCache.set cache ~key:"none_key" ~value:"";  (* Empty string represents None *)
  print_s [%sexp (Cache.TimedCache.get cache ~key:"none_key" : string option)];
  [%expect {| ("") |}];
  return ()

let%expect_test "test_edge_case_zero_expiration" =
  let%bind () = Clock.after (Time_float_unix.Span.of_sec 0.0) in
  let cache = Cache.TimedCache.create ~expiration:(Time_float_unix.Span.of_sec 0.0) in
  Cache.TimedCache.set cache ~key:"test" ~value:"value";
  let%bind () = Clock.after (Time_float_unix.Span.of_sec 0.001) in
  print_s [%sexp (Cache.TimedCache.get cache ~key:"test" : string option)];
  [%expect {| () |}];  (* Should be expired *)
  return ()

let%expect_test "test_negative_expiration" =
  let%bind () = Clock.after (Time_float_unix.Span.of_sec 0.0) in
  let cache = Cache.TimedCache.create ~expiration:(Time_float_unix.Span.of_sec (-1.0)) in
  Cache.TimedCache.set cache ~key:"test" ~value:"value";
  print_s [%sexp (Cache.TimedCache.get cache ~key:"test" : string option)];
  [%expect {| () |}];  (* Should be immediately expired *)
  return ()

let%expect_test "test_cache_consistency" =
  let%bind () = Clock.after (Time_float_unix.Span.of_sec 0.0) in
  let cache = Cache.TimedCache.create ~expiration:(Time_float_unix.Span.of_sec 10.0) in
  
  (* Add multiple items *)
  Cache.TimedCache.set cache ~key:"key1" ~value:"value1";
  Cache.TimedCache.set cache ~key:"key2" ~value:"value2";
  Cache.TimedCache.set cache ~key:"key3" ~value:"value3";
  
  (* Check all items *)
  print_s [%sexp (Cache.TimedCache.get cache ~key:"key1" : string option)];
  print_s [%sexp (Cache.TimedCache.get cache ~key:"key2" : string option)];
  print_s [%sexp (Cache.TimedCache.get cache ~key:"key3" : string option)];
  
  (* Overwrite one item *)
  Cache.TimedCache.set cache ~key:"key2" ~value:"updated_value";
  
  (* Check again *)
  print_s [%sexp (Cache.TimedCache.get cache ~key:"key1" : string option)];
  print_s [%sexp (Cache.TimedCache.get cache ~key:"key2" : string option)];
  print_s [%sexp (Cache.TimedCache.get cache ~key:"key3" : string option)];
  
  [%expect {|
    (value1)
    (value2)
    (value3)
    (value1)
    (updated_value)
    (value3) |}];
  return () 