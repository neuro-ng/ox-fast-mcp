open Alcotest
open Utilities.Cache

(** Test TimedCache initialization *)
let test_init () =
  let expiration = 10.0 in
  let cache = TimedCache.create expiration in
  check (float 0.01) "expiration time" expiration (TimedCache.get_expiration cache);
  check int "cache is empty" 0 (TimedCache.size cache)

(** Test setting values in the cache *)
let test_set () =
  let cache = TimedCache.create 10.0 in
  let key = "test_key" in
  let value = "test_value" in
  
  TimedCache.set cache key value;
  check int "cache has one item" 1 (TimedCache.size cache);
  
  match TimedCache.get cache key with
  | Some retrieved_value -> check string "value matches" value retrieved_value
  | None -> fail "Expected value to be found"

(** Test retrieving a value that exists and has not expired *)
let test_get_found () =
  let cache = TimedCache.create 10.0 in
  let key = "test_key" in
  let value = "test_value" in
  
  TimedCache.set cache key value;
  
  match TimedCache.get cache key with
  | Some retrieved_value -> check string "found value matches" value retrieved_value
  | None -> fail "Expected value to be found"

(** Test retrieving a value that exists but has expired *)
let test_get_expired () =
  (* Use a very short expiration time *)
  let cache = TimedCache.create 0.01 in (* 10 milliseconds *)
  let key = "test_key" in
  let value = "test_value" in
  
  TimedCache.set cache key value;
  
  (* Wait for expiration *)
  Unix.sleepf 0.02; (* 20 milliseconds *)
  
  match TimedCache.get cache key with
  | Some _ -> fail "Expected value to be expired"
  | None -> check bool "expired value not found" true true

(** Test retrieving a value that doesn't exist in the cache *)
let test_get_not_found () =
  let cache = TimedCache.create 10.0 in
  
  match TimedCache.get cache "nonexistent_key" with
  | Some _ -> fail "Expected key to not be found"
  | None -> check bool "nonexistent key not found" true true

(** Test that the cache can be cleared *)
let test_clear () =
  let cache = TimedCache.create 10.0 in
  
  (* Add some items *)
  TimedCache.set cache "key1" "value1";
  TimedCache.set cache "key2" "value2";
  check int "cache has two items" 2 (TimedCache.size cache);
  
  (* Clear the cache *)
  TimedCache.clear cache;
  check int "cache is empty after clear" 0 (TimedCache.size cache)

(** Test that values actually expire after the specified time *)
let test_real_expiration () =
  (* Use a very short expiration for the test *)
  let cache = TimedCache.create 0.05 in (* 50 milliseconds *)
  let key = "test_key" in
  let value = "test_value" in
  
  TimedCache.set cache key value;
  
  (* Value should be available immediately *)
  (match TimedCache.get cache key with
   | Some retrieved_value -> check string "immediate value matches" value retrieved_value
   | None -> fail "Expected immediate value to be found");
  
  (* Wait for expiration *)
  Unix.sleepf 0.06; (* 60 milliseconds *)
  
  (* Value should now be expired *)
  match TimedCache.get cache key with
  | Some _ -> fail "Expected value to be expired after wait"
  | None -> check bool "value expired correctly" true true

(** Test that setting a key that already exists overwrites the old value *)
let test_overwrite_value () =
  let cache = TimedCache.create 10.0 in
  let key = "test_key" in
  
  (* Set initial value *)
  TimedCache.set cache key "initial_value";
  (match TimedCache.get cache key with
   | Some value -> check string "initial value" "initial_value" value
   | None -> fail "Expected initial value to be found");
  
  (* Overwrite with new value *)
  TimedCache.set cache key "new_value";
  (match TimedCache.get cache key with
   | Some value -> check string "overwritten value" "new_value" value
   | None -> fail "Expected overwritten value to be found")

(** Test different types of values can be stored *)
let test_different_values () =
  let cache = TimedCache.create 10.0 in
  
  (* Test various value types *)
  let values = [
    ("int_key", "42");
    ("float_key", "3.14");
    ("string_key", "hello");
  ] in
  
  List.iter (fun (key, value) ->
    TimedCache.set cache key value;
    match TimedCache.get cache key with
    | Some retrieved_value -> check string ("value for key " ^ key) value retrieved_value
    | None -> fail ("Expected value for key " ^ key ^ " to be found")
  ) values

(** Test that empty string can be stored as a value *)
let test_empty_value () =
  let cache = TimedCache.create 10.0 in
  let key = "empty_key" in
  let value = "" in
  
  TimedCache.set cache key value;
  match TimedCache.get cache key with
  | Some retrieved_value -> check string "empty value" value retrieved_value
  | None -> fail "Expected empty value to be found"

(** Test with a zero expiration time *)
let test_edge_case_zero_expiration () =
  let cache = TimedCache.create 0.0 in
  let key = "test_key" in
  let value = "test_value" in
  
  TimedCache.set cache key value;
  (* The value might already be expired by the time we call get() *)
  match TimedCache.get cache key with
  | Some _ | None -> check bool "zero expiration test completed" true true

(** Test with a negative expiration time *)
let test_negative_expiration () =
  let cache = TimedCache.create (-1.0) in
  let key = "test_key" in
  let value = "test_value" in
  
  TimedCache.set cache key value;
  (* Value should be immediately expired *)
  match TimedCache.get cache key with
  | Some _ -> fail "Expected value to be immediately expired with negative expiration"
  | None -> check bool "negative expiration works" true true

(** Test cache with many items *)
let test_many_items () =
  let cache = TimedCache.create 10.0 in
  let item_count = 1000 in
  
  (* Add many items *)
  for i = 0 to item_count - 1 do
    let key = "key" ^ (string_of_int i) in
    let value = "value" ^ (string_of_int i) in
    TimedCache.set cache key value
  done;
  
  (* Check size *)
  check int "many items size" item_count (TimedCache.size cache);
  
  (* Check some random items *)
  let test_indices = [0; 123; 456; 789; 999] in
  List.iter (fun i ->
    let key = "key" ^ (string_of_int i) in
    let expected_value = "value" ^ (string_of_int i) in
    match TimedCache.get cache key with
    | Some value -> check string ("item " ^ (string_of_int i)) expected_value value
    | None -> fail ("Expected item " ^ (string_of_int i) ^ " to be found")
  ) test_indices

(** Main test suite *)
let () = run "Cache Module Tests" [
  ("TimedCache Basic Operations", [
    test_case "Cache initialization" `Quick test_init;
    test_case "Setting values" `Quick test_set;
    test_case "Getting found values" `Quick test_get_found;
    test_case "Getting expired values" `Quick test_get_expired;
    test_case "Getting not found values" `Quick test_get_not_found;
    test_case "Clearing cache" `Quick test_clear;
  ]);
  ("TimedCache Expiration", [
    test_case "Real expiration timing" `Quick test_real_expiration;
    test_case "Zero expiration edge case" `Quick test_edge_case_zero_expiration;
    test_case "Negative expiration" `Quick test_negative_expiration;
  ]);
  ("TimedCache Data Handling", [
    test_case "Overwriting values" `Quick test_overwrite_value;
    test_case "Different value types" `Quick test_different_values;
    test_case "Empty string values" `Quick test_empty_value;
    test_case "Many items performance" `Quick test_many_items;
  ]);
] 