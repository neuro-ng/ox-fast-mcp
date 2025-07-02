open Base
open Stdio

let test_basic () =
  printf "Running basic test...\n";
  let result = 1 + 1 in
  if result = 2 then
    printf "✓ Basic arithmetic test passed\n"
  else
    printf "✗ Basic arithmetic test failed\n"

let test_list_operations () =
  printf "Running list operations test...\n";
  let numbers = [1; 2; 3] in
  let doubled = List.map numbers ~f:(fun x -> x * 2) in
  let expected = [2; 4; 6] in
  if List.equal Int.equal doubled expected then
    printf "✓ List operations test passed\n"
  else
    printf "✗ List operations test failed\n"

let () =
  printf "Running client tests...\n";
  test_basic ();
  test_list_operations ();
  printf "Tests completed.\n"