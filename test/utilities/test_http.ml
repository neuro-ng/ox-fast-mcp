open Alcotest
open Unix
open Utilities.Http

(** Test port finding functionality *)
let test_find_available_port () =
  (* Get a port *)
  let port = find_available_port () in
  
  (* Verify port is in valid range *)
  check bool "port is positive" true (port > 0);
  check bool "port is in valid range" true (port < 65536);
  
  (* Verify we can bind to this port *)
  let sock = socket PF_INET SOCK_STREAM 0 in
  let can_bind = try
    let localhost = inet_addr_of_string "127.0.0.1" in
    bind sock (ADDR_INET (localhost, port));
    true
  with Unix_error _ ->
    false
  in
  close sock;
  check bool "can bind to port" true can_bind

(** Test multiple port allocations *)
let test_multiple_ports () =
  let port1 = find_available_port () in
  let port2 = find_available_port () in
  
  (* Verify ports are different *)
  check bool "different ports" true (port1 <> port2);
  
  (* Verify both ports are valid *)
  check bool "port1 is valid" true (port1 > 0 && port1 < 65536);
  check bool "port2 is valid" true (port2 > 0 && port2 < 65536)

(** Test error handling *)
let test_error_handling () =
  (* Create and bind a socket *)
  let sock = socket PF_INET SOCK_STREAM 0 in
  let port = find_available_port () in
  let localhost = inet_addr_of_string "127.0.0.1" in
  bind sock (ADDR_INET (localhost, port));
  
  (* Try to bind another socket to the same port *)
  let sock2 = socket PF_INET SOCK_STREAM 0 in
  let raises = try
    bind sock2 (ADDR_INET (localhost, port));
    false
  with Unix_error(EADDRINUSE, _, _) ->
    true
  | _ ->
    false
  in
  close sock;
  close sock2;
  check bool "raises EADDRINUSE" true raises

(** Main test suite *)
let () =
  run "HTTP Module Tests" [
    ("Port Finding", [
      test_case "Find available port" `Quick test_find_available_port;
      test_case "Multiple port allocations" `Quick test_multiple_ports;
      test_case "Error handling" `Quick test_error_handling;
    ]);
  ] 