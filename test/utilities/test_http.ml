open! Core
open! Async
open Expect_test_helpers_core

let%expect_test "find_available_port returns a valid port number" =
  let port = Http.find_available_port () in
  let is_valid_port = port > 0 && port < 65536 in
  print_s [%sexp (is_valid_port : bool)];
  [%expect {| true |}];  (* Just check if it's a valid port *)
  (* Try to bind to the port to ensure it was actually available *)
  let sock = Core_unix.socket ~domain:Core_unix.PF_INET ~kind:Core_unix.SOCK_STREAM ~protocol:0 () in
  let addr = Core_unix.ADDR_INET (Core_unix.Inet_addr.localhost, port) in
  Core_unix.bind sock ~addr;
  Core_unix.close sock;
  [%expect {| |}];  (* No exception means we could bind successfully *)
  Deferred.unit

let%expect_test "find_available_port returns different ports on consecutive calls" =
  let port1 = Http.find_available_port () in
  let port2 = Http.find_available_port () in
  let ports_are_different = port1 <> port2 in
  print_s [%sexp (ports_are_different : bool)];
  [%expect {| true |}];  (* Just check if they're different *)
  Deferred.unit 