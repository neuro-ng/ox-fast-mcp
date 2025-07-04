open Core
open Expect_test_helpers_core

let%expect_test "find_available_port returns a valid port number" =
  let port = Http.find_available_port () in
  print_s [%sexp (port : int)];
  [%expect {| |}];  (* Port number will vary, so we just check it prints *)
  [%test_result: bool]
    (port > 0 && port < 65536)
    ~expect:true
    ~message:"Port should be in valid range (1-65535)";
  (* Try to bind to the port to ensure it was actually available *)
  let sock = Core_unix.socket ~domain:Core_unix.PF_INET ~kind:Core_unix.SOCK_STREAM ~protocol:0 () in
  let addr = Core_unix.ADDR_INET (Core_unix.Inet_addr.localhost, port) in
  Core_unix.bind sock ~addr;
  Core_unix.close sock;
  [%expect {| |}]  (* No exception means we could bind successfully *)

let%expect_test "find_available_port returns different ports on consecutive calls" =
  let port1 = Http.find_available_port () in
  let port2 = Http.find_available_port () in
  print_s [%sexp (port1 : int), (port2 : int)];
  [%expect {| |}];  (* Port numbers will vary *)
  [%test_result: bool]
    (port1 <> port2)
    ~expect:true
    ~message:"Consecutive calls should return different ports" 