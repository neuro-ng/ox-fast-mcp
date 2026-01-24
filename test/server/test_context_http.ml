(** Tests for Context HTTP request access *)

open! Core
open! Async
open! Expect_test_helpers_core

(* Access Context using internal module name *)
module Context = Server__Context

let%expect_test "get_http_request returns None when no HTTP context" =
  let%bind () =
    let ctx = Context.create () in
    let http_req = Context.get_http_request ctx in
    print_s [%sexp (Option.is_none http_req : bool)];
    [%expect {| true |}];
    return ()
  in
  return ()

let%expect_test "get_http_request is callable" =
  let%bind () =
    let ctx = Context.create () in
    let _http_req = Context.get_http_request ctx in
    print_endline "get_http_request executed without error";
    [%expect {| get_http_request executed without error |}];
    return ()
  in
  return ()
