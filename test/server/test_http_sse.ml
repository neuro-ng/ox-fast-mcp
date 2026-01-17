(** Tests for HTTP/SSE Transport **)

open! Core
open! Async

module Http = Server__Http
(** Access Http module using the Server__ prefix pattern used in other tests **)

(** {1 Test: SSE Transport}
    **)

let%expect_test "create SSE transport" =
  let _sse = Http.Sse_transport.create ~message_path:"/messages" in
  (* SSE transport created successfully *)
  print_endline "SSE transport created";
  [%expect {| SSE transport created |}];
  return ()

let%expect_test "SSE connection establishment" =
  let sse = Http.Sse_transport.create ~message_path:"/messages" in
  let request =
    Http.Request.create ~method_:Http.Http_method.GET ~path:"/sse" ()
  in

  let%bind conn_id, cleanup = Http.Sse_transport.connect_sse sse request in

  print_s [%sexp (conn_id >= 0 : bool)];

  (* Cleanup connection *)
  let%bind () = cleanup () in

  [%expect
    {|
    INFO SSE connection established: 0
    true
    INFO SSE connection closed: 0
    |}];
  return ()

let%expect_test "SSE broadcast message" =
  let sse = Http.Sse_transport.create ~message_path:"/messages" in
  let message =
    `Assoc [ ("type", `String "test"); ("data", `String "hello") ]
  in

  (* Broadcast to connections (currently none) *)
  Http.Sse_transport.broadcast_message sse message;

  print_endline "Message broadcasted";
  [%expect {| Message broadcasted |}];
  return ()

let%expect_test "handle POST message with valid JSON" =
  let sse = Http.Sse_transport.create ~message_path:"/messages" in
  let body = {|{"method":"tools/list","id":1}|} in
  let request =
    Http.Request.create ~method_:Http.Http_method.POST ~path:"/messages" ~body
      ()
  in

  let%bind response = Http.Sse_transport.handle_post_message sse request in

  print_s [%sexp (response.status : int)];
  [%expect {| 200 |}];
  return ()

let%expect_test "handle POST message with invalid JSON" =
  let sse = Http.Sse_transport.create ~message_path:"/messages" in
  let body = "not valid json" in
  let request =
    Http.Request.create ~method_:Http.Http_method.POST ~path:"/messages" ~body
      ()
  in

  let%bind response = Http.Sse_transport.handle_post_message sse request in

  print_s [%sexp (response.status : int)];
  [%expect
    {|
    ERROR Invalid JSON: ("Yojson__Common.Json_error(\"Line 1, bytes 0-14:\\nInvalid token 'not valid json'\")")
    400
    |}];
  return ()

let%expect_test "handle POST message without body" =
  let sse = Http.Sse_transport.create ~message_path:"/messages" in
  let request =
    Http.Request.create ~method_:Http.Http_method.POST ~path:"/messages" ()
  in

  let%bind response = Http.Sse_transport.handle_post_message sse request in

  print_s [%sexp (response.status : int)];
  (match response.body with
  | Some body -> print_endline body
  | None -> print_endline "No body");

  [%expect {|
    400
    Missing message body
    |}];
  return ()

(** {1 Test: HTTP Response Helpers}
    **)

let%expect_test "Response.json creates proper JSON response" =
  let json = `Assoc [ ("status", `String "ok"); ("count", `Int 42) ] in
  let response = Http.Response.json json in

  print_s [%sexp (response.status : int)];
  (match response.body with
  | Some body -> print_endline body
  | None -> print_endline "No body");

  [%expect {|
    200
    {"status":"ok","count":42}
    |}];
  return ()

let%expect_test "Response status codes" =
  let r1 = Http.Response.ok () in
  let r2 = Http.Response.created () in
  let r3 = Http.Response.bad_request () in
  let r4 = Http.Response.not_found () in
  let r5 = Http.Response.internal_server_error () in

  print_s [%sexp (r1.status : int)];
  print_s [%sexp (r2.status : int)];
  print_s [%sexp (r3.status : int)];
  print_s [%sexp (r4.status : int)];
  print_s [%sexp (r5.status : int)];

  [%expect {|
    200
    201
    400
    404
    500
    |}];
  return ()
