(** Tests for HTTP Middleware in applications.

    Translated from Python test_http_middleware.py to OCaml. Tests middleware
    composition, chaining, and header modification.

    Note: Python tests require ASGI transport and httpx client not available in
    OCaml. These tests focus on unit testing middleware directly. *)

open! Core
open! Expect_test_helpers_core
module Http = Server__Http

(* =============================================================================
   Helper Types and Functions
   ============================================================================= *)

(** Create a mock HTTP request *)
let make_request ?(method_ = Http.Http_method.GET) ?(path = "/test")
    ?(headers = []) () =
  Http.Request.create ~method_ ~path ~headers ()

(** Simple handler that returns a JSON response with a message *)
let simple_handler _request =
  Async.Deferred.return
    (Http.Response.json (`Assoc [ ("message", `String "hello") ]))

(** Handler that returns request path in response *)
let echo_path_handler request =
  Async.Deferred.return
    (Http.Response.json
       (`Assoc [ ("path", `String request.Http.Request.path) ]))

(* =============================================================================
   Custom Middleware Implementations
   ============================================================================= *)

(** Middleware that adds a custom header to all responses *)
let header_middleware ~header_name ~header_value : Http.Middleware.t =
 fun next request ->
  let%bind.Async.Deferred response = next request in
  let new_headers = response.headers @ [ (header_name, header_value) ] in
  Async.Deferred.return { response with headers = new_headers }

(** Middleware that appends to a tracking header to verify ordering *)
let tracking_middleware ~name : Http.Middleware.t =
 fun next request ->
  (* Add tracking header before calling next *)
  let existing =
    List.Assoc.find request.headers ~equal:String.equal "X-Middleware-Order"
    |> Option.value ~default:""
  in
  let new_order =
    if String.is_empty existing then name else existing ^ "," ^ name
  in
  let new_headers =
    List.Assoc.add request.headers ~equal:String.equal "X-Middleware-Order"
      new_order
  in
  let modified_request = { request with headers = new_headers } in
  let%bind.Async.Deferred response = next modified_request in
  (* Also add to response for verification *)
  let resp_headers =
    List.Assoc.add response.headers ~equal:String.equal "X-Middleware-Order"
      new_order
  in
  Async.Deferred.return { response with headers = resp_headers }

(** Run an Async.Deferred synchronously for tests *)
let run_async deferred =
  Async.Thread_safe.block_on_async_exn (fun () -> deferred)

(* =============================================================================
   Test: Middleware.identity
   ============================================================================= *)

let%expect_test "Middleware.identity - passes request through unchanged" =
  let request = make_request ~path:"/test-path" () in
  let handler = Http.Middleware.identity echo_path_handler in
  let response = run_async (handler request) in
  printf "status: %d\n" response.status;
  printf "body_contains_path: %b\n"
    (Option.value_map response.body ~default:false ~f:(fun b ->
         String.is_substring b ~substring:"/test-path"));
  [%expect {|
    status: 200
    body_contains_path: true
    |}]

let%expect_test "Middleware.identity - preserves response" =
  let request = make_request () in
  let handler = Http.Middleware.identity simple_handler in
  let response = run_async (handler request) in
  printf "status: %d\n" response.status;
  printf "has_body: %b\n" (Option.is_some response.body);
  [%expect {|
    status: 200
    has_body: true
    |}]

(* =============================================================================
   Test: Custom Header Middleware
   ============================================================================= *)

let%expect_test "header_middleware - adds custom header to response" =
  let middleware =
    header_middleware ~header_name:"X-Custom-Header" ~header_value:"test-value"
  in
  let request = make_request () in
  let handler = middleware simple_handler in
  let response = run_async (handler request) in
  let custom_header =
    List.Assoc.find response.headers ~equal:String.equal "X-Custom-Header"
  in
  printf "status: %d\n" response.status;
  printf "has_custom_header: %b\n" (Option.is_some custom_header);
  printf "header_value: %s\n" (Option.value custom_header ~default:"missing");
  [%expect
    {|
    status: 200
    has_custom_header: true
    header_value: test-value
    |}]

let%expect_test "header_middleware - does not remove existing headers" =
  let middleware = header_middleware ~header_name:"X-New" ~header_value:"new" in
  let original_handler _request =
    Async.Deferred.return
      (Http.Response.create ~status:200
         ~headers:[ ("X-Existing", "existing") ]
         ())
  in
  let request = make_request () in
  let handler = middleware original_handler in
  let response = run_async (handler request) in
  let existing =
    List.Assoc.find response.headers ~equal:String.equal "X-Existing"
  in
  let new_header =
    List.Assoc.find response.headers ~equal:String.equal "X-New"
  in
  printf "has_existing: %b\n" (Option.is_some existing);
  printf "has_new: %b\n" (Option.is_some new_header);
  [%expect {|
    has_existing: true
    has_new: true
    |}]

(* =============================================================================
   Test: Middleware.compose
   ============================================================================= *)

let%expect_test "Middleware.compose - empty list is identity" =
  let composed = Http.Middleware.compose [] in
  let request = make_request ~path:"/composed-test" () in
  let handler = composed echo_path_handler in
  let response = run_async (handler request) in
  printf "status: %d\n" response.status;
  printf "body_contains_path: %b\n"
    (Option.value_map response.body ~default:false ~f:(fun b ->
         String.is_substring b ~substring:"/composed-test"));
  [%expect {|
    status: 200
    body_contains_path: true
    |}]

let%expect_test "Middleware.compose - single middleware works" =
  let middleware =
    header_middleware ~header_name:"X-Single" ~header_value:"single-value"
  in
  let composed = Http.Middleware.compose [ middleware ] in
  let request = make_request () in
  let handler = composed simple_handler in
  let response = run_async (handler request) in
  let header =
    List.Assoc.find response.headers ~equal:String.equal "X-Single"
  in
  printf "has_header: %b\n" (Option.is_some header);
  printf "header_value: %s\n" (Option.value header ~default:"missing");
  [%expect {|
    has_header: true
    header_value: single-value
    |}]

let%expect_test "Middleware.compose - multiple middleware all applied" =
  let m1 = header_middleware ~header_name:"X-First" ~header_value:"first" in
  let m2 = header_middleware ~header_name:"X-Second" ~header_value:"second" in
  let m3 = header_middleware ~header_name:"X-Third" ~header_value:"third" in
  let composed = Http.Middleware.compose [ m1; m2; m3 ] in
  let request = make_request () in
  let handler = composed simple_handler in
  let response = run_async (handler request) in
  let first = List.Assoc.find response.headers ~equal:String.equal "X-First" in
  let second =
    List.Assoc.find response.headers ~equal:String.equal "X-Second"
  in
  let third = List.Assoc.find response.headers ~equal:String.equal "X-Third" in
  printf "has_first: %b\n" (Option.is_some first);
  printf "has_second: %b\n" (Option.is_some second);
  printf "has_third: %b\n" (Option.is_some third);
  [%expect
    {|
    has_first: true
    has_second: true
    has_third: true
    |}]

(* =============================================================================
   Test: Middleware Ordering
   ============================================================================= *)

let%expect_test "Middleware ordering - executed in correct order" =
  let m1 = tracking_middleware ~name:"first" in
  let m2 = tracking_middleware ~name:"second" in
  let m3 = tracking_middleware ~name:"third" in
  let composed = Http.Middleware.compose [ m1; m2; m3 ] in
  let request = make_request () in
  let handler = composed simple_handler in
  let response = run_async (handler request) in
  let order =
    List.Assoc.find response.headers ~equal:String.equal "X-Middleware-Order"
  in
  printf "order: %s\n" (Option.value order ~default:"none");
  (* With fold_right, first middleware is outermost, so it sets order first *)
  [%expect {| order: first |}]

(* =============================================================================
   Test: SSE and Streamable HTTP App with Middleware
   ============================================================================= *)

let%expect_test "create_sse_app - app is created with routes" =
  let server = `Assoc [ ("name", `String "TestServer") ] in
  let app =
    Http.create_sse_app ~server ~sse_path:"/sse" ~message_path:"/message" ()
  in
  printf "routes_count: %d\n" (List.length app.routes);
  printf "has_sse_route: %b\n"
    (List.exists app.routes ~f:(fun r -> String.is_prefix r.path ~prefix:"/sse"));
  [%expect {|
    routes_count: 2
    has_sse_route: true
    |}]

let%expect_test "create_streamable_http_app - with custom routes" =
  let server = `Assoc [ ("name", `String "TestServer") ] in
  let custom_route =
    Http.Route.create ~path:"/custom" ~methods:[ Http.Http_method.GET ]
      simple_handler
  in
  let app =
    Http.create_streamable_http_app ~server ~streamable_http_path:"/mcp"
      ~routes:[ custom_route ] ()
  in
  printf "routes_count: %d\n" (List.length app.routes);
  printf "has_mcp_route: %b\n"
    (List.exists app.routes ~f:(fun r -> String.equal r.path "/mcp"));
  printf "has_custom_route: %b\n"
    (List.exists app.routes ~f:(fun r -> String.equal r.path "/custom"));
  [%expect
    {|
    routes_count: 2
    has_mcp_route: true
    has_custom_route: true
    |}]

(* =============================================================================
   Test: Response Helpers
   ============================================================================= *)

let%expect_test "Response.json - creates JSON response with correct \
                 content-type" =
  let json = `Assoc [ ("key", `String "value") ] in
  let response = Http.Response.json json in
  printf "status: %d\n" response.status;
  printf "has_body: %b\n" (Option.is_some response.body);
  printf "body_contains_key: %b\n"
    (Option.value_map response.body ~default:false ~f:(fun b ->
         String.is_substring b ~substring:"key"));
  [%expect
    {|
    status: 200
    has_body: true
    body_contains_key: true
    |}]
