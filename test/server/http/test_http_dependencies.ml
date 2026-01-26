(** Tests for HTTP Dependencies module.

    Translated from Python test_http_dependencies.py to OCaml. Tests HTTP
    request context management and header extraction from dependencies module.

    Note: Python tests require Client/transports not available in OCaml. These
    tests focus on unit testing the dependencies module directly. *)

open! Core
open! Expect_test_helpers_core
module Dependencies = Ox_fast_mcp_server__Dependencies

(* =============================================================================
   Helper Functions
   ============================================================================= *)

(** Create a mock Cohttp request with given headers *)
let make_mock_request ?(headers = []) ?(meth = `GET) ~uri () =
  let uri = Uri.of_string uri in
  let cohttp_headers = Cohttp.Header.of_list headers in
  Cohttp.Request.make ~meth ~headers:cohttp_headers uri

(* =============================================================================
   Test: Http_request Context Management
   ============================================================================= *)

let%expect_test "Http_request.set_request and get_request - basic flow" =
  let req =
    make_mock_request ~uri:"http://localhost/test"
      ~headers:[ ("x-demo-header", "ABC") ]
      ()
  in
  Dependencies.Http_request.set_request req;
  let retrieved = Dependencies.Http_request.get_request () in
  printf "uri_path: %s\n" (Uri.path (Cohttp.Request.uri retrieved));
  Dependencies.Http_request.clear_request ();
  [%expect {| uri_path: /test |}]

let%expect_test "Http_request.get_request - fails when no request set" =
  Dependencies.Http_request.clear_request ();
  let raised =
    match Dependencies.Http_request.get_request () with
    | _ -> false
    | exception Failure _ -> true
  in
  printf "raised_exception: %b\n" raised;
  [%expect {| raised_exception: true |}]

let%expect_test "Http_request.clear_request - clears the request" =
  let req = make_mock_request ~uri:"http://localhost/path" () in
  Dependencies.Http_request.set_request req;
  Dependencies.Http_request.clear_request ();
  let raised =
    match Dependencies.Http_request.get_request () with
    | _ -> false
    | exception Failure _ -> true
  in
  printf "cleared: %b\n" raised;
  [%expect {| cleared: true |}]

(* =============================================================================
   Test: Http_request.get_headers
   ============================================================================= *)

let%expect_test "get_headers - extracts custom headers" =
  let req =
    make_mock_request ~uri:"http://localhost/test"
      ~headers:
        [
          ("X-Demo-Header", "ABC");
          ("X-Custom", "value");
          ("Accept", "text/plain");
        ]
      ()
  in
  Dependencies.Http_request.set_request req;
  let headers = Dependencies.Http_request.get_headers () in
  (* Headers should be lowercased *)
  printf "has_x_demo_header: %b\n" (Map.mem headers "x-demo-header");
  printf "x_demo_header_value: %s\n"
    (Map.find headers "x-demo-header" |> Option.value ~default:"missing");
  printf "has_x_custom: %b\n" (Map.mem headers "x-custom");
  Dependencies.Http_request.clear_request ();
  [%expect
    {|
    has_x_demo_header: true
    x_demo_header_value: ABC
    has_x_custom: true
    |}]

let%expect_test "get_headers - excludes problematic headers by default" =
  let req =
    make_mock_request ~uri:"http://localhost/test"
      ~headers:
        [
          ("X-Demo-Header", "ABC");
          ("Content-Length", "100");
          ("Host", "localhost");
          ("Connection", "keep-alive");
        ]
      ()
  in
  Dependencies.Http_request.set_request req;
  let headers = Dependencies.Http_request.get_headers () in
  (* Should have custom header but not excluded ones *)
  printf "has_x_demo_header: %b\n" (Map.mem headers "x-demo-header");
  printf "has_content_length: %b\n" (Map.mem headers "content-length");
  printf "has_host: %b\n" (Map.mem headers "host");
  printf "has_connection: %b\n" (Map.mem headers "connection");
  Dependencies.Http_request.clear_request ();
  [%expect
    {|
    has_x_demo_header: true
    has_content_length: false
    has_host: false
    has_connection: false
    |}]

let%expect_test "get_headers - include_all returns all headers" =
  let req =
    make_mock_request ~uri:"http://localhost/test"
      ~headers:
        [
          ("X-Demo-Header", "ABC");
          ("Content-Length", "100");
          ("Host", "localhost");
        ]
      ()
  in
  Dependencies.Http_request.set_request req;
  let headers = Dependencies.Http_request.get_headers ~include_all:true () in
  printf "has_x_demo_header: %b\n" (Map.mem headers "x-demo-header");
  printf "has_content_length: %b\n" (Map.mem headers "content-length");
  printf "has_host: %b\n" (Map.mem headers "host");
  Dependencies.Http_request.clear_request ();
  [%expect
    {|
    has_x_demo_header: true
    has_content_length: true
    has_host: true
    |}]

let%expect_test "get_headers - returns empty map when no request" =
  Dependencies.Http_request.clear_request ();
  let headers = Dependencies.Http_request.get_headers () in
  printf "is_empty: %b\n" (Map.is_empty headers);
  [%expect {| is_empty: true |}]

let%expect_test "get_headers - header names are lowercase" =
  let req =
    make_mock_request ~uri:"http://localhost/test"
      ~headers:[ ("X-UPPER-CASE", "value"); ("X-Mixed-Case", "value2") ]
      ()
  in
  Dependencies.Http_request.set_request req;
  let headers = Dependencies.Http_request.get_headers () in
  printf "has_lowercase_upper: %b\n" (Map.mem headers "x-upper-case");
  printf "has_lowercase_mixed: %b\n" (Map.mem headers "x-mixed-case");
  (* Original case should not exist *)
  printf "has_original_upper: %b\n" (Map.mem headers "X-UPPER-CASE");
  Dependencies.Http_request.clear_request ();
  [%expect
    {|
    has_lowercase_upper: true
    has_lowercase_mixed: true
    has_original_upper: false
    |}]

(* =============================================================================
   Test: Context Module
   ============================================================================= *)

let%expect_test "Context.get_context - fails when no context set" =
  Dependencies.Context.clear_context ();
  let raised =
    match Dependencies.Context.get_context () with
    | _ -> false
    | exception Failure _ -> true
  in
  printf "raised_exception: %b\n" raised;
  [%expect {| raised_exception: true |}]

(* =============================================================================
   Test: Access_token Module
   ============================================================================= *)

let%expect_test "Access_token.get_token - returns None when no request" =
  Dependencies.Http_request.clear_request ();
  let token = Dependencies.Access_token.get_token () in
  printf "has_token: %b\n" (Option.is_some token);
  [%expect {| has_token: false |}]

let%expect_test "Access_token.get_token - extracts Bearer token" =
  let req =
    make_mock_request ~uri:"http://localhost/test"
      ~headers:[ ("Authorization", "Bearer test-token-123") ]
      ()
  in
  Dependencies.Http_request.set_request req;
  let token = Dependencies.Access_token.get_token () in
  printf "has_token: %b\n" (Option.is_some token);
  printf "token_value: %s\n" (Option.value token ~default:"none");
  Dependencies.Http_request.clear_request ();
  [%expect {|
    has_token: true
    token_value: test-token-123
    |}]

let%expect_test "Access_token.get_token - returns None for non-Bearer auth" =
  let req =
    make_mock_request ~uri:"http://localhost/test"
      ~headers:[ ("Authorization", "Basic dXNlcjpwYXNz") ]
      ()
  in
  Dependencies.Http_request.set_request req;
  let token = Dependencies.Access_token.get_token () in
  printf "has_token: %b\n" (Option.is_some token);
  Dependencies.Http_request.clear_request ();
  [%expect {| has_token: false |}]
