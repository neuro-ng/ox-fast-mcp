(** OAuth Callback Server Tests

    Tests for the OAuth callback HTTP server. *)

open! Core
open! Async
open! Expect_test_helpers_core

(** {1 HTML Template Tests} *)

let%expect_test "success_html contains expected content" =
  let html = Client_auth.Oauth_callback.success_html in
  let has_title = String.is_substring html ~substring:"Authorization Successful" in
  let has_close_msg = String.is_substring html ~substring:"close this window" in
  printf "Has title: %b\n" has_title;
  printf "Has close message: %b\n" has_close_msg;
  [%expect {|
    Has title: true
    Has close message: true |}];
  return ()

let%expect_test "error_html contains error message" =
  let html = Client_auth.Oauth_callback.error_html ~error:"Test error message" in
  let has_title = String.is_substring html ~substring:"Authorization Failed" in
  let has_error = String.is_substring html ~substring:"Test error message" in
  printf "Has failed title: %b\n" has_title;
  printf "Has error message: %b\n" has_error;
  [%expect {|
    Has failed title: true
    Has error message: true |}];
  return ()

(** {1 Callback Result Type Tests} *)

let%expect_test "callback_result with code" =
  let result : Client_auth.Oauth_callback.callback_result = {
    code = Some "test_code_123";
    state = Some "xyz_state";
    error = None;
    error_description = None;
  } in
  printf "Has code: %b\n" (Option.is_some result.code);
  printf "Has state: %b\n" (Option.is_some result.state);
  printf "Has error: %b\n" (Option.is_some result.error);
  [%expect {|
    Has code: true
    Has state: true
    Has error: false |}];
  return ()

let%expect_test "callback_result with error" =
  let result : Client_auth.Oauth_callback.callback_result = {
    code = None;
    state = Some "xyz_state";
    error = Some "access_denied";
    error_description = Some "User denied access";
  } in
  printf "Has code: %b\n" (Option.is_some result.code);
  printf "Has error: %b\n" (Option.is_some result.error);
  printf "Error: %s\n" (Option.value result.error ~default:"none");
  printf "Description: %s\n" (Option.value result.error_description ~default:"none");
  [%expect {|
    Has code: false
    Has error: true
    Error: access_denied
    Description: User denied access |}];
  return ()

(** {1 Integration Note} *)

(* Full integration tests require:
   1. Actually starting the HTTP server on a port
   2. Making HTTP requests to the callback endpoint
   3. This is more suitable for integration test suite
   
   The start_callback_server function is tested via manual verification:
   - Run server manually
   - curl http://localhost:PORT/callback?code=test&state=xyz
   - Verify HTML response and Ivar fill
   
   Example manual test:
   ```ocaml
   let () = 
     let open Async in
     don't_wait_for (
       let%bind result = Client_auth.Oauth_callback.start_callback_server 
         ~port:8765 
         ~timeout:(Time_ns.Span.of_sec 30.0) 
       in
       printf "Got code: %s\n" (Option.value result.code ~default:"none");
       return ()
     );
     Scheduler.go ()
   ```
*)
