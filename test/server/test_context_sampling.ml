(** Tests for Context sampling and elicitation methods *)

open! Core
open! Async
open! Expect_test_helpers_core

(* Access Context using internal module name *)
module Context = Ox_fast_mcp_server__Context

let create_test_context ?session () = Context.create ?session ()

(** Helper to create a text content *)
let make_text_content text =
  { Mcp.Types.type_ = `Text; text; annotations = None; meta = None }

(** Helper to create a sampling message *)
let make_user_message text : Mcp.Types.sampling_message =
  { role = `User; content = `Text (make_text_content text); meta = None }

(** {1 Sampling Tests} *)

let%expect_test "sample with no session raises error" =
  let%bind () =
    let ctx = create_test_context () in
    let messages = [ make_user_message "Hello" ] in
    (* This should raise an error *)
    let%bind result =
      Monitor.try_with ~extract_exn:true (fun () ->
          Context.sample ctx ~messages ())
    in
    match result with
    | Ok _ ->
      print_endline "Unexpected success";
      return ()
    | Error exn ->
      print_endline (Exn.to_string exn);
      return ()
  in
  [%expect
    {|
    (Failure
      "Cannot sample: no active session. Sampling requires an active MCP session.")
    |}];
  return ()

let%expect_test "elicit with no session raises error" =
  let%bind () =
    let ctx = create_test_context () in
    (* This should raise an error *)
    let%bind result =
      Monitor.try_with ~extract_exn:true (fun () ->
          Context.elicit ctx ~message:"What is your name?"
            ~requested_schema:(`Assoc []) ())
    in
    match result with
    | Ok _ ->
      print_endline "Unexpected success";
      return ()
    | Error exn ->
      print_endline (Exn.to_string exn);
      return ()
  in
  [%expect
    {|
    (Failure
      "Cannot elicit: no active session. Elicitation requires an active MCP session.")
    |}];
  return ()

(** Note: Tests with actual sessions require creating a session_bridge, which
    needs Lwt/Async integration. These are integration tests that would be
    better handled in a separate test suite. *)

let%expect_test "sample signature accepts all parameters" =
  (* This test verifies the signature compiles correctly *)
  let ctx = create_test_context () in
  let messages = [ make_user_message "Test" ] in
  (* Try to call with all optional parameters (will fail, but that's
     expected) *)
  (try
     let (_ : Mcp.Types.client_request Deferred.t) =
       Context.sample ctx ~messages ~max_tokens:500
         ~system_prompt:"You are helpful" ~temperature:0.7
         ~stop_sequences:[ "END" ] ()
     in
     print_endline "Should not reach here"
   with Failure msg ->
     print_endline "Caught expected failure";
     print_s
       [%sexp (String.is_substring msg ~substring:"no active session" : bool)]);
  [%expect {|
    Caught expected failure
    true |}];
  return ()

let%expect_test "elicit signature works correctly" =
  (* Verify the signature compiles *)
  let ctx = create_test_context () in
  (try
     let (_ : Mcp.Types.client_request Deferred.t) =
       Context.elicit ctx ~message:"Enter value" ~requested_schema:(`Assoc [])
         ()
     in
     print_endline "Should not reach here"
   with Failure msg ->
     print_endline "Caught expected failure";
     print_s
       [%sexp (String.is_substring msg ~substring:"no active session" : bool)]);
  [%expect {|
    Caught expected failure
    true |}];
  return ()
