(** Tests for Context module capability checking

    Tests that sample() and elicit() methods properly check client capabilities
    before attempting operations. *)

open! Core
open! Async
open! Expect_test_helpers_core
module Context = Ox_fast_mcp_server__Context

(** Test sample() fails without session *)
let%expect_test "sample fails without session" =
  let%bind () =
    let ctx = Context.create () in
    let%bind result =
      Monitor.try_with ~extract_exn:true (fun () ->
          Context.sample ctx ~messages:[] ())
    in
    (match result with
    | Ok _ -> print_endline "FAIL: Should have raised an exception"
    | Error exn ->
      let error_msg = Exn.to_string exn in
      if String.is_substring error_msg ~substring:"no active session" then
        print_endline "PASS: Correct error for missing session"
      else (
        print_endline "FAIL: Unexpected error message";
        print_endline error_msg));
    return ()
  in
  [%expect {| PASS: Correct error for missing session |}];
  return ()

(** Test elicit() fails without session *)
let%expect_test "elicit fails without session" =
  let%bind () =
    let ctx = Context.create () in
    let requested_schema =
      `Assoc [ ("type", `String "string"); ("description", `String "test") ]
    in
    let%bind result =
      Monitor.try_with ~extract_exn:true (fun () ->
          Context.elicit ctx ~message:"test" ~requested_schema ())
    in
    (match result with
    | Ok _ -> print_endline "FAIL: Should have raised an exception"
    | Error exn ->
      let error_msg = Exn.to_string exn in
      if String.is_substring error_msg ~substring:"no active session" then
        print_endline "PASS: Correct error for missing session"
      else (
        print_endline "FAIL: Unexpected error message";
        print_endline error_msg));
    return ()
  in
  [%expect {| PASS: Correct error for missing session |}];
  return ()

(** Test sample error message mentions sampling *)
let%expect_test "sample error mentions sampling requirement" =
  let%bind () =
    let ctx = Context.create () in
    let%bind result =
      Monitor.try_with ~extract_exn:true (fun () ->
          Context.sample ctx ~messages:[] ())
    in
    (match result with
    | Ok _ -> print_endline "Should have failed"
    | Error exn ->
      let error_msg = Exn.to_string exn in
      let has_session_mention =
        String.is_substring error_msg ~substring:"session"
      in
      let has_sampling_mention =
        String.is_substring error_msg ~substring:"sampl"
      in
      print_s
        [%message
          "Error message quality"
            (has_session_mention : bool)
            (has_sampling_mention : bool)]);
    return ()
  in
  [%expect
    {|
    ("Error message quality"
      (has_session_mention  true)
      (has_sampling_mention true))
    |}];
  return ()

(** Test elicit error message mentions elicitation *)
let%expect_test "elicit error mentions elicitation requirement" =
  let%bind () =
    let ctx = Context.create () in
    let requested_schema = `Assoc [ ("type", `String "string") ] in
    let%bind result =
      Monitor.try_with ~extract_exn:true (fun () ->
          Context.elicit ctx ~message:"test" ~requested_schema ())
    in
    (match result with
    | Ok _ -> print_endline "Should have failed"
    | Error exn ->
      let error_msg = Exn.to_string exn in
      let has_session_mention =
        String.is_substring error_msg ~substring:"session"
      in
      let has_elicit_mention =
        String.is_substring error_msg ~substring:"elicit"
      in
      print_s
        [%message
          "Error message quality"
            (has_session_mention : bool)
            (has_elicit_mention : bool)]);
    return ()
  in
  [%expect
    {|
    ("Error message quality"
      (has_session_mention true)
      (has_elicit_mention  true))
    |}];
  return ()

(** Test that context can be created without a session *)
let%expect_test "context creation without session succeeds" =
  let ctx = Context.create () in
  let has_session =
    match ctx.session with
    | None -> false
    | Some _ -> true
  in
  print_s [%message (has_session : bool)];
  [%expect {| (has_session false) |}];
  return ()

(** Test sample with various parameters still fails without session *)
let%expect_test "sample with params fails without session" =
  let%bind () =
    let ctx = Context.create () in
    let messages = [] in
    let%bind result =
      Monitor.try_with ~extract_exn:true (fun () ->
          Context.sample ctx ~messages ~max_tokens:500 ~temperature:0.7
            ~stop_sequences:[ "DONE" ] ())
    in
    let is_error = Result.is_error result in
    print_s [%message "Should fail" (is_error : bool)];
    return ()
  in
  [%expect {| ("Should fail" (is_error true)) |}];
  return ()
