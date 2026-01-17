(** Unit tests for SSE Protocol Parser *)

open! Core
open! Async

module Sse = Mcp_client_transports.Sse_protocol

let%expect_test "parse single-line SSE event" =
  let parser = Sse.Parser.create () in
  let events = Sse.Parser.feed_line parser "data: hello world" in
  print_s [%sexp (events : Sse.Event.t list)];
  
  (* Blank line completes the event *)
  let events = Sse.Parser.feed_line parser "" in
  print_s [%sexp (events : Sse.Event.t list)];
  
  [%expect {|
    ()
    (((event_type message) (data "hello world") (id ()) (retry ())))
  |}];
  return ()

let%expect_test "parse multi-line data fields" =
  let parser = Sse.Parser.create () in
  let _ = Sse.Parser.feed_line parser "data: line 1" in
  let _ = Sse.Parser.feed_line parser "data: line 2" in
  let _ = Sse.Parser.feed_line parser "data: line 3" in
  let events = Sse.Parser.feed_line parser "" in
  
  print_s [%sexp (List.hd_exn events : Sse.Event.t)];
  [%expect {|
    ((event_type message) (data  "line 1\
                                \nline 2\
                                \nline 3") (id ()) (retry ()))
    |}];
  return ()

let%expect_test "parse event with ID field" =
  let parser = Sse.Parser.create () in
  let _ = Sse.Parser.feed_line parser "event: test" in
  let _ = Sse.Parser.feed_line parser "id: event-123" in
  let _ = Sse.Parser.feed_line parser "data: test data" in
  let events = Sse.Parser.feed_line parser "" in
  
  print_s [%sexp (List.hd_exn events : Sse.Event.t)];
  [%expect {|
    ((event_type test) (data "test data") (id (event-123)) (retry ()))
  |}];
  return ()

let%expect_test "parse event with retry field" =
  let parser = Sse.Parser.create () in
  let _ = Sse.Parser.feed_line parser "retry: 5000" in
  let _ = Sse.Parser.feed_line parser "data: reconnect test" in
  let events = Sse.Parser.feed_line parser "" in
  
  print_s [%sexp (List.hd_exn events : Sse.Event.t)];
  [%expect {|
    ((event_type message) (data "reconnect test") (id ()) (retry (5000)))
  |}];
  return ()

let%expect_test "parse JSON-RPC from event data" =
  let event = Sse.Event.create {|{"jsonrpc":"2.0","id":1,"result":{"status":"ok"}}|} in
  
  (match Sse.Event.parse_jsonrpc event with
  | Ok (`Response resp) ->
      printf "Result: Response with id ";
      (match resp.id with
      | `Int i -> printf "(Int %d)" i
      | `String s -> printf "(String %s)" s);
      printf " and jsonrpc %s\n" resp.jsonrpc
  | Ok (`Request _) -> print_endline "Result: Request"
  | Ok (`Notification _) -> print_endline "Result: Notification"
  | Ok (`Error _) -> print_endline "Result: Error"
  | Error err ->
      print_endline (Error.to_string_hum err));
  [%expect {|
    Result: Response with id (Int 1) and jsonrpc 2.0
  |}];
  return ()

let%expect_test "handle malformed JSON in event gracefully" =
  let event = Sse.Event.create "not valid json" in
  
  (match Sse.Event.parse_jsonrpc event with
  | Ok _ ->
      print_endline "Unexpected success"
  | Error err ->
      print_endline "Parse failed as expected";
      printf "Error contains 'parse': %b\n"
        (String.is_substring (Error.to_string_hum err) ~substring:"parse"));
  
  [%expect {|
    Parse failed as expected
    Error contains 'parse': true
  |}];
  return ()

let%expect_test "ignore comment lines starting with colon" =
  let parser = Sse.Parser.create () in
  let events = Sse.Parser.feed_line parser ": this is a comment" in
  print_s [%sexp (events : Sse.Event.t list)];
  
  [%expect {| () |}];
  return ()

let%expect_test "handle field with no colon as empty value" =
  let parser = Sse.Parser.create () in
  let _ = Sse.Parser.feed_line parser "data" in
  let events = Sse.Parser.feed_line parser "" in
  
  print_s [%sexp (List.hd_exn events : Sse.Event.t)];
  [%expect {|
    ((event_type message) (data "") (id ()) (retry ()))
  |}];
  return ()

let%expect_test "handle field with leading space after colon" =
  let parser = Sse.Parser.create () in
  let _ = Sse.Parser.feed_line parser "data:  value with spaces  " in
  let events = Sse.Parser.feed_line parser "" in
  
  print_s [%sexp (List.hd_exn events : Sse.Event.t)];
  [%expect {|
    ((event_type message) (data " value with spaces  ") (id ()) (retry ()))
  |}];
  return ()

let%expect_test "multiple events in sequence" =
  let parser = Sse.Parser.create () in
  
  (* First event *)
  let _ = Sse.Parser.feed_line parser "data: event 1" in
  let events1 = Sse.Parser.feed_line parser "" in
  
  (* Second event *)
  let _ = Sse.Parser.feed_line parser "data: event 2" in
  let events2 = Sse.Parser.feed_line parser "" in
  
  print_s [%sexp (List.hd_exn events1 : Sse.Event.t)];
  print_s [%sexp (List.hd_exn events2 : Sse.Event.t)];
  
  [%expect {|
    ((event_type message) (data "event 1") (id ()) (retry ()))
    ((event_type message) (data "event 2") (id ()) (retry ()))
  |}];
  return ()

let%expect_test "reset event type to default after each event" =
  let parser = Sse.Parser.create () in
  
  (* Custom event type *)
  let _ = Sse.Parser.feed_line parser "event: special" in
  let _ = Sse.Parser.feed_line parser "data: first" in
  let events1 = Sse.Parser.feed_line parser "" in
  
  (* No event type specified - should reset to "message" *)
  let _ = Sse.Parser.feed_line parser "data: second" in
  let events2 = Sse.Parser.feed_line parser "" in
  
  print_s [%sexp ((List.hd_exn events1).event_type : string)];
  print_s [%sexp ((List.hd_exn events2).event_type : string)];
  
  [%expect {|
    special
    message
  |}];
  return ()
