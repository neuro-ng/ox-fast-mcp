(** Unit tests for StreamableHTTP Transport *)

open! Core
open! Async

module Streamable = Mcp_client_transports.Streamable_http

(** {1 Connect and State Tests} *)

let%expect_test "connect creates transport with no session" =
  let config = Streamable.create_config ~url:"http://localhost:8080/mcp" () in
  let transport = Streamable.connect config in
  print_s [%sexp (Streamable.get_session_id transport : string option)];
  [%expect {| () |}];
  
  (* Clean up *)
  let%bind () = Streamable.close transport ~terminate_on_close:false in
  return ()

let%expect_test "read_stream and write_stream are accessible" =
  let config = Streamable.create_config ~url:"http://localhost:8080/mcp" () in
  let transport = Streamable.connect config in
  
  (* Verify streams are valid pipes *)
  let read_stream = Streamable.read_stream transport in
  let write_stream = Streamable.write_stream transport in
  
  print_s [%sexp (Pipe.is_closed read_stream : bool)];
  print_s [%sexp (Pipe.is_closed write_stream : bool)];
  [%expect {|
    false
    false
  |}];
  
  (* Clean up *)
  let%bind () = Streamable.close transport ~terminate_on_close:false in
  return ()

let%expect_test "close closes both streams" =
  let config = Streamable.create_config ~url:"http://localhost:8080/mcp" () in
  let transport = Streamable.connect config in
  
  let read_stream = Streamable.read_stream transport in
  let write_stream = Streamable.write_stream transport in
  
  let%bind () = Streamable.close transport ~terminate_on_close:false in
  
  (* Both streams should be closed after close *)
  print_s [%sexp (Pipe.is_closed read_stream : bool)];
  print_s [%sexp (Pipe.is_closed write_stream : bool)];
  [%expect {|
    true
    true
  |}];
  return ()

(** {1 Error Type Tests} *)

let%expect_test "error types produce correct messages" =
  let err1 = Streamable.Error.Streamable_http_error "connection failed" in
  let err2 = Streamable.Error.Resumption_error "invalid token" in
  
  print_endline (Streamable.Error.to_string err1);
  print_endline (Streamable.Error.to_string err2);
  
  [%expect {|
    StreamableHTTP error: connection failed
    Resumption error: invalid token
  |}];
  return ()

(** {1 Message Detection Tests} *)

let%expect_test "is_initialization_request detects initialize method" =
  let init_request : Mcp.Types.jsonrpc_message =
    `Request { jsonrpc = "2.0"; id = `Int 1; method_ = "initialize"; params = None }
  in
  let other_request : Mcp.Types.jsonrpc_message =
    `Request { jsonrpc = "2.0"; id = `Int 2; method_ = "ping"; params = None }
  in
  
  print_s [%sexp (Streamable.is_initialization_request init_request : bool)];
  print_s [%sexp (Streamable.is_initialization_request other_request : bool)];
  
  [%expect {|
    true
    false
  |}];
  return ()

let%expect_test "is_initialized_notification detects notifications/initialized" =
  let init_notif : Mcp.Types.jsonrpc_message =
    `Notification { jsonrpc = "2.0"; method_ = "notifications/initialized"; params = None }
  in
  let other_notif : Mcp.Types.jsonrpc_message =
    `Notification { jsonrpc = "2.0"; method_ = "notifications/cancelled"; params = None }
  in
  
  print_s [%sexp (Streamable.is_initialized_notification init_notif : bool)];
  print_s [%sexp (Streamable.is_initialized_notification other_notif : bool)];
  
  [%expect {|
    true
    false
  |}];
  return ()

(** {1 Header Constants Tests} *)

let%expect_test "header constants are correct" =
  print_endline Streamable.mcp_session_id;
  print_endline Streamable.mcp_protocol_version;
  print_endline Streamable.content_type_header;
  print_endline Streamable.accept_header;
  
  [%expect {|
    mcp-session-id
    mcp-protocol-version
    content-type
    accept
  |}];
  return ()

let%expect_test "content type constants are correct" =
  print_endline Streamable.json_content_type;
  print_endline Streamable.sse_content_type;
  
  [%expect {|
    application/json
    text/event-stream
  |}];
  return ()
