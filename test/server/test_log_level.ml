(** Log level parameter tests for OxFastMCP

    Test log_level parameter support in OxFastMCP server. *)

open Core
open Async

let%expect_test "run_stdio_accepts_log_level" =
  (* Test that run_async with stdio transport accepts log_level parameter *)
  let server = Server.Ox_fast_mcp.create ~name:"TestServer" () in
  (* This should accept the log_level parameter without error *)
  let%bind () =
    Server.Ox_fast_mcp.run_async server ~transport:Server.Transport.Stdio
      ~log_level:"debug" ()
  in
  [%expect
    {|
    INFO Setting log level to debug
    INFO Starting OxFastMCP server with stdio transport
    INFO EOF on stdin, shutting down
    |}];
  return ()

let%expect_test "run_http_accepts_log_level" =
  (* Test that run_async with SSE transport accepts log_level parameter *)
  (* Suppress log output to avoid dynamic timestamps in test expectations *)
  Log.Global.set_output [];
  let server = Server.Ox_fast_mcp.create ~name:"TestServer" () in
  (* HTTP servers run indefinitely, so we just verify the parameter is accepted *)
  (* and initialization starts without error *)
  let (_ : unit Deferred.t) =
    Server.Ox_fast_mcp.run_async server ~transport:Server.Transport.Sse
      ~host:"127.0.0.1" ~port:8000 ~log_level:"info" ()
  in
  (* Give it a moment to start up *)
  let%bind () = after (sec 0.1) in
  (* Test passes if no exception is thrown *)
  return ()

let%expect_test "run_sse_accepts_log_level" =
  (* Test that run_async with SSE transport accepts log_level parameter *)
  (* Suppress log output to avoid dynamic timestamps in test expectations *)
  Log.Global.set_output [];
  let server = Server.Ox_fast_mcp.create ~name:"TestServer" () in
  let (_ : unit Deferred.t) =
    Server.Ox_fast_mcp.run_async server ~transport:Server.Transport.Sse
      ~host:"127.0.0.1" ~port:8001 ~log_level:"info" ()
  in
  (* Give it a moment to start - this test just verifies no exception is
     thrown *)
  let%bind () = after (Time_float.Span.of_ms 100.0) in
  (* If we got here without an exception, the test passed *)
  return ()

let%expect_test "run_async_passes_log_level" =
  (* Test that run_async passes log_level to transport methods *)
  let server = Server.Ox_fast_mcp.create ~name:"TestServer" () in
  (* Test with different log levels *)
  let%bind () =
    Server.Ox_fast_mcp.run_async server ~transport:Server.Transport.Stdio
      ~log_level:"warning" ()
  in
  [%expect
    {|
    INFO Setting log level to warning
    INFO Starting OxFastMCP server with stdio transport
    INFO EOF on stdin, shutting down
    |}];
  return ()

let%expect_test "run_without_log_level" =
  (* Test that omitting log_level works (it's optional) *)
  let server = Server.Ox_fast_mcp.create ~name:"TestServer" () in
  let%bind () =
    Server.Ox_fast_mcp.run_async server ~transport:Server.Transport.Stdio ()
  in
  [%expect
    {|
    INFO Starting OxFastMCP server with stdio transport
    INFO EOF on stdin, shutting down
    |}];
  return ()
