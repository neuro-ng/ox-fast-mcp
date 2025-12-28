(** Log level parameter tests for OxFastMCP

    Test log_level parameter support in OxFastMCP server. *)

open Async

let%expect_test "run_stdio_accepts_log_level" =
  (* Test that run_async with stdio transport accepts log_level parameter *)
  let server = Server.Ox_fast_mcp.create ~name:"TestServer" () in
  (* This should accept the log_level parameter without error *)
  let%bind () =
    Server.Ox_fast_mcp.run_async server ~transport:Server.Transport.Stdio
      ~log_level:"DEBUG" ()
  in
  [%expect
    {|
    INFO Setting log level to DEBUG
    INFO Starting OxFastMCP server with stdio transport
    INFO EOF on stdin, shutting down
    |}];
  return ()

let%expect_test "run_http_accepts_log_level" =
  (* Test that run_async with HTTP transport accepts log_level parameter *)
  let server = Server.Ox_fast_mcp.create ~name:"TestServer" () in
  (* This should accept the log_level parameter without error *)
  let%bind () =
    Server.Ox_fast_mcp.run_async server ~transport:Server.Transport.Http
      ~host:"127.0.0.1" ~port:8000 ~log_level:"INFO" ()
  in
  [%expect
    {|
    INFO Setting log level to INFO
    INFO Starting OxFastMCP server with HTTP transport
    |}];
  return ()

let%expect_test "run_async_passes_log_level" =
  (* Test that run_async passes log_level to transport methods *)
  let server = Server.Ox_fast_mcp.create ~name:"TestServer" () in
  (* Test with different log levels *)
  let%bind () =
    Server.Ox_fast_mcp.run_async server ~transport:Server.Transport.Stdio
      ~log_level:"WARNING" ()
  in
  [%expect
    {|
    INFO Setting log level to WARNING
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
