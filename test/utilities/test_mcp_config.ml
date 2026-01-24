(** Tests for MCP Configuration module *)

open! Core
open! Async
open! Expect_test_helpers_core
open Mcp_config

let%expect_test "Transport_type to_string and of_string" =
  let test transport =
    let str = Transport_type.to_string transport in
    let back = Transport_type.of_string str in
    print_s [%sexp (str : string)];
    require ~here:[%here] (Transport_type.equal transport back)
  in
  test Transport_type.Stdio;
  test Transport_type.Sse;
  test Transport_type.Streamable_http;
  [%expect {|
    stdio
    sse
    streamable-http |}];
  return ()

let%expect_test "Transport_type of_string variants" =
  print_s [%sexp (Transport_type.of_string "http" : Transport_type.t)];
  [%expect {| Streamable_http |}];
  print_s
    [%sexp (Transport_type.of_string "streamable-http" : Transport_type.t)];
  [%expect {| Streamable_http |}];
  return ()

let%expect_test "create_stdio_server" =
  let server = create_stdio_server ~command:"python" ~args:[ "-m"; "mcp" ] () in
  print_s [%sexp (server.command : string)];
  print_s [%sexp (server.args : string list)];
  [%expect {|
    python
    (-m mcp) |}];
  return ()

let%expect_test "create_remote_server" =
  let server =
    create_remote_server ~url:"http://localhost:8080/mcp"
      ~transport:Transport_type.Streamable_http ()
  in
  print_s [%sexp (server.url : string)];
  print_s [%sexp (server.transport : Transport_type.t option)];
  [%expect {|
    http://localhost:8080/mcp
    (Streamable_http) |}];
  return ()

let%expect_test "infer_transport_type_from_url sse" =
  let url = "http://localhost:8080/sse" in
  let transport = infer_transport_type_from_url url in
  print_s [%sexp (transport : Transport_type.t)];
  [%expect {| Sse |}];
  let url2 = "http://localhost:8080/sse/" in
  let transport2 = infer_transport_type_from_url url2 in
  print_s [%sexp (transport2 : Transport_type.t)];
  [%expect {| Sse |}];
  return ()

let%expect_test "infer_transport_type_from_url http" =
  let url = "http://localhost:8080/mcp" in
  let transport = infer_transport_type_from_url url in
  print_s [%sexp (transport : Transport_type.t)];
  [%expect {| Streamable_http |}];
  return ()

let%expect_test "mcp_config add and get server" =
  let config = create_config () in
  let server = Stdio (create_stdio_server ~command:"node" ()) in
  let config = add_server config ~name:"test-server" ~server in
  let found = get_server config ~name:"test-server" in
  require ~here:[%here] (Option.is_some found);
  print_s [%sexp (Option.is_some found : bool)];
  [%expect {| true |}];
  return ()

let%expect_test "mcp_config remove server" =
  let config = create_config () in
  let server = Stdio (create_stdio_server ~command:"node" ()) in
  let config = add_server config ~name:"test-server" ~server in
  let config = remove_server config ~name:"test-server" in
  let found = get_server config ~name:"test-server" in
  print_s [%sexp (Option.is_none found : bool)];
  [%expect {| true |}];
  return ()

let%expect_test "stdio_mcp_server JSON round-trip" =
  let server =
    create_stdio_server ~command:"python" ~args:[ "-m"; "server" ]
      ~env:[ ("DEBUG", "true") ]
      ~cwd:"/tmp" ()
  in
  let json = yojson_of_stdio_mcp_server server in
  let back = stdio_mcp_server_of_yojson json in
  require ~here:[%here] (String.equal server.command back.command);
  require ~here:[%here] (List.equal String.equal server.args back.args);
  print_s [%sexp (back.command : string)];
  [%expect {| python |}];
  return ()

let%expect_test "remote_mcp_server JSON round-trip" =
  let server =
    create_remote_server ~url:"http://localhost:8080"
      ~headers:[ ("Authorization", "Bearer token") ]
      ~timeout:5000 ()
  in
  let json = yojson_of_remote_mcp_server server in
  let back = remote_mcp_server_of_yojson json in
  require ~here:[%here] (String.equal server.url back.url);
  print_s [%sexp (back.url : string)];
  [%expect {| http://localhost:8080 |}];
  return ()

let%expect_test "mcp_server_of_yojson stdio" =
  let json =
    `Assoc
      [ ("command", `String "node"); ("args", `List [ `String "server.js" ]) ]
  in
  let server = mcp_server_of_yojson json in
  (match server with
  | Stdio s -> print_s [%sexp (s.command : string)]
  | Remote _ -> failwith "Expected Stdio server");
  [%expect {| node |}];
  return ()

let%expect_test "mcp_server_of_yojson remote" =
  let json = `Assoc [ ("url", `String "http://localhost:8080") ] in
  let server = mcp_server_of_yojson json in
  (match server with
  | Remote r -> print_s [%sexp (r.url : string)]
  | Stdio _ -> failwith "Expected Remote server");
  [%expect {| http://localhost:8080 |}];
  return ()

let%expect_test "get_transport_type with explicit transport" =
  let server =
    create_remote_server ~url:"http://localhost/mcp"
      ~transport:Transport_type.Sse ()
  in
  let transport = get_transport_type server in
  print_s [%sexp (transport : Transport_type.t)];
  [%expect {| Sse |}];
  return ()

let%expect_test "get_transport_type inferred" =
  let server = create_remote_server ~url:"http://localhost/sse" () in
  let transport = get_transport_type server in
  print_s [%sexp (transport : Transport_type.t)];
  [%expect {| Sse |}];
  return ()
