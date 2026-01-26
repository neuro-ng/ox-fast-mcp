open Core
open Async
open Alcotest_async

(* Test infer transport *)
module Transport_tests = struct
  let test_sse_transport_inference switch () =
    let sse_urls =
      [
        "http://example.com/api/sse/stream";
        "https://localhost:8080/mcp/sse/endpoint";
        "http://example.com/api/sse";
        "http://example.com/api/sse/";
        "https://localhost:8080/mcp/sse/";
        "http://example.com/api/sse?param=value";
        "https://localhost:8080/mcp/sse/?param=value";
        "https://localhost:8000/mcp/sse?x=1&y=2";
      ]
    in

    List.iter sse_urls ~f:(fun url ->
        let transport = Transports.infer_transport url in
        check bool "is SSE transport" true
          (match transport with
          | Transports.SSE_transport _ -> true
          | _ -> false));
    return ()

  let test_http_transport_inference switch () =
    let http_urls =
      [
        "http://example.com/api";
        "https://localhost:8080/mcp/";
        "http://example.com/asset/image.jpg";
        "https://localhost:8080/sservice/endpoint";
        "https://example.com/assets/file";
      ]
    in

    List.iter http_urls ~f:(fun url ->
        let transport = Transports.infer_transport url in
        check bool "is HTTP transport" true
          (match transport with
          | Transports.Streamable_http_transport _ -> true
          | _ -> false));
    return ()

  let test_config_transport_inference switch () =
    let config =
      `Assoc
        [
          ( "mcpServers",
            `Assoc
              [
                ( "test_server",
                  `Assoc
                    [
                      ("url", `String "http://localhost:8000/sse/");
                      ( "headers",
                        `Assoc [ ("Authorization", `String "Bearer 123") ] );
                    ] );
              ] );
        ]
    in

    let transport = Transports.infer_transport config in
    check bool "is config transport" true
      (match transport with
      | Transports.MCP_config_transport _ -> true
      | _ -> false);
    return ()

  let test_local_config_transport_inference switch () =
    let config =
      `Assoc
        [
          ( "mcpServers",
            `Assoc
              [
                ( "test_server",
                  `Assoc
                    [
                      ("command", `String "echo");
                      ("args", `List [ `String "hello" ]);
                    ] );
              ] );
        ]
    in

    let transport = Transports.infer_transport config in
    check bool "is config transport" true
      (match transport with
      | Transports.MCP_config_transport _ -> true
      | _ -> false);
    return ()

  let test_empty_config_transport_inference switch () =
    let config = `Assoc [ ("mcpServers", `Assoc []) ] in

    check_raises "empty config raises"
      (Failure "No MCP servers defined in the config") (fun () ->
        ignore (Transports.infer_transport config));
    return ()

  let test_composite_config_transport_inference switch () =
    let config =
      `Assoc
        [
          ( "mcpServers",
            `Assoc
              [
                ( "local",
                  `Assoc
                    [
                      ("command", `String "echo");
                      ("args", `List [ `String "hello" ]);
                    ] );
                ( "remote",
                  `Assoc
                    [
                      ("url", `String "http://localhost:8000/sse/");
                      ( "headers",
                        `Assoc [ ("Authorization", `String "Bearer 123") ] );
                    ] );
              ] );
        ]
    in

    let transport = Transports.infer_transport config in
    check bool "is config transport" true
      (match transport with
      | Transports.MCP_config_transport _ -> true
      | _ -> false);
    return ()

  let test_fastmcp_transport_inference switch () =
    let server = Ox_fast_mcp_server.Server.create ~name:"TestServer" () in
    let transport = Transports.infer_transport server in
    check bool "is FastMCP transport" true
      (match transport with
      | Transports.FastMCP_transport _ -> true
      | _ -> false);
    return ()

  (* Test FastMCP v1 server compatibility *)
  let test_fastmcp_v1_server_compatibility switch () =
    let server = Ox_fast_mcp_server.Server.create_v1 ~name:"TestServer" () in
    let transport = Transports.infer_transport server in
    check bool "is FastMCP transport" true
      (match transport with
      | Transports.FastMCP_transport _ -> true
      | _ -> false);
    return ()
end

(* Test auth *)
module Auth_tests = struct
  let test_default_auth_is_none switch () =
    let client =
      Client.create
        (Transports.Streamable_http_transport "http://localhost:8000")
    in
    check bool "auth is none" true
      (match client.transport with
      | Transports.Streamable_http_transport t -> Option.is_none t.auth
      | _ -> false);
    return ()

  let test_stdio_doesnt_support_auth switch () =
    check_raises "stdio auth raises"
      (Failure "This transport does not support auth") (fun () ->
        ignore
          (Client.create ~auth:"oauth"
             (Transports.Stdio_transport
                { command = "echo"; args = [ "hello" ] })));
    return ()

  let test_oauth_literal_sets_up_oauth_shttp switch () =
    let client =
      Client.create ~auth:"oauth"
        (Transports.Streamable_http_transport "http://localhost:8000")
    in
    check bool "is oauth auth" true
      (match client.transport with
      | Transports.Streamable_http_transport t -> (
        match t.auth with
        | Some (Auth.OAuth_client_provider _) -> true
        | _ -> false)
      | _ -> false);
    return ()

  let test_oauth_literal_sets_up_oauth_sse switch () =
    let client =
      Client.create ~auth:"oauth"
        (Transports.SSE_transport "http://localhost:8000")
    in
    check bool "is oauth auth" true
      (match client.transport with
      | Transports.SSE_transport t -> (
        match t.auth with
        | Some (Auth.OAuth_client_provider _) -> true
        | _ -> false)
      | _ -> false);
    return ()

  let test_auth_string_sets_up_bearer_auth_shttp switch () =
    let client =
      Client.create ~auth:"test_token"
        (Transports.Streamable_http_transport "http://localhost:8000")
    in
    check bool "is bearer auth" true
      (match client.transport with
      | Transports.Streamable_http_transport t -> (
        match t.auth with
        | Some (Auth.Bearer_auth b) -> b.token = "test_token"
        | _ -> false)
      | _ -> false);
    return ()

  let test_auth_string_sets_up_bearer_auth_sse switch () =
    let client =
      Client.create ~auth:"test_token"
        (Transports.SSE_transport "http://localhost:8000")
    in
    check bool "is bearer auth" true
      (match client.transport with
      | Transports.SSE_transport t -> (
        match t.auth with
        | Some (Auth.Bearer_auth b) -> b.token = "test_token"
        | _ -> false)
      | _ -> false);
    return ()
end

let () =
  run "Client Transport Tests"
    [
      ( "transport inference",
        [
          test_case "SSE transport inference" `Quick
            Transport_tests.test_sse_transport_inference;
          test_case "HTTP transport inference" `Quick
            Transport_tests.test_http_transport_inference;
          test_case "config transport inference" `Quick
            Transport_tests.test_config_transport_inference;
          test_case "local config transport inference" `Quick
            Transport_tests.test_local_config_transport_inference;
          test_case "empty config transport inference" `Quick
            Transport_tests.test_empty_config_transport_inference;
          test_case "composite config transport inference" `Quick
            Transport_tests.test_composite_config_transport_inference;
          test_case "FastMCP transport inference" `Quick
            Transport_tests.test_fastmcp_transport_inference;
          test_case "FastMCP v1 server compatibility" `Quick
            Transport_tests.test_fastmcp_v1_server_compatibility;
        ] );
      ( "auth",
        [
          test_case "default auth is none" `Quick
            Auth_tests.test_default_auth_is_none;
          test_case "stdio doesn't support auth" `Quick
            Auth_tests.test_stdio_doesnt_support_auth;
          test_case "oauth literal sets up oauth shttp" `Quick
            Auth_tests.test_oauth_literal_sets_up_oauth_shttp;
          test_case "oauth literal sets up oauth sse" `Quick
            Auth_tests.test_oauth_literal_sets_up_oauth_sse;
          test_case "auth string sets up bearer auth shttp" `Quick
            Auth_tests.test_auth_string_sets_up_bearer_auth_shttp;
          test_case "auth string sets up bearer auth sse" `Quick
            Auth_tests.test_auth_string_sets_up_bearer_auth_sse;
        ] );
    ]
