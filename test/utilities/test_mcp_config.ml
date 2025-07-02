open Alcotest
open Utilities.Mcp_config

let test_parse_single_stdio_config () =
  let config_json = `Assoc [
    ("mcpServers", `Assoc [
      ("test_server", `Assoc [
        ("command", `String "echo");
        ("args", `List [`String "hello"]);
      ]);
    ]);
  ] in
  let config = config_of_json config_json in
  match List.assoc_opt "test_server" config.mcp_servers with
  | Some (StdioServer server) ->
      check string "command" "echo" server.command;
      check (list string) "args" ["hello"] server.args;
      check (list (pair string string)) "env" [] server.env;
      check (option string) "cwd" None server.cwd;
      check bool "transport is stdio" true (match server.transport with `Stdio -> true)
  | _ -> Alcotest.fail "Expected StdioServer"

let test_parse_single_remote_config () =
  let config_json = `Assoc [
    ("mcpServers", `Assoc [
      ("test_server", `Assoc [
        ("url", `String "http://localhost:8000");
      ]);
    ]);
  ] in
  let config = config_of_json config_json in
  match List.assoc_opt "test_server" config.mcp_servers with
  | Some (RemoteServer server) ->
      check string "url" "http://localhost:8000" server.url;
      check (list (pair string string)) "headers" [] server.headers;
      check bool "transport is none" true (server.transport = None);
      check bool "auth is none" true (server.auth = None)
  | _ -> Alcotest.fail "Expected RemoteServer"

let test_parse_remote_config_with_transport () =
  let config_json = `Assoc [
    ("mcpServers", `Assoc [
      ("test_server", `Assoc [
        ("url", `String "http://localhost:8000");
        ("transport", `String "sse");
      ]);
    ]);
  ] in
  let config = config_of_json config_json in
  match List.assoc_opt "test_server" config.mcp_servers with
  | Some (RemoteServer server) ->
      check string "url" "http://localhost:8000" server.url;
      check bool "transport is sse" true (server.transport = Some `SSE)
  | _ -> Alcotest.fail "Expected RemoteServer"

let test_parse_remote_config_with_url_inference () =
  let config_json = `Assoc [
    ("mcpServers", `Assoc [
      ("test_server", `Assoc [
        ("url", `String "http://localhost:8000/sse/");
      ]);
    ]);
  ] in
  let config = config_of_json config_json in
  match List.assoc_opt "test_server" config.mcp_servers with
  | Some (RemoteServer server) ->
      check string "url" "http://localhost:8000/sse/" server.url;
      check bool "transport inferred as sse" true (infer_transport_type_from_url server.url = SSE)
  | _ -> Alcotest.fail "Expected RemoteServer"

let test_parse_multiple_servers () =
  let config_json = `Assoc [
    ("mcpServers", `Assoc [
      ("test_server", `Assoc [
        ("url", `String "http://localhost:8000/sse/");
      ]);
      ("test_server_2", `Assoc [
        ("command", `String "echo");
        ("args", `List [`String "hello"]);
        ("env", `Assoc [("TEST", `String "test")]);
      ]);
    ]);
  ] in
  let config = config_of_json config_json in
  check int "server count" 2 (List.length config.mcp_servers);
  
  match List.assoc_opt "test_server" config.mcp_servers with
  | Some (RemoteServer server) ->
      check string "url" "http://localhost:8000/sse/" server.url;
      check bool "transport inferred as sse" true (infer_transport_type_from_url server.url = SSE)
  | _ -> Alcotest.fail "Expected RemoteServer for test_server";
  
  match List.assoc_opt "test_server_2" config.mcp_servers with
  | Some (StdioServer server) ->
      check string "command" "echo" server.command;
      check (list string) "args" ["hello"] server.args;
      check (list (pair string string)) "env" [("TEST", "test")] server.env
  | _ -> Alcotest.fail "Expected StdioServer for test_server_2"

let test_remote_config_with_auth_token () =
  let config_json = `Assoc [
    ("mcpServers", `Assoc [
      ("test_server", `Assoc [
        ("url", `String "http://localhost:8000");
        ("auth", `String "test_token");
      ]);
    ]);
  ] in
  let config = config_of_json config_json in
  match List.assoc_opt "test_server" config.mcp_servers with
  | Some (RemoteServer server) ->
      check bool "has bearer auth" true (server.auth = Some (`Bearer "test_token"))
  | _ -> Alcotest.fail "Expected RemoteServer"

let test_remote_config_with_oauth_literal () =
  let config_json = `Assoc [
    ("mcpServers", `Assoc [
      ("test_server", `Assoc [
        ("url", `String "http://localhost:8000");
        ("auth", `String "oauth");
      ]);
    ]);
  ] in
  let config = config_of_json config_json in
  match List.assoc_opt "test_server" config.mcp_servers with
  | Some (RemoteServer server) ->
      check bool "has oauth auth" true (server.auth = Some `OAuth)
  | _ -> Alcotest.fail "Expected RemoteServer"

let test_json_roundtrip () =
  let original_json = `Assoc [
    ("mcpServers", `Assoc [
      ("test_server", `Assoc [
        ("url", `String "http://localhost:8000");
        ("transport", `String "sse");
        ("auth", `String "test_token");
      ]);
      ("test_server_2", `Assoc [
        ("command", `String "echo");
        ("args", `List [`String "hello"]);
        ("env", `Assoc [("TEST", `String "test")]);
      ]);
    ]);
  ] in
  let config = config_of_json original_json in
  let roundtrip_json = json_of_config config in
  Printf.printf "Original JSON: %s\n" (Yojson.Safe.to_string original_json);
  Printf.printf "Roundtrip JSON: %s\n" (Yojson.Safe.to_string roundtrip_json);
  let rec sort_json = function
    | `Assoc fields -> `Assoc (List.sort compare (List.map (fun (k, v) -> (k, sort_json v)) fields))
    | `List items -> `List (List.map sort_json items)
    | other -> other
  in
  let sorted_original = sort_json original_json in
  let sorted_roundtrip = sort_json roundtrip_json in
  check bool "json roundtrip" true (Yojson.Safe.equal sorted_original sorted_roundtrip)

let () =
  run "MCP Config Tests" [
    "config_parsing", [
      test_case "parse single stdio config" `Quick test_parse_single_stdio_config;
      test_case "parse single remote config" `Quick test_parse_single_remote_config;
      test_case "parse remote config with transport" `Quick test_parse_remote_config_with_transport;
      test_case "parse remote config with URL inference" `Quick test_parse_remote_config_with_url_inference;
      test_case "parse multiple servers" `Quick test_parse_multiple_servers;
    ];
    "authentication", [
      test_case "remote config with auth token" `Quick test_remote_config_with_auth_token;
      test_case "remote config with OAuth literal" `Quick test_remote_config_with_oauth_literal;
    ];
    "serialization", [
      test_case "json roundtrip" `Quick test_json_roundtrip;
    ];
  ] 