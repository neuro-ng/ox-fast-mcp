open Alcotest
open Ox_fast_mcp.Mcp_types

let test_jsonrpc_request_serialization () =
  let req =
    {
      jsonrpc = "2.0";
      id = Some (`String "test-id");
      method_ = "tools/list";
      params = None;
    }
  in
  let json = jsonrpc_request_to_yojson req in
  let parsed = jsonrpc_request_of_yojson json in
  match parsed with
  | Ok parsed_req ->
    check string "jsonrpc version" "2.0" parsed_req.jsonrpc;
    check string "method" "tools/list" parsed_req.method_;
    check (option string) "id" (Some "test-id")
      (match parsed_req.id with
      | Some (`String s) -> Some s
      | _ -> None)
  | Error err -> fail ("Failed to parse JSON: " ^ err)

let test_tool_def_creation () =
  let tool =
    {
      name = "add_numbers";
      description = "Add two numbers together";
      input_schema =
        Some
          (`Assoc
            [
              ("type", `String "object");
              ( "properties",
                `Assoc
                  [
                    ("a", `Assoc [ ("type", `String "number") ]);
                    ("b", `Assoc [ ("type", `String "number") ]);
                  ] );
            ]);
    }
  in
  check string "tool name" "add_numbers" tool.name;
  check string "tool description" "Add two numbers together" tool.description;
  match tool.input_schema with
  | Some schema ->
    let json_str = Yojson.Safe.pretty_to_string schema in
    check bool "has schema" true (String.contains json_str '{')
  | None -> fail "Expected input schema"

let test_resource_def_creation () =
  let resource =
    {
      uri = "config://settings";
      name = Some "Application Settings";
      description = Some "Configuration settings for the application";
      mime_type = Some "application/json";
    }
  in
  check string "resource uri" "config://settings" resource.uri;
  check (option string) "resource name" (Some "Application Settings")
    resource.name;
  check (option string) "mime type" (Some "application/json") resource.mime_type

let test_prompt_def_creation () =
  let prompt =
    {
      name = "summarize";
      description = "Generate a summary of the given text";
      arguments =
        Some
          [
            {
              name = "text";
              type_ = "string";
              description = Some "Text to summarize";
              required = true;
            };
          ];
    }
  in
  check string "prompt name" "summarize" prompt.name;
  check string "prompt description" "Generate a summary of the given text"
    prompt.description;
  match prompt.arguments with
  | Some args ->
    check int "argument count" 1 (List.length args);
    let arg = List.hd args in
    check string "argument name" "text" arg.name;
    check string "argument type" "string" arg.type_
  | None -> fail "Expected arguments"

let test_content_type_variants () =
  let text_content = Text "Hello, world!" in
  let image_content = Image { data = "base64data"; mime_type = "image/png" } in
  let resource_content =
    Resource
      {
        uri = "file://test.txt";
        text = Some "content";
        mime_type = Some "text/plain";
      }
  in

  match text_content with
  | Text s -> check string "text content" "Hello, world!" s
  | _ -> (
    fail "Expected Text variant";

    match image_content with
    | Image { data; mime_type } ->
      check string "image data" "base64data" data;
      check string "image mime type" "image/png" mime_type
    | _ -> (
      fail "Expected Image variant";

      match resource_content with
      | Resource { uri; text; mime_type } ->
        check string "resource uri" "file://test.txt" uri;
        check (option string) "resource text" (Some "content") text;
        check (option string) "resource mime type" (Some "text/plain") mime_type
      | _ -> fail "Expected Resource variant"))

let test_message_creation () =
  let msg = { role = "user"; content = Text "What is the weather like?" } in
  check string "message role" "user" msg.role;
  match msg.content with
  | Text s -> check string "message content" "What is the weather like?" s
  | _ -> fail "Expected Text content"

let test_execution_context () =
  let ctx =
    {
      request_id = Some "req-123";
      client_id = Some "client-456";
      session_data = Hashtbl.create 10;
    }
  in
  Hashtbl.add ctx.session_data "key1" (`String "value1");
  check (option string) "request id" (Some "req-123") ctx.request_id;
  check (option string) "client id" (Some "client-456") ctx.client_id;
  check bool "session data exists" true (Hashtbl.mem ctx.session_data "key1")

let test_transport_types () =
  let stdio_transport = Stdio in
  let http_transport =
    Http { host = "localhost"; port = 8080; path = "/mcp" }
  in
  let sse_transport = Sse { host = "0.0.0.0"; port = 9000 } in

  match stdio_transport with
  | Stdio -> check bool "stdio transport" true true
  | _ -> (
    fail "Expected Stdio transport";

    match http_transport with
    | Http { host; port; path } ->
      check string "http host" "localhost" host;
      check int "http port" 8080 port;
      check string "http path" "/mcp" path
    | _ -> (
      fail "Expected Http transport";

      match sse_transport with
      | Sse { host; port } ->
        check string "sse host" "0.0.0.0" host;
        check int "sse port" 9000 port
      | _ -> fail "Expected Sse transport"))

let test_server_capabilities () =
  let capabilities =
    {
      logging = Some (`Assoc [ ("level", `String "info") ]);
      prompts = Some (`Assoc [ ("listChanged", `Bool true) ]);
      resources = Some (`Assoc [ ("subscribe", `Bool true) ]);
      tools = Some (`Assoc [ ("listChanged", `Bool true) ]);
    }
  in
  check bool "has logging capability" true (Option.is_some capabilities.logging);
  check bool "has prompts capability" true (Option.is_some capabilities.prompts);
  check bool "has resources capability" true
    (Option.is_some capabilities.resources);
  check bool "has tools capability" true (Option.is_some capabilities.tools)

let () =
  run "MCP Types Tests"
    [
      ( "JSON-RPC",
        [
          test_case "Request serialization" `Quick
            test_jsonrpc_request_serialization;
        ] );
      ( "Tool definitions",
        [ test_case "Tool creation" `Quick test_tool_def_creation ] );
      ( "Resource definitions",
        [ test_case "Resource creation" `Quick test_resource_def_creation ] );
      ( "Prompt definitions",
        [ test_case "Prompt creation" `Quick test_prompt_def_creation ] );
      ( "Content types",
        [ test_case "Content type variants" `Quick test_content_type_variants ]
      );
      ("Messages", [ test_case "Message creation" `Quick test_message_creation ]);
      ( "Execution context",
        [ test_case "Context creation" `Quick test_execution_context ] );
      ( "Transport types",
        [ test_case "Transport variants" `Quick test_transport_types ] );
      ( "Server capabilities",
        [ test_case "Capabilities creation" `Quick test_server_capabilities ] );
    ]
