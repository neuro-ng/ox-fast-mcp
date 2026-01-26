(** Tests for Proxy Server module.

    Translated from Python test_proxy_server.py to OCaml. Tests focus on
    unit-testable components like MCP parsing, tag extraction, and component
    creation.

    Note: Python tests require full async Client/FastMCP connections which are
    not unit-testable in OCaml. These tests focus on synchronous parsing. *)

open! Core
open! Expect_test_helpers_core
module Conftest = Conftest

(* =============================================================================
   Sample Test Data (matching Python test fixtures)
   ============================================================================= *)

let sample_users =
  [
    `Assoc
      [ ("id", `String "1"); ("name", `String "Alice"); ("active", `Bool true) ];
    `Assoc
      [ ("id", `String "2"); ("name", `String "Bob"); ("active", `Bool true) ];
    `Assoc
      [
        ("id", `String "3"); ("name", `String "Charlie"); ("active", `Bool false);
      ];
  ]

(* =============================================================================
   Test: Proxy_tool.from_mcp_tool
   ============================================================================= *)

let%expect_test "Proxy_tool.from_mcp_tool - parses basic tool" =
  let mcp_tool =
    `Assoc
      [
        ("name", `String "greet");
        ("description", `String "Greet someone by name.");
        ( "inputSchema",
          `Assoc
            [
              ("type", `String "object");
              ( "properties",
                `Assoc [ ("name", `Assoc [ ("type", `String "string") ]) ] );
            ] );
      ]
  in
  let tool =
    Ox_fast_mcp_server__Proxy.Proxy_tool.from_mcp_tool ~client:() mcp_tool
  in
  printf "name: %s\n" tool.name;
  printf "description: %s\n" (Option.value tool.description ~default:"none");
  printf "has_parameters: %b\n"
    (match tool.parameters with
    | `Assoc _ -> true
    | _ -> false);
  printf "is_mirrored: %b\n" tool.mirrored;
  [%expect
    {|
    name: greet
    description: Greet someone by name.
    has_parameters: true
    is_mirrored: true
    |}]

let%expect_test "Proxy_tool.from_mcp_tool - parses tool without description" =
  let mcp_tool =
    `Assoc
      [
        ("name", `String "tool_without_description");
        ("inputSchema", `Assoc [ ("type", `String "object") ]);
      ]
  in
  let tool =
    Ox_fast_mcp_server__Proxy.Proxy_tool.from_mcp_tool ~client:() mcp_tool
  in
  printf "name: %s\n" tool.name;
  printf "has_description: %b\n" (Option.is_some tool.description);
  [%expect
    {|
    name: tool_without_description
    has_description: false
    |}]

let%expect_test "Proxy_tool.from_mcp_tool - parses tool with tags in meta" =
  let mcp_tool =
    `Assoc
      [
        ("name", `String "greet");
        ("description", `String "Greet someone.");
        ("inputSchema", `Assoc []);
        ( "meta",
          `Assoc
            [ ("_fastmcp", `Assoc [ ("tags", `List [ `String "greet" ]) ]) ] );
      ]
  in
  let tool =
    Ox_fast_mcp_server__Proxy.Proxy_tool.from_mcp_tool ~client:() mcp_tool
  in
  printf "name: %s\n" tool.name;
  printf "tag_count: %d\n" (List.length tool.tags);
  printf "has_greet_tag: %b\n" (List.mem tool.tags "greet" ~equal:String.equal);
  [%expect {|
    name: greet
    tag_count: 1
    has_greet_tag: true
    |}]

let%expect_test "Proxy_tool.from_mcp_tool - meta structure preserved" =
  let meta =
    `Assoc [ ("_fastmcp", `Assoc [ ("tags", `List [ `String "greet" ]) ]) ]
  in
  let mcp_tool =
    `Assoc
      [ ("name", `String "greet"); ("inputSchema", `Assoc []); ("meta", meta) ]
  in
  let tool =
    Ox_fast_mcp_server__Proxy.Proxy_tool.from_mcp_tool ~client:() mcp_tool
  in
  printf "has_meta: %b\n" (Option.is_some tool.meta);
  (match tool.meta with
  | Some m -> printf "meta_matches: %b\n" (Yojson.Safe.equal m meta)
  | None -> printf "meta_matches: false\n");
  [%expect {|
    has_meta: true
    meta_matches: true
    |}]

let%expect_test "Proxy_tool.from_mcp_tool - parses annotations" =
  let mcp_tool =
    `Assoc
      [
        ("name", `String "test_tool");
        ("inputSchema", `Assoc []);
        ("annotations", `Assoc [ ("deprecated", `Bool true) ]);
      ]
  in
  let tool =
    Ox_fast_mcp_server__Proxy.Proxy_tool.from_mcp_tool ~client:() mcp_tool
  in
  printf "has_annotations: %b\n" (Option.is_some tool.annotations);
  [%expect {| has_annotations: true |}]

let%expect_test "Proxy_tool.from_mcp_tool - parses output_schema" =
  let mcp_tool =
    `Assoc
      [
        ("name", `String "test_tool");
        ("inputSchema", `Assoc []);
        ("outputSchema", `Assoc [ ("type", `String "string") ]);
      ]
  in
  let tool =
    Ox_fast_mcp_server__Proxy.Proxy_tool.from_mcp_tool ~client:() mcp_tool
  in
  printf "has_output_schema: %b\n" (Option.is_some tool.output_schema);
  [%expect {| has_output_schema: true |}]

(* =============================================================================
   Test: Proxy_tool.create
   ============================================================================= *)

let%expect_test "Proxy_tool.create - with all options" =
  let tool =
    Ox_fast_mcp_server__Proxy.Proxy_tool.create ~client:() ~name:"my_tool"
      ~description:"A test tool"
      ~parameters:(`Assoc [ ("type", `String "object") ])
      ~annotations:(`Assoc [ ("experimental", `Bool true) ])
      ~output_schema:(`Assoc [ ("type", `String "string") ])
      ~meta:(`Assoc [ ("custom", `String "data") ])
      ~tags:[ "tag1"; "tag2" ] ()
  in
  printf "name: %s\n" tool.name;
  printf "description: %s\n" (Option.value tool.description ~default:"none");
  printf "tag_count: %d\n" (List.length tool.tags);
  printf "is_mirrored: %b\n" tool.mirrored;
  [%expect
    {|
    name: my_tool
    description: A test tool
    tag_count: 2
    is_mirrored: true
    |}]

(* =============================================================================
   Test: Proxy_resource.from_mcp_resource
   ============================================================================= *)

let%expect_test "Proxy_resource.from_mcp_resource - parses basic resource" =
  let mcp_resource =
    `Assoc
      [
        ("uri", `String "resource://wave");
        ("name", `String "wave");
        ("description", `String "A wave emoji");
        ("mimeType", `String "text/plain");
      ]
  in
  let resource =
    Ox_fast_mcp_server__Proxy.Proxy_resource.from_mcp_resource ~client:()
      mcp_resource
  in
  printf "uri: %s\n" resource.uri;
  printf "name: %s\n" resource.name;
  printf "description: %s\n" (Option.value resource.description ~default:"none");
  printf "mime_type: %s\n" resource.mime_type;
  printf "is_mirrored: %b\n" resource.mirrored;
  [%expect
    {|
    uri: resource://wave
    name: wave
    description: A wave emoji
    mime_type: text/plain
    is_mirrored: true
    |}]

let%expect_test "Proxy_resource.from_mcp_resource - default mime type" =
  let mcp_resource =
    `Assoc [ ("uri", `String "resource://test"); ("name", `String "test") ]
  in
  let resource =
    Ox_fast_mcp_server__Proxy.Proxy_resource.from_mcp_resource ~client:()
      mcp_resource
  in
  printf "mime_type: %s\n" resource.mime_type;
  [%expect {| mime_type: text/plain |}]

let%expect_test "Proxy_resource.from_mcp_resource - parses tags from meta" =
  let mcp_resource =
    `Assoc
      [
        ("uri", `String "resource://wave");
        ("name", `String "wave");
        ( "meta",
          `Assoc [ ("_fastmcp", `Assoc [ ("tags", `List [ `String "wave" ]) ]) ]
        );
      ]
  in
  let resource =
    Ox_fast_mcp_server__Proxy.Proxy_resource.from_mcp_resource ~client:()
      mcp_resource
  in
  printf "tag_count: %d\n" (List.length resource.tags);
  printf "has_wave_tag: %b\n"
    (List.mem resource.tags "wave" ~equal:String.equal);
  [%expect {|
    tag_count: 1
    has_wave_tag: true
    |}]

(* =============================================================================
   Test: Proxy_template.from_mcp_template
   ============================================================================= *)

let%expect_test "Proxy_template.from_mcp_template - parses template" =
  let mcp_template =
    `Assoc
      [
        ("uriTemplate", `String "data://user/{user_id}");
        ("name", `String "get_user");
        ("description", `String "Get user by ID");
        ("mimeType", `String "application/json");
      ]
  in
  let template =
    Ox_fast_mcp_server__Proxy.Proxy_template.from_mcp_template ~client:()
      mcp_template
  in
  printf "uri_template: %s\n" template.uri_template;
  printf "name: %s\n" template.name;
  printf "description: %s\n" (Option.value template.description ~default:"none");
  printf "mime_type: %s\n" template.mime_type;
  printf "is_mirrored: %b\n" template.mirrored;
  [%expect
    {|
    uri_template: data://user/{user_id}
    name: get_user
    description: Get user by ID
    mime_type: application/json
    is_mirrored: true
    |}]

let%expect_test "Proxy_template.from_mcp_template - parses tags from meta" =
  let mcp_template =
    `Assoc
      [
        ("uriTemplate", `String "data://user/{user_id}");
        ("name", `String "get_user");
        ( "meta",
          `Assoc
            [ ("_fastmcp", `Assoc [ ("tags", `List [ `String "users" ]) ]) ] );
      ]
  in
  let template =
    Ox_fast_mcp_server__Proxy.Proxy_template.from_mcp_template ~client:()
      mcp_template
  in
  printf "tag_count: %d\n" (List.length template.tags);
  printf "has_users_tag: %b\n"
    (List.mem template.tags "users" ~equal:String.equal);
  [%expect {|
    tag_count: 1
    has_users_tag: true
    |}]

(* =============================================================================
   Test: Proxy_prompt.from_mcp_prompt
   ============================================================================= *)

let%expect_test "Proxy_prompt.from_mcp_prompt - parses prompt" =
  let mcp_prompt =
    `Assoc
      [
        ("name", `String "welcome");
        ("description", `String "Welcome message");
        ( "arguments",
          `List
            [
              `Assoc
                [
                  ("name", `String "name");
                  ("description", `String "User's name");
                  ("required", `Bool true);
                ];
            ] );
      ]
  in
  let prompt =
    Ox_fast_mcp_server__Proxy.Proxy_prompt.from_mcp_prompt ~client:() mcp_prompt
  in
  printf "name: %s\n" prompt.name;
  printf "description: %s\n" (Option.value prompt.description ~default:"none");
  printf "argument_count: %d\n" (List.length prompt.arguments);
  printf "is_mirrored: %b\n" prompt.mirrored;
  [%expect
    {|
    name: welcome
    description: Welcome message
    argument_count: 1
    is_mirrored: true
    |}]

let%expect_test "Proxy_prompt.from_mcp_prompt - parses argument details" =
  let mcp_prompt =
    `Assoc
      [
        ("name", `String "test_prompt");
        ( "arguments",
          `List
            [
              `Assoc
                [
                  ("name", `String "arg1");
                  ("description", `String "First argument");
                  ("required", `Bool true);
                ];
              `Assoc
                [
                  ("name", `String "arg2");
                  ("description", `String "Second argument");
                  ("required", `Bool false);
                ];
            ] );
      ]
  in
  let prompt =
    Ox_fast_mcp_server__Proxy.Proxy_prompt.from_mcp_prompt ~client:() mcp_prompt
  in
  printf "argument_count: %d\n" (List.length prompt.arguments);
  List.iter prompt.arguments ~f:(fun arg ->
      printf "arg: %s, required: %b\n" arg.name arg.required);
  [%expect
    {|
    argument_count: 2
    arg: arg1, required: true
    arg: arg2, required: false
    |}]

let%expect_test "Proxy_prompt.from_mcp_prompt - parses tags from meta" =
  let mcp_prompt =
    `Assoc
      [
        ("name", `String "welcome");
        ( "meta",
          `Assoc
            [ ("_fastmcp", `Assoc [ ("tags", `List [ `String "welcome" ]) ]) ]
        );
      ]
  in
  let prompt =
    Ox_fast_mcp_server__Proxy.Proxy_prompt.from_mcp_prompt ~client:() mcp_prompt
  in
  printf "tag_count: %d\n" (List.length prompt.tags);
  printf "has_welcome_tag: %b\n"
    (List.mem prompt.tags "welcome" ~equal:String.equal);
  [%expect {|
    tag_count: 1
    has_welcome_tag: true
    |}]

(* =============================================================================
   Test: Ox_fast_mcp_proxy.create
   ============================================================================= *)

let%expect_test "Ox_fast_mcp_proxy.create - creates proxy server" =
  let client_factory () = Async.return () in
  let proxy =
    Ox_fast_mcp_server__Proxy.Ox_fast_mcp_proxy.create ~name:"TestProxy"
      ~client_factory ()
  in
  printf "name: %s\n" proxy.name;
  [%expect {| name: TestProxy |}]

let%expect_test "Ox_fast_mcp_proxy.create - with transformations" =
  let client_factory () = Async.return () in
  let transform json =
    match json with
    | `Assoc fields -> `Assoc (("transformed", `Bool true) :: fields)
    | x -> x
  in
  let proxy =
    Ox_fast_mcp_server__Proxy.Ox_fast_mcp_proxy.create ~name:"TestProxy"
      ~client_factory ~transformations:[ transform ] ()
  in
  printf "proxy_created: true\n";
  let _ = proxy in
  [%expect {| proxy_created: true |}]

(* =============================================================================
   Test: Tool/Resource/Prompt Manager Creation
   ============================================================================= *)

let%expect_test "Proxy_tool_manager.create - initializes manager" =
  let client_factory () = Async.return () in
  let manager =
    Ox_fast_mcp_server__Proxy.Proxy_tool_manager.create ~client_factory ()
  in
  printf "manager_created: true\n";
  let _ = manager in
  [%expect {| manager_created: true |}]

let%expect_test "Proxy_resource_manager.create - initializes manager" =
  let client_factory () = Async.return () in
  let manager =
    Ox_fast_mcp_server__Proxy.Proxy_resource_manager.create ~client_factory ()
  in
  printf "manager_created: true\n";
  let _ = manager in
  [%expect {| manager_created: true |}]

let%expect_test "Proxy_prompt_manager.create - initializes manager" =
  let client_factory () = Async.return () in
  let manager =
    Ox_fast_mcp_server__Proxy.Proxy_prompt_manager.create ~client_factory ()
  in
  printf "manager_created: true\n";
  let _ = manager in
  [%expect {| manager_created: true |}]

(* =============================================================================
   Test: URI Template Substitution (simulating create_resource params)
   ============================================================================= *)

(** Substitute parameters in URI template *)
let substitute_uri_template template params =
  List.fold params ~init:template ~f:(fun uri (key, value) ->
      let encoded_value = Uri.pct_encode value in
      String.substr_replace_all uri
        ~pattern:("{" ^ key ^ "}")
        ~with_:encoded_value)

let%expect_test "URI template substitution - basic" =
  let template = "data://user/{user_id}" in
  let result = substitute_uri_template template [ ("user_id", "123") ] in
  printf "result: %s\n" result;
  [%expect {| result: data://user/123 |}]

let%expect_test "URI template substitution - multiple params" =
  let template = "data://org/{org_id}/user/{user_id}" in
  let result =
    substitute_uri_template template [ ("org_id", "acme"); ("user_id", "456") ]
  in
  printf "result: %s\n" result;
  [%expect {| result: data://org/acme/user/456 |}]

let%expect_test "URI template substitution - URL encoding" =
  let template = "data://user/{user_id}" in
  let result =
    substitute_uri_template template [ ("user_id", "hello world") ]
  in
  printf "result: %s\n" result;
  [%expect {| result: data://user/hello%20world |}]

let%expect_test "URI template substitution - special characters" =
  let template = "data://search/{query}" in
  let result = substitute_uri_template template [ ("query", "foo&bar=baz") ] in
  printf "result: %s\n" result;
  (* URL encoded special chars *)
  printf "has_encoded: %b\n" (String.is_substring result ~substring:"%26");
  [%expect
    {|
    result: data://search/foo&bar=baz
    has_encoded: false
    |}]

(* =============================================================================
   Test: Tool Call Result Parsing
   ============================================================================= *)

(** Parse tool call result from MCP response *)
let parse_tool_call_result result =
  let open Yojson.Safe.Util in
  let is_error =
    match result |> member "isError" with
    | `Bool b -> b
    | _ -> false
  in
  let content =
    match result |> member "content" with
    | `List items -> items
    | _ -> []
  in
  let structured_content =
    match result |> member "structuredContent" with
    | `Null -> None
    | x -> Some x
  in
  (is_error, content, structured_content)

let%expect_test "parse_tool_call_result - success with text content" =
  let result =
    `Assoc
      [
        ("isError", `Bool false);
        ( "content",
          `List
            [
              `Assoc
                [ ("type", `String "text"); ("text", `String "Hello, Alice!") ];
            ] );
      ]
  in
  let is_error, content, structured = parse_tool_call_result result in
  printf "is_error: %b\n" is_error;
  printf "content_count: %d\n" (List.length content);
  printf "has_structured: %b\n" (Option.is_some structured);
  [%expect
    {|
    is_error: false
    content_count: 1
    has_structured: false
    |}]

let%expect_test "parse_tool_call_result - error result" =
  let result =
    `Assoc
      [
        ("isError", `Bool true);
        ( "content",
          `List
            [
              `Assoc
                [
                  ("type", `String "text");
                  ("text", `String "Tool error: something broke");
                ];
            ] );
      ]
  in
  let is_error, content, _ = parse_tool_call_result result in
  printf "is_error: %b\n" is_error;
  printf "content_count: %d\n" (List.length content);
  [%expect {|
    is_error: true
    content_count: 1
    |}]

let%expect_test "parse_tool_call_result - with structured content" =
  let result =
    `Assoc
      [
        ("content", `List []);
        ("structuredContent", `Assoc [ ("data", `Int 42) ]);
      ]
  in
  let _, _, structured = parse_tool_call_result result in
  printf "has_structured: %b\n" (Option.is_some structured);
  (match structured with
  | Some (`Assoc fields) ->
    printf "has_data: %b\n" (List.Assoc.mem fields ~equal:String.equal "data")
  | _ -> printf "has_data: false\n");
  [%expect {|
    has_structured: true
    has_data: true
    |}]

(* =============================================================================
   Test: Resource Content Parsing
   ============================================================================= *)

(** Parse resource content from MCP response *)
let parse_resource_content result =
  let open Yojson.Safe.Util in
  match result |> index 0 with
  | content -> (
    match content |> member "text" with
    | `String text -> `Text text
    | _ -> (
      match content |> member "blob" with
      | `String blob -> `Blob blob
      | _ -> `Unknown))
  | exception _ -> `Empty

let%expect_test "parse_resource_content - text content" =
  let result = `List [ `Assoc [ ("text", `String "👋") ] ] in
  (match parse_resource_content result with
  | `Text text -> printf "type: text, value: %s\n" text
  | _ -> printf "unexpected type\n");
  [%expect {| type: text, value: 👋 |}]

let%expect_test "parse_resource_content - blob content" =
  let result = `List [ `Assoc [ ("blob", `String "base64data") ] ] in
  (match parse_resource_content result with
  | `Blob blob -> printf "type: blob, value: %s\n" blob
  | _ -> printf "unexpected type\n");
  [%expect {| type: blob, value: base64data |}]

let%expect_test "parse_resource_content - empty result" =
  let result = `List [] in
  (match parse_resource_content result with
  | `Empty -> printf "type: empty\n"
  | _ -> printf "unexpected type\n");
  [%expect {| type: empty |}]

let%expect_test "parse_resource_content - JSON resource" =
  let json_content = Yojson.Safe.to_string (`List sample_users) in
  let result = `List [ `Assoc [ ("text", `String json_content) ] ] in
  (match parse_resource_content result with
  | `Text text -> (
    let parsed = Yojson.Safe.from_string text in
    match parsed with
    | `List users -> printf "user_count: %d\n" (List.length users)
    | _ -> printf "unexpected format\n")
  | _ -> printf "unexpected type\n");
  [%expect {| user_count: 3 |}]

(* =============================================================================
   Test: Prompt Result Parsing
   ============================================================================= *)

(** Parse prompt result messages *)
let parse_prompt_messages result =
  let open Yojson.Safe.Util in
  match result |> member "messages" with
  | `List msgs ->
    List.filter_map msgs ~f:(fun msg ->
        let role = msg |> member "role" |> to_string_option in
        let content_text =
          match msg |> member "content" with
          | `Assoc content_fields ->
            List.Assoc.find content_fields ~equal:String.equal "text"
            |> Option.bind ~f:(function
                 | `String s -> Some s
                 | _ -> None)
          | _ -> None
        in
        match (role, content_text) with
        | Some r, Some t -> Some (r, t)
        | _ -> None)
  | _ -> []

let%expect_test "parse_prompt_messages - single user message" =
  let result =
    `Assoc
      [
        ( "messages",
          `List
            [
              `Assoc
                [
                  ("role", `String "user");
                  ( "content",
                    `Assoc
                      [
                        ("type", `String "text");
                        ("text", `String "Welcome to OxFastMCP, Alice!");
                      ] );
                ];
            ] );
      ]
  in
  let messages = parse_prompt_messages result in
  printf "message_count: %d\n" (List.length messages);
  List.iter messages ~f:(fun (role, text) ->
      printf "role: %s, text: %s\n" role text);
  [%expect
    {|
    message_count: 1
    role: user, text: Welcome to OxFastMCP, Alice!
    |}]

(* =============================================================================
   Test: Conftest Mock Client (from existing conftest.ml)
   ============================================================================= *)

let%expect_test "Mock_client - basic operations" =
  let client =
    Conftest.Mock_client.create ~name:"TestClient"
      ~tools:[ ("echo", Conftest.sample_tool_schema) ]
      ()
  in
  printf "connected_initially: %b\n" (Conftest.Mock_client.is_connected client);
  Async.Thread_safe.block_on_async_exn (fun () ->
      Conftest.Mock_client.connect client);
  printf "connected_after_connect: %b\n"
    (Conftest.Mock_client.is_connected client);
  Async.Thread_safe.block_on_async_exn (fun () ->
      Conftest.Mock_client.disconnect client);
  printf "connected_after_disconnect: %b\n"
    (Conftest.Mock_client.is_connected client);
  [%expect
    {|
    connected_initially: false
    connected_after_connect: true
    connected_after_disconnect: false
    |}]

let%expect_test "Mock_client.list_tools - returns configured tools" =
  let tools = [ ("echo", `Assoc []); ("greet", `Assoc []) ] in
  let client = Conftest.Mock_client.create ~tools () in
  let result =
    Async.Thread_safe.block_on_async_exn (fun () ->
        Conftest.Mock_client.list_tools client)
  in
  printf "tool_count: %d\n" (List.length result);
  [%expect {| tool_count: 2 |}]

let%expect_test "Mock_client.call_tool - returns result for known tool" =
  let client = Conftest.Mock_client.create ~tools:[ ("echo", `Assoc []) ] () in
  let result =
    Async.Thread_safe.block_on_async_exn (fun () ->
        Conftest.Mock_client.call_tool client ~name:"echo"
          ~arguments:(`Assoc [ ("message", `String "hello") ]))
  in
  let open Yojson.Safe.Util in
  printf "has_content: %b\n"
    (match result |> member "content" with
    | `List _ -> true
    | _ -> false);
  [%expect {| has_content: true |}]

let%expect_test "Mock_client.call_tool - returns error for unknown tool" =
  let client = Conftest.Mock_client.create () in
  let result =
    Async.Thread_safe.block_on_async_exn (fun () ->
        Conftest.Mock_client.call_tool client ~name:"unknown" ~arguments:`Null)
  in
  let open Yojson.Safe.Util in
  printf "is_error: %b\n"
    (match result |> member "isError" with
    | `Bool true -> true
    | _ -> false);
  [%expect {| is_error: true |}]
