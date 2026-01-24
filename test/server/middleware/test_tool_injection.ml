(** Tests for tool injection middleware. *)

open! Core
open! Expect_test_helpers_core
open Server_middleware.Tool_injection

(* =============================================================================
   Tests for Injectable_tool
   ============================================================================= *)

let%expect_test "Injectable_tool - create with name and schema" =
  let tool =
    Injectable_tool.create ~name:"test_tool"
      ~input_schema:
        (`Assoc [ ("type", `String "object"); ("properties", `Assoc []) ])
      ~run:(fun _args -> Async.return (`String "result"))
      ()
  in
  printf "name: %s\n" (Injectable_tool.name tool);
  printf "has description: %b\n"
    (Option.is_some (Injectable_tool.description tool));
  [%expect {|
    name: test_tool
    has description: false
    |}]

let%expect_test "Injectable_tool - create with description" =
  let tool =
    Injectable_tool.create ~name:"add" ~description:"Add two numbers"
      ~input_schema:(`Assoc [ ("type", `String "object") ])
      ~run:(fun _args -> Async.return (`Int 42))
      ()
  in
  printf "name: %s\n" (Injectable_tool.name tool);
  printf "description: %s\n"
    (Option.value (Injectable_tool.description tool) ~default:"none");
  [%expect {|
    name: add
    description: Add two numbers
    |}]

let%expect_test "Injectable_tool - input_schema" =
  let schema =
    `Assoc
      [
        ("type", `String "object");
        ( "properties",
          `Assoc
            [
              ("a", `Assoc [ ("type", `String "integer") ]);
              ("b", `Assoc [ ("type", `String "integer") ]);
            ] );
      ]
  in
  let tool =
    Injectable_tool.create ~name:"multiply" ~input_schema:schema
      ~run:(fun _args -> Async.return (`Int 0))
      ()
  in
  printf "has input_schema: %b\n"
    (not (Yojson.Safe.equal (Injectable_tool.input_schema tool) `Null));
  [%expect {| has input_schema: true |}]

(* =============================================================================
   Tests for ToolInjectionMiddleware
   ============================================================================= *)

let%expect_test "ToolInjectionMiddleware - create empty" =
  let middleware = ToolInjectionMiddleware.create ~tools:[] () in
  printf "tools count: %d\n"
    (List.length (ToolInjectionMiddleware.tools middleware));
  [%expect {| tools count: 0 |}]

let%expect_test "ToolInjectionMiddleware - create with tools" =
  let tool1 =
    Injectable_tool.create ~name:"tool1"
      ~input_schema:(`Assoc [ ("type", `String "object") ])
      ~run:(fun _args -> Async.return `Null)
      ()
  in
  let tool2 =
    Injectable_tool.create ~name:"tool2"
      ~input_schema:(`Assoc [ ("type", `String "object") ])
      ~run:(fun _args -> Async.return `Null)
      ()
  in
  let middleware = ToolInjectionMiddleware.create ~tools:[ tool1; tool2 ] () in
  printf "tools count: %d\n"
    (List.length (ToolInjectionMiddleware.tools middleware));
  [%expect {| tools count: 2 |}]

let%expect_test "ToolInjectionMiddleware - find_tool" =
  let tool =
    Injectable_tool.create ~name:"find_me"
      ~input_schema:(`Assoc [ ("type", `String "object") ])
      ~run:(fun _args -> Async.return `Null)
      ()
  in
  let middleware = ToolInjectionMiddleware.create ~tools:[ tool ] () in
  printf "found: %b\n"
    (Option.is_some (ToolInjectionMiddleware.find_tool middleware "find_me"));
  printf "not_found: %b\n"
    (Option.is_none (ToolInjectionMiddleware.find_tool middleware "not_here"));
  [%expect {|
    found: true
    not_found: true
    |}]

let%expect_test "ToolInjectionMiddleware - has_tool" =
  let tool =
    Injectable_tool.create ~name:"exists"
      ~input_schema:(`Assoc [ ("type", `String "object") ])
      ~run:(fun _args -> Async.return `Null)
      ()
  in
  let middleware = ToolInjectionMiddleware.create ~tools:[ tool ] () in
  printf "has exists: %b\n"
    (ToolInjectionMiddleware.has_tool middleware "exists");
  printf "has missing: %b\n"
    (ToolInjectionMiddleware.has_tool middleware "missing");
  [%expect {|
    has exists: true
    has missing: false
    |}]

let%expect_test "ToolInjectionMiddleware - tool_names" =
  let tool1 =
    Injectable_tool.create ~name:"alpha"
      ~input_schema:(`Assoc [ ("type", `String "object") ])
      ~run:(fun _args -> Async.return `Null)
      ()
  in
  let tool2 =
    Injectable_tool.create ~name:"beta"
      ~input_schema:(`Assoc [ ("type", `String "object") ])
      ~run:(fun _args -> Async.return `Null)
      ()
  in
  let middleware = ToolInjectionMiddleware.create ~tools:[ tool1; tool2 ] () in
  let names = ToolInjectionMiddleware.tool_names middleware in
  List.iter names ~f:(fun name -> printf "tool: %s\n" name);
  [%expect {|
    tool: alpha
    tool: beta
    |}]

let%expect_test "ToolInjectionMiddleware - run_tool returns Some for existing \
                 tool" =
  let tool =
    Injectable_tool.create ~name:"runner"
      ~input_schema:(`Assoc [ ("type", `String "object") ])
      ~run:(fun _args -> Async.return (`String "executed"))
      ()
  in
  let middleware = ToolInjectionMiddleware.create ~tools:[ tool ] () in
  printf "run existing: %b\n"
    (Option.is_some
       (ToolInjectionMiddleware.run_tool middleware "runner" `Null));
  printf "run missing: %b\n"
    (Option.is_none
       (ToolInjectionMiddleware.run_tool middleware "missing" `Null));
  [%expect {|
    run existing: true
    run missing: true
    |}]

(* =============================================================================
   Tests for PromptToolMiddleware
   ============================================================================= *)

let%expect_test "PromptToolMiddleware - create" =
  let middleware = PromptToolMiddleware.create () in
  let tool_injection = PromptToolMiddleware.tool_injection middleware in
  printf "has list_prompts: %b\n"
    (ToolInjectionMiddleware.has_tool tool_injection "list_prompts");
  printf "has get_prompt: %b\n"
    (ToolInjectionMiddleware.has_tool tool_injection "get_prompt");
  [%expect {|
    has list_prompts: true
    has get_prompt: true
    |}]

let%expect_test "PromptToolMiddleware - tool names" =
  let middleware = PromptToolMiddleware.create () in
  let tool_injection = PromptToolMiddleware.tool_injection middleware in
  let names = ToolInjectionMiddleware.tool_names tool_injection in
  List.iter names ~f:(fun name -> printf "tool: %s\n" name);
  [%expect {|
    tool: list_prompts
    tool: get_prompt
    |}]

(* =============================================================================
   Tests for ResourceToolMiddleware
   ============================================================================= *)

let%expect_test "ResourceToolMiddleware - create" =
  let middleware = ResourceToolMiddleware.create () in
  let tool_injection = ResourceToolMiddleware.tool_injection middleware in
  printf "has list_resources: %b\n"
    (ToolInjectionMiddleware.has_tool tool_injection "list_resources");
  printf "has read_resource: %b\n"
    (ToolInjectionMiddleware.has_tool tool_injection "read_resource");
  [%expect {|
    has list_resources: true
    has read_resource: true
    |}]

let%expect_test "ResourceToolMiddleware - tool names" =
  let middleware = ResourceToolMiddleware.create () in
  let tool_injection = ResourceToolMiddleware.tool_injection middleware in
  let names = ToolInjectionMiddleware.tool_names tool_injection in
  List.iter names ~f:(fun name -> printf "tool: %s\n" name);
  [%expect {|
    tool: list_resources
    tool: read_resource
    |}]

(* =============================================================================
   Tests for input schema structure
   ============================================================================= *)

let%expect_test "Injectable_tool - json schema structure" =
  let schema =
    `Assoc
      [
        ("type", `String "object");
        ( "properties",
          `Assoc [ ("name", `Assoc [ ("type", `String "string") ]) ] );
        ("required", `List [ `String "name" ]);
      ]
  in
  let tool =
    Injectable_tool.create ~name:"schema_test" ~input_schema:schema
      ~run:(fun _args -> Async.return `Null)
      ()
  in
  let output = Injectable_tool.input_schema tool in
  printf "schema type: %s\n"
    (match output with
    | `Assoc _ -> "object"
    | _ -> "other");
  [%expect {| schema type: object |}]
