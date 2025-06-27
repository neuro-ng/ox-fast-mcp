open Alcotest
open Lwt.Syntax
open Ox_fast_mcp
open Mcp_types

(* Test server creation and basic functionality *)
let test_server_creation () =
  let server = Fast_mcp_server.create ~name:"Test Server" () in
  check string "server name" "Test Server" (Fast_mcp_server.get_name server);
  check int "initial tool count" 0 (Fast_mcp_server.tool_count server);
  check int "initial resource count" 0 (Fast_mcp_server.resource_count server);
  check int "initial prompt count" 0 (Fast_mcp_server.prompt_count server)

let test_server_with_instructions () =
  let instructions =
    "This server provides mathematical operations and data analysis tools."
  in
  let server = Fast_mcp_server.create ~name:"Math Server" ~instructions () in
  match Fast_mcp_server.get_instructions server with
  | Some inst -> check string "server instructions" instructions inst
  | None -> fail "Expected instructions to be set"

(* Test tool registration *)
let test_tool_registration () =
  let server = Fast_mcp_server.create ~name:"Tool Test Server" () in

  (* Simple tool function *)
  let add_tool a b = a + b in

  (* Register tool *)
  Fast_mcp_server.register_tool server ~name:"add"
    ~description:"Add two numbers" ~func:add_tool;

  check int "tool count after registration" 1
    (Fast_mcp_server.tool_count server);
  check bool "tool exists" true (Fast_mcp_server.has_tool server "add")

let test_tool_with_complex_params () =
  let server = Fast_mcp_server.create ~name:"Complex Tool Server" () in

  (* Tool with JSON parameters *)
  let complex_tool ~name ~age ~hobbies =
    Printf.sprintf "User %s is %d years old and likes: %s" name age
      (String.concat ", " hobbies)
  in

  Fast_mcp_server.register_tool server ~name:"create_profile"
    ~description:"Create a user profile" ~func:complex_tool;

  check int "complex tool count" 1 (Fast_mcp_server.tool_count server);
  check bool "complex tool exists" true
    (Fast_mcp_server.has_tool server "create_profile")

(* Test resource registration *)
let test_resource_registration () =
  let server = Fast_mcp_server.create ~name:"Resource Test Server" () in

  (* Simple resource *)
  let config_resource () =
    `Assoc [ ("version", `String "1.0.0"); ("debug", `Bool true) ]
  in

  Fast_mcp_server.register_resource server ~uri:"config://settings"
    ~name:"Application Config" ~description:"Application configuration"
    ~func:config_resource;

  check int "resource count" 1 (Fast_mcp_server.resource_count server);
  check bool "resource exists" true
    (Fast_mcp_server.has_resource server "config://settings")

let test_resource_template () =
  let server = Fast_mcp_server.create ~name:"Template Server" () in

  (* Parameterized resource *)
  let user_profile_resource user_id =
    `Assoc
      [
        ("id", `String user_id);
        ("name", `String ("User " ^ user_id));
        ("status", `String "active");
      ]
  in

  Fast_mcp_server.register_resource_template server
    ~uri_pattern:"users://{user_id}/profile" ~name:"User Profile"
    ~description:"Get user profile by ID" ~func:user_profile_resource;

  check int "template count" 1 (Fast_mcp_server.resource_count server);
  check bool "template exists" true
    (Fast_mcp_server.has_resource_template server "users://{user_id}/profile")

(* Test prompt registration *)
let test_prompt_registration () =
  let server = Fast_mcp_server.create ~name:"Prompt Server" () in

  (* Simple prompt *)
  let summarize_prompt text =
    Printf.sprintf "Please summarize the following text:\n\n%s" text
  in

  Fast_mcp_server.register_prompt server ~name:"summarize"
    ~description:"Generate a text summary" ~func:summarize_prompt;

  check int "prompt count" 1 (Fast_mcp_server.prompt_count server);
  check bool "prompt exists" true
    (Fast_mcp_server.has_prompt server "summarize")

(* Test tool execution *)
let test_tool_execution () =
  Lwt_main.run
    (let server = Fast_mcp_server.create ~name:"Execution Server" () in

     (* Register a tool *)
     let multiply_tool a b = a * b in
     Fast_mcp_server.register_tool server ~name:"multiply"
       ~description:"Multiply two numbers" ~func:multiply_tool;

     (* Execute the tool *)
     let* result =
       Fast_mcp_server.call_tool server "multiply"
         (`Assoc [ ("a", `Int 5); ("b", `Int 3) ])
     in

     match result with
     | Ok content -> (
       match content with
       | Text result_str ->
         check string "multiplication result" "15" result_str;
         Lwt.return_unit
       | _ -> fail "Expected text result")
     | Error err -> fail ("Tool execution failed: " ^ err))

(* Test resource access *)
let test_resource_access () =
  Lwt_main.run
    (let server = Fast_mcp_server.create ~name:"Resource Access Server" () in

     (* Register a resource *)
     let version_resource () = `String "2.1.0" in
     Fast_mcp_server.register_resource server ~uri:"app://version"
       ~name:"App Version" ~description:"Application version"
       ~func:version_resource;

     (* Access the resource *)
     let* result = Fast_mcp_server.read_resource server "app://version" in

     match result with
     | Ok content -> (
       match content with
       | Text result_str ->
         check string "version result" "2.1.0" result_str;
         Lwt.return_unit
       | _ -> fail "Expected text result")
     | Error err -> fail ("Resource access failed: " ^ err))

(* Test server capabilities *)
let test_server_capabilities () =
  let server = Fast_mcp_server.create ~name:"Capabilities Server" () in

  (* Add some tools and resources *)
  Fast_mcp_server.register_tool server ~name:"test_tool" ~description:"Test"
    ~func:(fun () -> "test");
  Fast_mcp_server.register_resource server ~uri:"test://resource" ~name:"Test"
    ~description:"Test" ~func:(fun () -> `String "test");
  Fast_mcp_server.register_prompt server ~name:"test_prompt" ~description:"Test"
    ~func:(fun () -> "test prompt");

  let capabilities = Fast_mcp_server.get_capabilities server in
  check bool "has tools capability" true (Option.is_some capabilities.tools);
  check bool "has resources capability" true
    (Option.is_some capabilities.resources);
  check bool "has prompts capability" true (Option.is_some capabilities.prompts)

(* Test error handling *)
let test_error_handling () =
  Lwt_main.run
    (let server = Fast_mcp_server.create ~name:"Error Server" () in

     (* Try to call non-existent tool *)
     let* result = Fast_mcp_server.call_tool server "nonexistent" `Null in
     match result with
     | Error _ ->
       check bool "error on nonexistent tool" true true;
       Lwt.return_unit
     | Ok _ -> fail "Expected error for nonexistent tool")

(* Test context injection *)
let test_context_injection () =
  Lwt_main.run
    (let server = Fast_mcp_server.create ~name:"Context Server" () in

     (* Tool that uses context *)
     let context_tool ctx value =
       let request_id =
         match ctx.request_id with
         | Some id -> id
         | None -> "unknown"
       in
       Printf.sprintf "Processing %s for request %s" value request_id
     in

     Fast_mcp_server.register_tool_with_context server ~name:"context_tool"
       ~description:"Tool that uses context" ~func:context_tool;

     (* Create context *)
     let ctx =
       {
         request_id = Some "test-req-123";
         client_id = Some "test-client";
         session_data = Hashtbl.create 10;
       }
     in

     (* Execute with context *)
     let* result =
       Fast_mcp_server.call_tool_with_context server ctx "context_tool"
         (`String "test_value")
     in

     match result with
     | Ok (Text result_str) ->
       check bool "context used" true
         (String.contains result_str "test-req-123");
       Lwt.return_unit
     | _ -> fail "Expected successful context tool execution")

let () =
  run "FastMCP Server Tests"
    [
      ( "Server Creation",
        [
          test_case "Basic server creation" `Quick test_server_creation;
          test_case "Server with instructions" `Quick
            test_server_with_instructions;
        ] );
      ( "Tool Registration",
        [
          test_case "Simple tool registration" `Quick test_tool_registration;
          test_case "Complex parameter tool" `Quick
            test_tool_with_complex_params;
        ] );
      ( "Resource Registration",
        [
          test_case "Static resource registration" `Quick
            test_resource_registration;
          test_case "Resource template registration" `Quick
            test_resource_template;
        ] );
      ( "Prompt Registration",
        [ test_case "Prompt registration" `Quick test_prompt_registration ] );
      ( "Execution",
        [
          test_case "Tool execution" `Quick test_tool_execution;
          test_case "Resource access" `Quick test_resource_access;
        ] );
      ( "Server Features",
        [
          test_case "Server capabilities" `Quick test_server_capabilities;
          test_case "Error handling" `Quick test_error_handling;
          test_case "Context injection" `Quick test_context_injection;
        ] );
    ]
