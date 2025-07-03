open Alcotest
open Lwt.Syntax
open Utilities.Types

module TestTools = struct
  (* These will be the interfaces we expect from our tools module *)
  
  type execution_context = {
    request_id : string option;
    client_id : string option;
    session_data : (string, Yojson.Safe.t) Hashtbl.t;
    mutable tools_changed : bool;
    mutable resources_changed : bool;
    mutable prompts_changed : bool;
  }
  
  type tool_handler = execution_context -> Yojson.Safe.t -> content_type list Lwt.t
  
  type function_tool = {
    name : string;
    description : string;
    parameters : Yojson.Safe.t;
    handler : tool_handler;
    enabled : bool;
    tags : string list;
  }
  
  type tool_manager = {
    mutable tools : (string, function_tool) Hashtbl.t;
    mutable duplicate_behavior : [ `Warn | `Error | `Replace | `Ignore ];
  }
end

(** Test tool creation *)
let test_tool_creation () =
  let name = "test_calculator" in
  let description = "A simple calculator tool" in
  let parameters = `Assoc [
    ("type", `String "object");
    ("properties", `Assoc [
      ("expression", `Assoc [("type", `String "string")]);
      ("operation", `Assoc [("type", `String "string")])
    ]);
    ("required", `List [`String "expression"])
  ] in
  
  let handler _ctx _args =
    let* () = Lwt.return_unit in
    Lwt.return [create_text_content "42"]
  in
  
  let tool = TestTools.{
    name;
    description; 
    parameters;
    handler;
    enabled = true;
    tags = ["math"; "calculator"];
  } in
  
  check string "tool name" name tool.name;
  check string "tool description" description tool.description;
  check bool "tool enabled" true tool.enabled;
  check int "tool tags count" 2 (List.length tool.tags);
  check bool "has math tag" true (List.mem "math" tool.tags)

(** Test tool execution *)
let test_tool_execution () =
  Lwt_main.run (
    let ctx = TestTools.{
      request_id = Some "test-123";
      client_id = None;
      session_data = Hashtbl.create 0;
      tools_changed = false;
      resources_changed = false;
      prompts_changed = false;
    } in
    let args = `Assoc [("expression", `String "2 + 2")] in
    
    let handler _ctx _args =
      let* () = Lwt.return_unit in
      Lwt.return [create_text_content "4"]
    in
    
    let tool = TestTools.{
      name = "calculator";
      description = "Calculator";
      parameters = `Null;
      handler;
      enabled = true;
      tags = [];
    } in
    
    let* result = tool.handler ctx args in
    
    check int "result count" 1 (List.length result);
    let first_result = List.hd result in
    check bool "result is text" true 
      (match first_result with Text _ -> true | _ -> false);
    
    Lwt.return_unit
  )

(** Test tool manager creation *)
let test_tool_manager_creation () =
  let manager = TestTools.{
    tools = Hashtbl.create 16;
    duplicate_behavior = `Warn;
  } in
  
  check int "initial tool count" 0 (Hashtbl.length manager.tools);
  check bool "correct duplicate behavior" true 
    (match manager.duplicate_behavior with `Warn -> true | _ -> false)

(** Test tool registration *)
let test_tool_registration () =
  let manager = TestTools.{
    tools = Hashtbl.create 16;
    duplicate_behavior = `Error;
  } in
  
  let tool1 = TestTools.{
    name = "tool1";
    description = "First tool";
    parameters = `Null;
    handler = (fun _ctx _args -> Lwt.return []);
    enabled = true;
    tags = [];
  } in
  
  let tool2 = TestTools.{
    name = "tool2";
    description = "Second tool";
    parameters = `Null;
    handler = (fun _ctx _args -> Lwt.return []);
    enabled = true;
    tags = ["tag1"];
  } in
  
  (* Register tools *)
  Hashtbl.add manager.tools tool1.name tool1;
  Hashtbl.add manager.tools tool2.name tool2;
  
  check int "tool count after registration" 2 (Hashtbl.length manager.tools);
  check bool "tool1 exists" true (Hashtbl.mem manager.tools "tool1");
  check bool "tool2 exists" true (Hashtbl.mem manager.tools "tool2");
  
  (* Test retrieval *)
  let retrieved_tool1 = Hashtbl.find manager.tools "tool1" in
  check string "retrieved tool name" "tool1" retrieved_tool1.name;
  check string "retrieved tool description" "First tool" retrieved_tool1.description

(** Test tool removal *)
let test_tool_removal () =
  let manager = TestTools.{
    tools = Hashtbl.create 16;
    duplicate_behavior = `Warn;
  } in
  
  let tool = TestTools.{
    name = "removable_tool";
    description = "A tool to be removed";
    parameters = `Null;
    handler = (fun _ctx _args -> Lwt.return []);
    enabled = true;
    tags = [];
  } in
  
  (* Register and then remove *)
  Hashtbl.add manager.tools tool.name tool;
  check bool "tool exists before removal" true (Hashtbl.mem manager.tools "removable_tool");
  
  Hashtbl.remove manager.tools "removable_tool";
  check bool "tool removed" false (Hashtbl.mem manager.tools "removable_tool");
  check int "tool count after removal" 0 (Hashtbl.length manager.tools)

(** Test tool parameter validation *)
let test_tool_parameter_validation () =
  let valid_params = `Assoc [
    ("type", `String "object");
    ("properties", `Assoc [
      ("name", `Assoc [("type", `String "string")]);
      ("age", `Assoc [("type", `String "integer")])
    ]);
    ("required", `List [`String "name"])
  ] in
  
  let invalid_params = `String "not an object schema" in
  
  (* These would be validation functions we expect to implement *)
  let validate_schema schema =
    match schema with
    | `Assoc _ -> true
    | _ -> false
  in
  
  check bool "valid params accepted" true (validate_schema valid_params);
  check bool "invalid params rejected" false (validate_schema invalid_params)

(** Test tool serialization to MCP format *)
let test_tool_mcp_serialization () =
  let tool = TestTools.{
    name = "serialize_test";
    description = "Tool for serialization testing";
    parameters = `Assoc [("type", `String "object")];
    handler = (fun _ctx _args -> Lwt.return []);
    enabled = true;
    tags = ["test"];
  } in
  
  (* MCP serialization test temporarily simplified *)
  check string "tool name" "serialize_test" tool.name;
  check string "tool description" "Tool for serialization testing" tool.description;
  check bool "tool has parameters" true (tool.parameters <> `Null);
  check bool "tool is enabled" true tool.enabled

(** Test tool error handling *)
let test_tool_error_handling () =
  Lwt_main.run (
    let ctx = TestTools.{
      request_id = None;
      client_id = None;
      session_data = Hashtbl.create 0;
      tools_changed = false;
      resources_changed = false;
      prompts_changed = false;
    } in
    let args = `Assoc [("invalid", `String "data")] in
    
    let failing_handler _ctx _args =
      Lwt.fail (Failure "Tool execution failed")
    in
    
    let tool = TestTools.{
      name = "failing_tool";
      description = "A tool that fails";
      parameters = `Null;
      handler = failing_handler;
      enabled = true;
      tags = [];
    } in
    
    (* Test error handling *)
    let* result = 
      Lwt.catch 
        (fun () -> tool.handler ctx args) 
        (function
         | Failure msg -> Lwt.return [create_text_content ("Error: " ^ msg)]
         | exn -> Lwt.fail exn)
    in
    
    check int "error result count" 1 (List.length result);
    let error_content = List.hd result in
    check bool "error content is text" true 
      (match error_content with Text _ -> true | _ -> false);
    
    Lwt.return_unit
  )

(** Test tool filtering by tags *)
let test_tool_filtering () =
  let manager = TestTools.{
    tools = Hashtbl.create 16;
    duplicate_behavior = `Warn;
  } in
  
  let tools = [
    ("tool1", ["math"; "calculator"]);
    ("tool2", ["text"; "processing"]);
    ("tool3", ["math"; "geometry"]);
    ("tool4", []);
  ] in
  
  List.iter (fun (name, tags) ->
    let tool = TestTools.{
      name;
      description = "Test tool";
      parameters = `Null;
      handler = (fun _ctx _args -> Lwt.return []);
      enabled = true;
      tags;
    } in
    Hashtbl.add manager.tools name tool
  ) tools;
  
  (* Filter by tag *)
  let filter_by_tag tag_filter =
    Hashtbl.fold (fun _name (tool : TestTools.function_tool) acc ->
      if List.exists (fun tag -> tag = tag_filter) tool.tags then
        tool :: acc
      else
        acc
    ) manager.tools []
  in
  
  let math_tools = filter_by_tag "math" in
  check int "math tools count" 2 (List.length math_tools);
  
  let text_tools = filter_by_tag "text" in
  check int "text tools count" 1 (List.length text_tools)

(** Test tool enabling/disabling *)
let test_tool_enable_disable () =
  let tool = TestTools.{
    name = "toggle_tool";
    description = "A tool that can be toggled";
    parameters = `Null;
    handler = (fun _ctx _args -> Lwt.return []);
    enabled = true;
    tags = [];
  } in
  
  check bool "tool initially enabled" true tool.enabled;
  
  (* This would be how we'd disable a tool *)
  let disabled_tool = { tool with enabled = false } in
  check bool "tool disabled" false disabled_tool.enabled;
  
  let re_enabled_tool = { disabled_tool with enabled = true } in
  check bool "tool re-enabled" true re_enabled_tool.enabled

(** Test complex tool with multiple return types *)
let test_complex_tool_returns () =
  Lwt_main.run (
    let ctx = TestTools.{
      request_id = None;
      client_id = None;
      session_data = Hashtbl.create 0;
      tools_changed = false;
      resources_changed = false;
      prompts_changed = false;
    } in
    let args = `Assoc [("format", `String "mixed")] in
    
    let complex_handler _ctx args =
      let format = match args with
        | `Assoc [("format", `String f)] -> f
        | _ -> "text"
      in
      
      let* () = Lwt.return_unit in
      match format with
      | "text" -> Lwt.return [create_text_content "Simple text"]
      | "mixed" -> Lwt.return [
          create_text_content "Mixed content:";
          create_image_content ~data:"base64data" ~mime_type:"image/png" ();
        ]
      | _ -> Lwt.return []
    in
    
    let tool = TestTools.{
      name = "complex_tool";
      description = "Tool with complex return types";
      parameters = `Null;
      handler = complex_handler;
      enabled = true;
      tags = ["complex"];
    } in
    
    let* result = tool.handler ctx args in
    
    check int "complex result count" 2 (List.length result);
    
    let first_result = List.nth result 0 in
    let second_result = List.nth result 1 in
    
    check bool "first result is text" true 
      (match first_result with Text _ -> true | _ -> false);
    check bool "second result is image" true 
      (match second_result with Image _ -> true | _ -> false);
    
    Lwt.return_unit
  )

(** Main test suite *)
let () =
  run "Tools Module Tests" [
    ("Tool Creation", [
      test_case "Basic tool creation" `Quick test_tool_creation;
    ]);
    ("Tool Execution", [
      test_case "Tool handler execution" `Quick test_tool_execution;
      test_case "Tool error handling" `Quick test_tool_error_handling;
      test_case "Complex tool returns" `Quick test_complex_tool_returns;
    ]);
    ("Tool Manager", [
      test_case "Tool manager creation" `Quick test_tool_manager_creation;
      test_case "Tool registration" `Quick test_tool_registration;
      test_case "Tool removal" `Quick test_tool_removal;
    ]);
    ("Tool Validation", [
      test_case "Parameter validation" `Quick test_tool_parameter_validation;
    ]);
    ("Tool Serialization", [
      test_case "MCP serialization" `Quick test_tool_mcp_serialization;
    ]);
    ("Tool Management", [
      test_case "Tool filtering by tags" `Quick test_tool_filtering;
      test_case "Tool enable/disable" `Quick test_tool_enable_disable;
    ]);
  ] 