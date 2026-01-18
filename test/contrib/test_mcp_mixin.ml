(** Unit tests for MCP Mixin module *)

open Core
open Async
open Expect_test_helpers_core

type json = Yojson.Safe.t

(* Test registration info type constructors *)
let%expect_test "make_tool_info creates valid record" =
  let info =
    Mcp_mixin.make_tool_info ~name:"test_tool" ~description:"A test tool"
      ~tags:[ "test"; "example" ] ~enabled:true ()
  in
  print_s [%sexp (info.name : string)];
  print_s [%sexp (info.description : string option)];
  print_s [%sexp (info.tags : string list option)];
  print_s [%sexp (info.enabled : bool option)];
  [%expect
    {|
    test_tool
    ("A test tool")
    ((test example))
    (true) |}];
  return ()

let%expect_test "make_resource_info creates valid record" =
  let info =
    Mcp_mixin.make_resource_info ~uri:"file:///test" ~name:"test_resource"
      ~title:"Test Resource" ~description:"A test resource"
      ~mime_type:"text/plain" ()
  in
  print_s [%sexp (info.uri : string)];
  print_s [%sexp (info.name : string)];
  print_s [%sexp (info.title : string option)];
  print_s [%sexp (info.mime_type : string option)];
  [%expect
    {|
    file:///test
    test_resource
    ("Test Resource")
    (text/plain) |}];
  return ()

let%expect_test "make_prompt_info creates valid record" =
  let info =
    Mcp_mixin.make_prompt_info ~name:"test_prompt" ~title:"Test Prompt"
      ~description:"A test prompt" ~tags:[ "demo" ] ()
  in
  print_s [%sexp (info.name : string)];
  print_s [%sexp (info.title : string option)];
  print_s [%sexp (info.tags : string list option)];
  [%expect
    {|
    test_prompt
    ("Test Prompt")
    ((demo)) |}];
  return ()

(* Test JSON serialization of registration info types *)
let%expect_test "tool_registration_info JSON round-trip" =
  let info =
    Mcp_mixin.make_tool_info ~name:"json_tool" ~description:"JSON test" ()
  in
  let json = Mcp_mixin.yojson_of_tool_registration_info info in
  print_endline (Yojson.Safe.pretty_to_string json);
  let decoded = Mcp_mixin.tool_registration_info_of_yojson json in
  print_s [%sexp (String.equal info.name decoded.name : bool)];
  [%expect
    {|
    { "name": "json_tool", "description": "JSON test" }
    true |}];
  return ()

let%expect_test "resource_registration_info JSON round-trip" =
  let info =
    Mcp_mixin.make_resource_info ~uri:"test://uri" ~name:"json_resource" ()
  in
  let json = Mcp_mixin.yojson_of_resource_registration_info info in
  print_endline (Yojson.Safe.pretty_to_string json);
  let decoded = Mcp_mixin.resource_registration_info_of_yojson json in
  print_s [%sexp (String.equal info.uri decoded.uri : bool)];
  [%expect
    {|
    { "uri": "test://uri", "name": "json_resource" }
    true |}];
  return ()

let%expect_test "prompt_registration_info JSON round-trip" =
  let info = Mcp_mixin.make_prompt_info ~name:"json_prompt" () in
  let json = Mcp_mixin.yojson_of_prompt_registration_info info in
  print_endline (Yojson.Safe.pretty_to_string json);
  let decoded = Mcp_mixin.prompt_registration_info_of_yojson json in
  print_s [%sexp (String.equal info.name decoded.name : bool)];
  [%expect
    {|
    { "name": "json_prompt" }
    true |}];
  return ()

(* Test tool registration *)
let%expect_test "register_tools adds tools to manager" =
  let tool_handler _ctx _args = return [ Fmcp_types.create_text_content "result" ] in
  let tools : Mcp_mixin.registered_tool list =
    [
      {
        handler = tool_handler;
        info = Mcp_mixin.make_tool_info ~name:"add" ~description:"Add numbers" ();
      };
      {
        handler = tool_handler;
        info =
          Mcp_mixin.make_tool_info ~name:"subtract" ~description:"Subtract numbers" ();
      };
    ]
  in
  let manager = Tool.create_manager () in
  Mcp_mixin.register_tools ~tools ~manager ();
  let has_add = Option.is_some (Tool.get_tool manager "add") in
  let has_subtract = Option.is_some (Tool.get_tool manager "subtract") in
  print_s [%sexp (has_add : bool)];
  print_s [%sexp (has_subtract : bool)];
  [%expect
    {|
    true
    true |}];
  return ()

(* Test registration with prefix *)
let%expect_test "register_tools with prefix" =
  let tool_handler _ctx _args = return [ Fmcp_types.create_text_content "result" ] in
  let tools : Mcp_mixin.registered_tool list =
    [
      {
        handler = tool_handler;
        info = Mcp_mixin.make_tool_info ~name:"calc" ~description:"Calculator" ();
      };
    ]
  in
  let manager = Tool.create_manager () in
  Mcp_mixin.register_tools ~tools ~manager ~prefix:"math" ~separator:"_" ();
  let has_prefixed = Option.is_some (Tool.get_tool manager "math_calc") in
  let has_original = Option.is_some (Tool.get_tool manager "calc") in
  print_s [%sexp (has_prefixed : bool)];
  print_s [%sexp (has_original : bool)];
  [%expect
    {|
    true
    false |}];
  return ()

(* Test bulk tool caller re-exports *)
let%expect_test "bulk caller types are re-exported" =
  (* Verify types are accessible through Mcp_mixin *)
  let request : Mcp_mixin.call_tool_request =
    { tool = "test"; arguments = `Assoc [] }
  in
  print_s [%sexp (request.tool : string)];
  let result : Mcp_mixin.call_tool_request_result =
    { tool = "test"; arguments = `Assoc []; is_error = false; content = [] }
  in
  print_s [%sexp (result.is_error : bool)];
  [%expect
    {|
    test
    false |}];
  return ()

let%expect_test "create_bulk_caller works" =
  let caller = Mcp_mixin.create_bulk_caller () in
  (* Just verify it doesn't crash and returns a valid caller *)
  ignore caller;
  print_endline "Bulk caller created successfully";
  [%expect {| Bulk caller created successfully |}];
  return ()
