(** Unit tests for Bulk Tool Caller module *)

open Core
open Async
open Expect_test_helpers_core

(* Helper: Create a mock call_tool function that returns success *)
let mock_call_tool_success ~name ~arguments =
  let content =
    [
      `Text
        {
          Mcp.Types.type_ = `Text;
          text = sprintf "Called %s with %s" name (Yojson.Safe.to_string arguments);
          annotations = None;
          meta = None;
        };
    ]
  in
  return
    {
      Mcp.Types.content;
      structured_content = None;
      is_error = false;
      result = { Mcp.Types.meta = None };
    }

(* Helper: Create a mock call_tool function that returns error *)
let mock_call_tool_error ~name ~arguments:_ =
  let content =
    [
      `Text
        {
          Mcp.Types.type_ = `Text;
          text = sprintf "Error calling %s" name;
          annotations = None;
          meta = None;
        };
    ]
  in
  return
    {
      Mcp.Types.content;
      structured_content = None;
      is_error = true;
      result = { Mcp.Types.meta = None };
    }

(* Test type JSON serialization *)
let%expect_test "call_tool_request JSON round-trip" =
  let request =
    { Bulk_tool_caller.tool = "test_tool"; arguments = `Assoc [ ("x", `Int 42) ] }
  in
  let json = Bulk_tool_caller.yojson_of_call_tool_request request in
  print_endline (Yojson.Safe.pretty_to_string json);
  let decoded = Bulk_tool_caller.call_tool_request_of_yojson json in
  print_s [%sexp (String.equal request.tool decoded.tool : bool)];
  [%expect
    {|
    { "tool": "test_tool", "arguments": { "x": 42 } }
    true |}];
  return ()

let%expect_test "call_tool_request_result JSON round-trip" =
  let result =
    {
      Bulk_tool_caller.tool = "test_tool";
      arguments = `Assoc [ ("x", `Int 42) ];
      is_error = false;
      content = [ `String "result" ];
    }
  in
  let json = Bulk_tool_caller.yojson_of_call_tool_request_result result in
  print_endline (Yojson.Safe.pretty_to_string json);
  let decoded = Bulk_tool_caller.call_tool_request_result_of_yojson json in
  print_s [%sexp (String.equal result.tool decoded.tool : bool)];
  print_s [%sexp (Bool.equal result.is_error decoded.is_error : bool)];
  [%expect
    {|
    {
      "tool": "test_tool",
      "arguments": { "x": 42 },
      "isError": false,
      "content": [ "result" ]
    }
    true
    true |}];
  return ()

(* Test bulk execution - success case *)
let%expect_test "call_tools_bulk success" =
  let caller = Bulk_tool_caller.create () in
  let tool_calls =
    [
      { Bulk_tool_caller.tool = "tool1"; arguments = `Assoc [ ("a", `Int 1) ] };
      { Bulk_tool_caller.tool = "tool2"; arguments = `Assoc [ ("b", `Int 2) ] };
    ]
  in
  let%bind results =
    Bulk_tool_caller.call_tools_bulk caller ~tool_calls ~continue_on_error:true
      ~call_tool:mock_call_tool_success ()
  in
  print_s [%sexp (List.length results : int)];
  List.iter results ~f:(fun r ->
      print_s [%sexp (r.Bulk_tool_caller.tool : string)];
      print_s [%sexp (r.Bulk_tool_caller.is_error : bool)]);
  [%expect
    {|
    2
    tool1
    false
    tool2
    false |}];
  return ()

(* Test bulk execution with error - continue_on_error:true *)
let%expect_test "call_tools_bulk error continue" =
  let caller = Bulk_tool_caller.create () in
  let tool_calls =
    [
      { Bulk_tool_caller.tool = "tool1"; arguments = `Assoc [] };
      { Bulk_tool_caller.tool = "error_tool"; arguments = `Assoc [] };
      { Bulk_tool_caller.tool = "tool2"; arguments = `Assoc [] };
    ]
  in
  let call_tool ~name ~arguments =
    if String.equal name "error_tool" then mock_call_tool_error ~name ~arguments
    else mock_call_tool_success ~name ~arguments
  in
  let%bind results =
    Bulk_tool_caller.call_tools_bulk caller ~tool_calls ~continue_on_error:true
      ~call_tool ()
  in
  print_s [%sexp (List.length results : int)];
  List.iter results ~f:(fun r ->
      printf "%s: is_error=%b\n" r.Bulk_tool_caller.tool r.Bulk_tool_caller.is_error);
  [%expect
    {|
    3
    tool1: is_error=false
    error_tool: is_error=true
    tool2: is_error=false |}];
  return ()

(* Test bulk execution with error - continue_on_error:false (early termination) *)
let%expect_test "call_tools_bulk error stop" =
  let caller = Bulk_tool_caller.create () in
  let tool_calls =
    [
      { Bulk_tool_caller.tool = "tool1"; arguments = `Assoc [] };
      { Bulk_tool_caller.tool = "error_tool"; arguments = `Assoc [] };
      { Bulk_tool_caller.tool = "tool2"; arguments = `Assoc [] };
    ]
  in
  let call_tool ~name ~arguments =
    if String.equal name "error_tool" then mock_call_tool_error ~name ~arguments
    else mock_call_tool_success ~name ~arguments
  in
  let%bind results =
    Bulk_tool_caller.call_tools_bulk caller ~tool_calls ~continue_on_error:false
      ~call_tool ()
  in
  (* Should stop after error_tool *)
  print_s [%sexp (List.length results : int)];
  List.iter results ~f:(fun r ->
      printf "%s: is_error=%b\n" r.Bulk_tool_caller.tool r.Bulk_tool_caller.is_error);
  [%expect
    {|
    2
    tool1: is_error=false
    error_tool: is_error=true |}];
  return ()

(* Test call_tool_bulk - same tool, multiple args *)
let%expect_test "call_tool_bulk success" =
  let caller = Bulk_tool_caller.create () in
  let tool_arguments =
    [
      `Assoc [ ("x", `Int 1) ];
      `Assoc [ ("x", `Int 2) ];
      `Assoc [ ("x", `Int 3) ];
    ]
  in
  let%bind results =
    Bulk_tool_caller.call_tool_bulk caller ~tool:"echo" ~tool_arguments
      ~continue_on_error:true ~call_tool:mock_call_tool_success ()
  in
  print_s [%sexp (List.length results : int)];
  List.iter results ~f:(fun r ->
      print_s [%sexp (r.Bulk_tool_caller.tool : string)]);
  [%expect
    {|
    3
    echo
    echo
    echo |}];
  return ()
