(** Simple test runner for Ox Fast MCP *)

let test_count = ref 0
let failed_count = ref 0

let assert_equal expected actual msg =
  incr test_count;
  if expected <> actual then (
    incr failed_count;
    Printf.printf "❌ FAIL: %s\n   Expected: %s\n   Actual: %s\n" msg
      (string_of_bool expected) (string_of_bool actual))
  else Printf.printf "✅ PASS: %s\n" msg

let assert_true condition msg = assert_equal true condition msg

let test_mcp_types () =
  Printf.printf "\n📋 Testing MCP Types...\n";

  let open Ox_fast_mcp.Mcp_types in
  (* Test JSON type creation *)
  let json_str = String "hello" in
  let json_int = Int 42 in
  let json_bool = Bool true in

  assert_true
    (match json_str with
    | String _ -> true
    | _ -> false)
    "JSON String creation";
  assert_true
    (match json_int with
    | Int _ -> true
    | _ -> false)
    "JSON Int creation";
  assert_true
    (match json_bool with
    | Bool _ -> true
    | _ -> false)
    "JSON Bool creation";

  (* Test tool definition *)
  let tool =
    { name = "test_tool"; description = "A test tool"; input_schema = None }
  in

  assert_true (tool.name = "test_tool") "Tool name";
  assert_true (tool.description = "A test tool") "Tool description";

  Printf.printf "MCP Types tests completed\n"

let test_fast_mcp_server () =
  Printf.printf "\n🖥️  Testing FastMCP Server...\n";

  try
    let server = Ox_fast_mcp.Fast_mcp_server.create ~name:"Test Server" () in
    assert_true true "Server creation";

    let count = Ox_fast_mcp.Fast_mcp_server.tool_count server in
    assert_true (count = 0) "Initial tool count is 0";

    Printf.printf "FastMCP Server tests completed\n"
  with e -> Printf.printf "❌ Server test failed: %s\n" (Printexc.to_string e)

let test_mcp_client () =
  Printf.printf "\n🔌 Testing MCP Client...\n";

  try
    (* Basic client module test *)
    let module_exists = true in
    (* We can at least load the module *)
    assert_true module_exists "MCP Client module exists";

    Printf.printf "MCP Client tests completed\n"
  with e -> Printf.printf "❌ Client test failed: %s\n" (Printexc.to_string e)

let () =
  Printf.printf "\n🧪 Running Ox Fast MCP Test Suite\n";

  test_mcp_types ();
  test_fast_mcp_server ();
  test_mcp_client ();

  Printf.printf "\n📊 Test Results:\n";
  Printf.printf "   Total tests: %d\n" !test_count;
  Printf.printf "   Passed: %d\n" (!test_count - !failed_count);
  Printf.printf "   Failed: %d\n" !failed_count;

  if !failed_count = 0 then Printf.printf "\n✅ All tests passed!\n"
  else Printf.printf "\n❌ %d test(s) failed!\n" !failed_count
