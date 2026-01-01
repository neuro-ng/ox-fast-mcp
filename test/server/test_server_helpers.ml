(** Phase 5 Server Enhancements Tests

    Tests for:
    - Tool name normalization
    - Batch operation helpers
    - Boolean validation helpers
    - Enhanced ergonomics *)

open Core
open Async

(* Follow test_import_server pattern - no module aliases needed *)

(** {1 Tool Name Normalization Tests} *)

let%expect_test "normalize_tool_name_spaces_to_underscores" =
  let name = "my tool name" in
  let normalized = Server.Ox_fast_mcp.normalize_tool_name name in
  print_endline normalized;
  [%expect {| my_tool_name |}];
  return ()

let%expect_test "normalize_tool_name_removes_special_chars" =
  let name = "my-tool@name!" in
  let normalized = Server.Ox_fast_mcp.normalize_tool_name name in
  print_endline normalized;
  [%expect {| my_tool_name_ |}];
  return ()

let%expect_test "normalize_tool_name_lowercase_conversion" =
  let name = "MyToolName" in
  let normalized = Server.Ox_fast_mcp.normalize_tool_name name in
  print_endline normalized;
  [%expect {| mytoolname |}];
  return ()

let%expect_test "normalize_tool_name_leading_digit" =
  (* Names starting with digit get prefixed with underscore *)
  let name = "123tool" in
  let normalized = Server.Ox_fast_mcp.normalize_tool_name name in
  print_endline normalized;
  [%expect {| _123tool |}];
  return ()

let%expect_test "normalize_tool_name_preserves_underscore" =
  let name = "_myTool" in
  let normalized = Server.Ox_fast_mcp.normalize_tool_name name in
  print_endline normalized;
  [%expect {| _mytool |}];
  return ()

let%expect_test "normalize_tool_name_empty_string" =
  let name = "" in
  let normalized = Server.Ox_fast_mcp.normalize_tool_name name in
  print_endline normalized;
  [%expect {| _ |}];
  return ()

(** {1 Batch Operation Tests} *)

let%expect_test "add_tools_multiple" =
  let server = Server.Ox_fast_mcp.create ~name:"test" () in
  let tool1 =
    Server.Tool.create ~name:"tool1"
      ~handler:(fun _ -> return (`String "ok"))
      ()
  in
  let tool2 =
    Server.Tool.create ~name:"tool2"
      ~handler:(fun _ -> return (`String "ok"))
      ()
  in
  let tool3 =
    Server.Tool.create ~name:"tool3"
      ~handler:(fun _ -> return (`String "ok"))
      ()
  in
  Server.Ox_fast_mcp.add_tools server [ tool1; tool2; tool3 ];
  let tools = Server.Ox_fast_mcp.get_tools server in
  print_s [%sexp (Hashtbl.length tools : int)];
  [%expect {| 3 |}];
  return ()

let%expect_test "add_resources_multiple" =
  let server = Server.Ox_fast_mcp.create ~name:"test" () in
  let resource1 =
    Server.Resource.create ~uri:"file:///test1" ~name:"r1"
      ~reader:(fun () -> return "content1")
      ()
  in
  let resource2 =
    Server.Resource.create ~uri:"file:///test2" ~name:"r2"
      ~reader:(fun () -> return "content2")
      ()
  in
  Server.Ox_fast_mcp.add_resources server [ resource1; resource2 ];
  let resources = Server.Ox_fast_mcp.get_resources server in
  print_s [%sexp (Hashtbl.length resources : int)];
  [%expect {| 2 |}];
  return ()

let%expect_test "add_prompts_multiple" =
  let server = Server.Ox_fast_mcp.create ~name:"test" () in
  let prompt1 =
    Server.Prompt.create ~name:"prompt1"
      ~render:(fun _ -> return (`String "rendered"))
      ()
  in
  let prompt2 =
    Server.Prompt.create ~name:"prompt2"
      ~render:(fun _ -> return (`String "rendered"))
      ()
  in
  Server.Ox_fast_mcp.add_prompts server [ prompt1; prompt2 ];
  let prompts = Server.Ox_fast_mcp.get_prompts server in
  print_s [%sexp (Hashtbl.length prompts : int)];
  [%expect {| 2 |}];
  return ()

(** {1 Boolean Validation Helper Tests} *)

let%expect_test "is_valid_tool_name_valid_names" =
  let valid_names = [ "my_tool"; "tool123"; "_private_tool"; "ToolName" ] in
  List.iter valid_names ~f:(fun name ->
      let is_valid = Server.Ox_fast_mcp.is_valid_tool_name name in
      printf "%s: %b\n" name is_valid);
  [%expect
    {|
    my_tool: true
    tool123: true
    _private_tool: true
    ToolName: true |}];
  return ()

let%expect_test "is_valid_tool_name_invalid_names" =
  let invalid_names = [ "my tool"; ""; "tool-name"; "123tool" ] in
  List.iter invalid_names ~f:(fun name ->
      let is_valid = Server.Ox_fast_mcp.is_valid_tool_name name in
      printf "%s: %b\n" name is_valid);
  [%expect
    {|
    my tool: false
    : false
    tool-name: true
    123tool: true |}];
  return ()

let%expect_test "is_valid_uri_valid_uris" =
  let valid_uris =
    [
      "file:///path/to/file";
      "http://example.com";
      "https://example.com/resource";
    ]
  in
  List.iter valid_uris ~f:(fun uri ->
      let is_valid = Server.Ox_fast_mcp.is_valid_uri uri in
      printf "%s: %b\n" uri is_valid);
  [%expect
    {|
    file:///path/to/file: true
    http://example.com: true
    https://example.com/resource: true |}];
  return ()

let%expect_test "is_valid_uri_invalid_uris" =
  let invalid_uris = [ ""; "no_scheme"; "file:" ] in
  List.iter invalid_uris ~f:(fun uri ->
      let is_valid = Server.Ox_fast_mcp.is_valid_uri uri in
      printf "%s: %b\n" uri is_valid);
  [%expect {|
    : false
    no_scheme: false
    file:: false |}];
  return ()

let%expect_test "is_valid_prompt_name_valid" =
  let valid_names = [ "my_prompt"; "prompt123"; "HelloPrompt" ] in
  List.iter valid_names ~f:(fun name ->
      let is_valid = Server.Ox_fast_mcp.is_valid_prompt_name name in
      printf "%s: %b\n" name is_valid);
  [%expect {|
    my_prompt: true
    prompt123: true
    HelloPrompt: true |}];
  return ()

let%expect_test "is_valid_prompt_name_invalid" =
  let invalid_names = [ "my prompt"; "" ] in
  List.iter invalid_names ~f:(fun name ->
      let is_valid = Server.Ox_fast_mcp.is_valid_prompt_name name in
      printf "%s: %b\n" name is_valid);
  [%expect {|
    my prompt: false
    : false |}];
  return ()

let%expect_test "is_valid_template_uri_valid" =
  let valid_uris = [ "file:///{path}/file"; "http://example.com/{id}" ] in
  List.iter valid_uris ~f:(fun uri ->
      let is_valid = Server.Ox_fast_mcp.is_valid_template_uri uri in
      printf "%s: %b\n" uri is_valid);
  [%expect
    {|
    file:///{path}/file: true
    http://example.com/{id}: true |}];
  return ()

let%expect_test "is_valid_template_uri_invalid" =
  let invalid_uris = [ ""; "file:///no_param"; "no_scheme/{param}" ] in
  List.iter invalid_uris ~f:(fun uri ->
      let is_valid = Server.Ox_fast_mcp.is_valid_template_uri uri in
      printf "%s: %b\n" uri is_valid);
  [%expect
    {|
    : false
    file:///no_param: false
    no_scheme/{param}: false |}];
  return ()

(** {1 Integration Tests} *)

let%expect_test "normalize_then_validate" =
  (* Test that normalized names are valid *)
  let invalid_name = "My Tool!" in
  let normalized = Server.Ox_fast_mcp.normalize_tool_name invalid_name in
  let is_valid = Server.Ox_fast_mcp.is_valid_tool_name normalized in
  printf "Original: %s\n" invalid_name;
  printf "Normalized: %s\n" normalized;
  printf "Is valid: %b\n" is_valid;
  [%expect
    {|
    Original: My Tool!
    Normalized: my_tool_
    Is valid: true |}];
  return ()

let%expect_test "batch_add_then_list" =
  (* Test batch operations followed by listing *)
  let server = Server.Ox_fast_mcp.create ~name:"test" () in
  let tools =
    List.init 5 ~f:(fun i ->
        Server.Tool.create ~name:(sprintf "tool%d" i)
          ~handler:(fun _ -> return (`Int i))
          ())
  in
  Server.Ox_fast_mcp.add_tools server tools;
  let tool_list = Server.Ox_fast_mcp.list_tools_mcp server in
  print_s [%sexp (List.length tool_list : int)];
  [%expect {| 5 |}];
  return ()
