(** Tests for OxFastMCP Server Module *)

open! Core
module Ox_fast_mcp = Server.Ox_fast_mcp
module Tool = Server.Tool
module Resource = Server.Resource
module Prompt = Server.Prompt
module Transport = Server.Transport
module Duplicate_behavior = Server.Duplicate_behavior

(* Helper to create a sync handler for testing *)
let sync_handler result _args = Async.return result

(** {1 Test: Server Creation} *)

let%expect_test "create server with default name" =
  let server = Ox_fast_mcp.create () in
  let name = Ox_fast_mcp.name server in
  (* Name should start with OxFastMCP- *)
  print_s [%sexp (String.is_prefix name ~prefix:"OxFastMCP-" : bool)];
  [%expect {| true |}]

let%expect_test "create server with custom name" =
  let server = Ox_fast_mcp.create ~name:"test-server" () in
  print_s [%sexp (Ox_fast_mcp.name server : string)];
  [%expect {| test-server |}]

let%expect_test "create server with instructions" =
  let server =
    Ox_fast_mcp.create ~name:"test" ~instructions:"Server instructions" ()
  in
  print_s [%sexp (Ox_fast_mcp.instructions server : string option)];
  [%expect {| ("Server instructions") |}]

let%expect_test "create server with version" =
  let server = Ox_fast_mcp.create ~name:"test" ~version:"1.0.0" () in
  print_s [%sexp (Ox_fast_mcp.version server : string option)];
  [%expect {| (1.0.0) |}]

(** {1 Test: Tool Management} *)

let%expect_test "add and get tool" =
  let server = Ox_fast_mcp.create ~name:"test" () in
  let tool =
    Tool.create ~name:"add" ~description:"Add two numbers"
      ~handler:(sync_handler (`Int 3))
      ()
  in
  Ox_fast_mcp.add_tool server tool;
  let tools = Ox_fast_mcp.get_tools server in
  print_s [%sexp (Hashtbl.length tools : int)];
  print_s [%sexp (Hashtbl.mem tools "add" : bool)];
  [%expect {|
    1
    true |}]

let%expect_test "remove tool" =
  let server = Ox_fast_mcp.create ~name:"test" () in
  let tool = Tool.create ~name:"add" ~handler:(sync_handler (`Int 0)) () in
  Ox_fast_mcp.add_tool server tool;
  Ox_fast_mcp.remove_tool server ~name:"add";
  let tools = Ox_fast_mcp.get_tools server in
  print_s [%sexp (Hashtbl.length tools : int)];
  [%expect {| 0 |}]

let%expect_test "list tools mcp format" =
  let server = Ox_fast_mcp.create ~name:"test" () in
  let tool =
    Tool.create ~name:"greet" ~description:"Greet someone"
      ~handler:(sync_handler (`String "hello"))
      ()
  in
  Ox_fast_mcp.add_tool server tool;
  let mcp_tools = Ox_fast_mcp.list_tools_mcp server in
  print_s [%sexp (List.length mcp_tools : int)];
  [%expect {| 1 |}]

(** {1 Test: Resource Management} *)

let%expect_test "add and get resource" =
  let server = Ox_fast_mcp.create ~name:"test" () in
  let resource =
    Resource.create ~uri:"resource://data" ~name:"data"
      ~description:"Data resource"
      ~reader:(fun () -> Async.return "Hello, world!")
      ()
  in
  Ox_fast_mcp.add_resource server resource;
  let resources = Ox_fast_mcp.get_resources server in
  print_s [%sexp (Hashtbl.length resources : int)];
  print_s [%sexp (Hashtbl.mem resources "resource://data" : bool)];
  [%expect {|
    1
    false
    |}]

let%expect_test "list resources mcp format" =
  let server = Ox_fast_mcp.create ~name:"test" () in
  let resource =
    Resource.create ~uri:"resource://data" ~name:"data"
      ~reader:(fun () -> Async.return "content")
      ()
  in
  Ox_fast_mcp.add_resource server resource;
  let mcp_resources = Ox_fast_mcp.list_resources_mcp server in
  print_s [%sexp (List.length mcp_resources : int)];
  [%expect {| 1 |}]

(** {1 Test: Prompt Management} *)

let%expect_test "add and get prompt" =
  let server = Ox_fast_mcp.create ~name:"test" () in
  let prompt =
    Prompt.create ~name:"greet" ~description:"Greeting prompt"
      ~render:(sync_handler (`String "Hello!"))
      ()
  in
  Ox_fast_mcp.add_prompt server prompt;
  let prompts = Ox_fast_mcp.get_prompts server in
  print_s [%sexp (Hashtbl.length prompts : int)];
  print_s [%sexp (Hashtbl.mem prompts "greet" : bool)];
  [%expect {|
    1
    true |}]

(** {1 Test: Tag Filtering} *)

let%expect_test "filter tools by include tags" =
  let server =
    Ox_fast_mcp.create ~name:"test"
      ~include_tags:(String.Set.of_list [ "enabled" ])
      ()
  in
  let tool1 =
    Tool.create ~name:"enabled_tool"
      ~tags:(String.Set.of_list [ "enabled" ])
      ~handler:(sync_handler `Null) ()
  in
  let tool2 =
    Tool.create ~name:"disabled_tool"
      ~tags:(String.Set.of_list [ "disabled" ])
      ~handler:(sync_handler `Null) ()
  in
  Ox_fast_mcp.add_tool server tool1;
  Ox_fast_mcp.add_tool server tool2;
  let tools = Ox_fast_mcp.get_tools server in
  print_s [%sexp (Hashtbl.length tools : int)];
  print_s [%sexp (Hashtbl.mem tools "enabled_tool" : bool)];
  print_s [%sexp (Hashtbl.mem tools "disabled_tool" : bool)];
  [%expect {|
    1
    true
    false |}]

let%expect_test "filter tools by exclude tags" =
  let server =
    Ox_fast_mcp.create ~name:"test"
      ~exclude_tags:(String.Set.of_list [ "excluded" ])
      ()
  in
  let tool1 =
    Tool.create ~name:"included_tool"
      ~tags:(String.Set.of_list [ "ok" ])
      ~handler:(sync_handler `Null) ()
  in
  let tool2 =
    Tool.create ~name:"excluded_tool"
      ~tags:(String.Set.of_list [ "excluded" ])
      ~handler:(sync_handler `Null) ()
  in
  Ox_fast_mcp.add_tool server tool1;
  Ox_fast_mcp.add_tool server tool2;
  let tools = Ox_fast_mcp.get_tools server in
  print_s [%sexp (Hashtbl.length tools : int)];
  print_s [%sexp (Hashtbl.mem tools "included_tool" : bool)];
  print_s [%sexp (Hashtbl.mem tools "excluded_tool" : bool)];
  [%expect {|
    1
    true
    false |}]

(** {1 Test: Duplicate Behavior} *)

let%expect_test "duplicate tool with replace behavior" =
  let server =
    Ox_fast_mcp.create ~name:"test"
      ~on_duplicate_tools:Duplicate_behavior.Replace ()
  in
  let tool1 =
    Tool.create ~name:"tool" ~description:"First"
      ~handler:(sync_handler (`Int 1))
      ()
  in
  let tool2 =
    Tool.create ~name:"tool" ~description:"Second"
      ~handler:(sync_handler (`Int 2))
      ()
  in
  Ox_fast_mcp.add_tool server tool1;
  Ox_fast_mcp.add_tool server tool2;
  let tools = Ox_fast_mcp.get_tools server in
  let tool = Hashtbl.find_exn tools "tool" in
  print_s [%sexp (tool.description : string option)];
  [%expect {| (Second) |}]

let%expect_test "duplicate tool with ignore behavior" =
  let server =
    Ox_fast_mcp.create ~name:"test"
      ~on_duplicate_tools:Duplicate_behavior.Ignore ()
  in
  let tool1 =
    Tool.create ~name:"tool" ~description:"First"
      ~handler:(sync_handler (`Int 1))
      ()
  in
  let tool2 =
    Tool.create ~name:"tool" ~description:"Second"
      ~handler:(sync_handler (`Int 2))
      ()
  in
  Ox_fast_mcp.add_tool server tool1;
  Ox_fast_mcp.add_tool server tool2;
  let tools = Ox_fast_mcp.get_tools server in
  let tool = Hashtbl.find_exn tools "tool" in
  print_s [%sexp (tool.description : string option)];
  [%expect {| (First) |}]

(** {1 Test: Transport Types} *)

let%expect_test "transport to_string" =
  print_endline (Transport.to_string Transport.Stdio);
  print_endline (Transport.to_string Transport.Http);
  print_endline (Transport.to_string Transport.Sse);
  print_endline (Transport.to_string Transport.Streamable_http);
  [%expect {|
    Stdio
    Http
    Sse
    Streamable_http |}]

(** {1 Test: Helper Functions} *)

let%expect_test "add_resource_prefix protocol format" =
  let uri = "resource://data" in
  let result =
    Server.add_resource_prefix ~uri ~prefix:"mounted"
      ~format:Server.Resource_prefix_format.Protocol
  in
  print_endline result;
  [%expect {| resource://mounted/data |}]

let%expect_test "add_resource_prefix path format" =
  let uri = "resource://data" in
  let result =
    Server.add_resource_prefix ~uri ~prefix:"mounted"
      ~format:Server.Resource_prefix_format.Path
  in
  print_endline result;
  [%expect {| mounted/resource://data |}]

let%expect_test "has_resource_prefix" =
  let uri = "resource://mounted/data" in
  let result =
    Server.has_resource_prefix ~uri ~prefix:"mounted"
      ~format:Server.Resource_prefix_format.Protocol
  in
  print_s [%sexp (result : bool)];
  [%expect {| true |}]
