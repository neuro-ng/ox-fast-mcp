open Alcotest
open Lwt.Syntax
open Utilities.Inspect
open Fastmcp.Core

(** Test FastMCPInfo record creation *)
let test_fastmcp_info_creation () =
  let tool = {
    key = "tool1";
    name = "tool1";
    description = "Test tool";
    input_schema = `Assoc [];
  } in
  let info = {
    name = "TestServer";
    instructions = Some "Test instructions";
    fastmcp_version = "1.0.0";
    mcp_version = "1.0.0";
    server_version = "1.0.0";
    tools = [tool];
    prompts = [];
    resources = [];
    templates = [];
    capabilities = `Assoc [("tools", `Assoc [("listChanged", `Bool true)])];
  } in
  
  check string "name" "TestServer" info.name;
  check (option string) "instructions" (Some "Test instructions") info.instructions;
  check string "fastmcp_version" "1.0.0" info.fastmcp_version;
  check string "mcp_version" "1.0.0" info.mcp_version;
  check string "server_version" "1.0.0" info.server_version;
  check int "tools count" 1 (List.length info.tools);
  check string "tool name" "tool1" (List.hd info.tools).name

(** Test FastMCPInfo with None instructions *)
let test_fastmcp_info_with_none_instructions () =
  let info = {
    name = "TestServer";
    instructions = None;
    fastmcp_version = "1.0.0";
    mcp_version = "1.0.0";
    server_version = "1.0.0";
    tools = [];
    prompts = [];
    resources = [];
    templates = [];
    capabilities = `Assoc [];
  } in
  
  check (option string) "instructions is None" None info.instructions

(** Test inspect_fastmcp with an empty server *)
let test_empty_server () =
  let%lwt mcp = FastMCP.create ~name:"EmptyServer" ~instructions:"Empty server for testing" () in
  let%lwt info = inspect_fastmcp mcp in
  
  check string "name" "EmptyServer" info.name;
  check (option string) "instructions" (Some "Empty server for testing") info.instructions;
  check string "fastmcp_version" Fastmcp.Version.current info.fastmcp_version;
  check bool "mcp_version not empty" true (String.length info.mcp_version > 0);
  check string "server_version" Fastmcp.Version.current info.server_version;
  check int "tools empty" 0 (List.length info.tools);
  check int "prompts empty" 0 (List.length info.prompts);
  check int "resources empty" 0 (List.length info.resources);
  check int "templates empty" 0 (List.length info.templates);
  
  (* Check capabilities *)
  let has_capability key =
    match info.capabilities with
    | `Assoc caps -> List.mem_assoc key caps
    | _ -> false
  in
  check bool "has tools capability" true (has_capability "tools");
  check bool "has resources capability" true (has_capability "resources");
  check bool "has prompts capability" true (has_capability "prompts");
  check bool "has logging capability" true (has_capability "logging");
  
  Lwt.return_unit

(** Test inspect_fastmcp with a server that has tools *)
let test_server_with_tools () =
  let%lwt mcp = FastMCP.create ~name:"ToolServer" () in
  
  (* Add tools *)
  let add_numbers_tool = Tool.create
    ~name:"add_numbers"
    ~description:"Add two numbers"
    ~handler:(fun args ->
      let a = Yojson.Safe.Util.member "a" args |> Yojson.Safe.Util.to_int in
      let b = Yojson.Safe.Util.member "b" args |> Yojson.Safe.Util.to_int in
      Lwt.return (`String (string_of_int (a + b)))
    )
    ~input_schema:(`Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("a", `Assoc [("type", `String "integer")]);
        ("b", `Assoc [("type", `String "integer")])
      ])
    ])
    ()
  in
  
  let greet_tool = Tool.create
    ~name:"greet"
    ~description:"Greet someone"
    ~handler:(fun args ->
      let name = Yojson.Safe.Util.member "name" args |> Yojson.Safe.Util.to_string in
      Lwt.return (`String ("Hello, " ^ name ^ "!"))
    )
    ~input_schema:(`Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("name", `Assoc [("type", `String "string")])
      ])
    ])
    ()
  in
  
  FastMCP.add_tool mcp add_numbers_tool;
  FastMCP.add_tool mcp greet_tool;
  
  let%lwt info = inspect_fastmcp mcp in
  
  check string "name" "ToolServer" info.name;
  check int "tools count" 2 (List.length info.tools);
  
  let tool_names = List.map (fun tool -> tool.name) info.tools in
  check bool "has add_numbers" true (List.mem "add_numbers" tool_names);
  check bool "has greet" true (List.mem "greet" tool_names);
  
  Lwt.return_unit

(** Test inspect_fastmcp with a server that has resources *)
let test_server_with_resources () =
  let%lwt mcp = FastMCP.create ~name:"ResourceServer" () in
  
  (* Add static resource *)
  let static_resource = Resource.create
    ~uri:"resource://static"
    ~handler:(fun () -> Lwt.return (Text "Static data"))
    ()
  in
  
  (* Add dynamic resource template *)
  let dynamic_template = ResourceTemplate.create
    ~uri_template:"resource://dynamic/{param}"
    ~handler:(fun params ->
      let param = List.assoc "param" params in
      Lwt.return (Text ("Dynamic data: " ^ param))
    )
    ()
  in
  
  FastMCP.add_resource mcp static_resource;
  FastMCP.add_resource_template mcp dynamic_template;
  
  let%lwt info = inspect_fastmcp mcp in
  
  check string "name" "ResourceServer" info.name;
  check int "resources count" 1 (List.length info.resources);
  check int "templates count" 1 (List.length info.templates);
  
  let resource_uris = List.map (fun res -> res.uri) info.resources in
  let template_uris = List.map (fun tmpl -> tmpl.uri_template) info.templates in
  check bool "has static resource" true (List.mem "resource://static" resource_uris);
  check bool "has dynamic template" true (List.mem "resource://dynamic/{param}" template_uris);
  
  Lwt.return_unit

(** Test inspect_fastmcp with a server that has prompts *)
let test_server_with_prompts () =
  let%lwt mcp = FastMCP.create ~name:"PromptServer" () in
  
  (* Add prompts *)
  let analyze_prompt = Prompt.create
    ~name:"analyze_data"
    ~description:"Analyze data"
    ~handler:(fun args ->
      let data = Yojson.Safe.Util.member "data" args |> Yojson.Safe.Util.to_string in
      Lwt.return [
        `Assoc [("role", `String "user"); ("content", `String ("Analyze: " ^ data))]
      ]
    )
    ~arguments:[
      { name = "data"; description = Some "Data to analyze"; required = true }
    ]
    ()
  in
  
  let custom_prompt = Prompt.create
    ~name:"custom_prompt"
    ~description:"Custom analysis"
    ~handler:(fun args ->
      let text = Yojson.Safe.Util.member "text" args |> Yojson.Safe.Util.to_string in
      Lwt.return [
        `Assoc [("role", `String "user"); ("content", `String ("Custom: " ^ text))]
      ]
    )
    ~arguments:[
      { name = "text"; description = Some "Text to analyze"; required = true }
    ]
    ()
  in
  
  FastMCP.add_prompt mcp analyze_prompt;
  FastMCP.add_prompt mcp custom_prompt;
  
  let%lwt info = inspect_fastmcp mcp in
  
  check string "name" "PromptServer" info.name;
  check int "prompts count" 2 (List.length info.prompts);
  
  let prompt_names = List.map (fun prompt -> prompt.name) info.prompts in
  check bool "has analyze_data" true (List.mem "analyze_data" prompt_names);
  check bool "has custom_prompt" true (List.mem "custom_prompt" prompt_names);
  
  Lwt.return_unit

(** Test inspect_fastmcp with a comprehensive server *)
let test_comprehensive_server () =
  let%lwt mcp = FastMCP.create ~name:"ComprehensiveServer" ~instructions:"A server with everything" () in
  
  (* Add a tool *)
  let calculate_tool = Tool.create
    ~name:"calculate"
    ~description:"Calculate multiplication"
    ~handler:(fun args ->
      let x = Yojson.Safe.Util.member "x" args |> Yojson.Safe.Util.to_int in
      let y = Yojson.Safe.Util.member "y" args |> Yojson.Safe.Util.to_int in
      Lwt.return (`String (string_of_int (x * y)))
    )
    ~input_schema:(`Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("x", `Assoc [("type", `String "integer")]);
        ("y", `Assoc [("type", `String "integer")])
      ])
    ])
    ()
  in
  
  (* Add a resource *)
  let data_resource = Resource.create
    ~uri:"resource://data"
    ~handler:(fun () -> Lwt.return (Text "Some data"))
    ()
  in
  
  (* Add a template *)
  let item_template = ResourceTemplate.create
    ~uri_template:"resource://item/{id}"
    ~handler:(fun params ->
      let id = List.assoc "id" params in
      Lwt.return (Text ("Item " ^ id))
    )
    ()
  in
  
  (* Add a prompt *)
  let analyze_prompt = Prompt.create
    ~name:"analyze"
    ~description:"Analyze content"
    ~handler:(fun args ->
      let content = Yojson.Safe.Util.member "content" args |> Yojson.Safe.Util.to_string in
      Lwt.return [
        `Assoc [("role", `String "user"); ("content", `String content)]
      ]
    )
    ~arguments:[
      { name = "content"; description = Some "Content to analyze"; required = true }
    ]
    ()
  in
  
  FastMCP.add_tool mcp calculate_tool;
  FastMCP.add_resource mcp data_resource;
  FastMCP.add_resource_template mcp item_template;
  FastMCP.add_prompt mcp analyze_prompt;
  
  let%lwt info = inspect_fastmcp mcp in
  
  check string "name" "ComprehensiveServer" info.name;
  check (option string) "instructions" (Some "A server with everything") info.instructions;
  check string "fastmcp_version" Fastmcp.Version.current info.fastmcp_version;
  
  (* Check all components are present *)
  check int "tools count" 1 (List.length info.tools);
  let tool_names = List.map (fun tool -> tool.name) info.tools in
  check bool "has calculate" true (List.mem "calculate" tool_names);
  
  check int "resources count" 1 (List.length info.resources);
  let resource_uris = List.map (fun res -> res.uri) info.resources in
  check bool "has data resource" true (List.mem "resource://data" resource_uris);
  
  check int "templates count" 1 (List.length info.templates);
  let template_uris = List.map (fun tmpl -> tmpl.uri_template) info.templates in
  check bool "has item template" true (List.mem "resource://item/{id}" template_uris);
  
  check int "prompts count" 1 (List.length info.prompts);
  let prompt_names = List.map (fun prompt -> prompt.name) info.prompts in
  check bool "has analyze" true (List.mem "analyze" prompt_names);
  
  (* Check capabilities *)
  let has_capability key =
    match info.capabilities with
    | `Assoc caps -> List.mem_assoc key caps
    | _ -> false
  in
  check bool "has tools capability" true (has_capability "tools");
  check bool "has resources capability" true (has_capability "resources");
  check bool "has prompts capability" true (has_capability "prompts");
  check bool "has logging capability" true (has_capability "logging");
  
  Lwt.return_unit

(** Test inspect_fastmcp with a server that has no instructions *)
let test_server_no_instructions () =
  let%lwt mcp = FastMCP.create ~name:"NoInstructionsServer" () in
  let%lwt info = inspect_fastmcp mcp in
  
  check string "name" "NoInstructionsServer" info.name;
  check (option string) "instructions is None" None info.instructions;
  
  Lwt.return_unit

(** Test server with client integration *)
let test_server_with_client_integration () =
  let%lwt mcp = FastMCP.create ~name:"IntegrationServer" () in
  
  (* Add components *)
  let test_tool = Tool.create
    ~name:"test_tool"
    ~description:"Test tool"
    ~handler:(fun _ -> Lwt.return (`String "test"))
    ~input_schema:(`Assoc [])
    ()
  in
  
  let test_resource = Resource.create
    ~uri:"resource://test"
    ~handler:(fun () -> Lwt.return (Text "test resource"))
    ()
  in
  
  let test_prompt = Prompt.create
    ~name:"test_prompt"
    ~description:"Test prompt"
    ~handler:(fun _ -> Lwt.return [
      `Assoc [("role", `String "user"); ("content", `String "test")]
    ])
    ~arguments:[]
    ()
  in
  
  FastMCP.add_tool mcp test_tool;
  FastMCP.add_resource mcp test_resource;
  FastMCP.add_prompt mcp test_prompt;
  
  (* Get info using our function *)
  let%lwt info = inspect_fastmcp mcp in
  
  (* Verify using client *)
  let%lwt client = Client.create mcp in
  let%lwt tools = Client.list_tools client in
  let%lwt resources = Client.list_resources client in
  let%lwt prompts = Client.list_prompts client in
  let%lwt () = Client.close client in
  
  check int "tools count matches" (List.length tools) (List.length info.tools);
  check int "resources count matches" (List.length resources) (List.length info.resources);
  check int "prompts count matches" (List.length prompts) (List.length info.prompts);
  
  check string "tool name matches" (List.hd tools).name (List.hd info.tools).name;
  check string "resource uri matches" (List.hd resources).uri (List.hd info.resources).uri;
  check string "prompt name matches" (List.hd prompts).name (List.hd info.prompts).name;
  
  Lwt.return_unit

(** Test FastMCP 1.x detection *)
let test_fastmcp1x_detection () =
  let%lwt mcp1x = FastMCP1x.create ~name:"Test1x" () in
  let%lwt mcp2x = FastMCP.create ~name:"Test2x" () in
  
  check bool "detects v1" true (is_fastmcp_v1 mcp1x);
  check bool "detects v2" false (is_fastmcp_v1 mcp2x);
  
  Lwt.return_unit

(** Test FastMCP 1.x empty server *)
let test_fastmcp1x_empty_server () =
  let%lwt mcp = FastMCP1x.create ~name:"Test1x" () in
  let%lwt info = inspect_fastmcp_v1 mcp in
  
  check string "name" "Test1x" info.name;
  check (option string) "instructions is None" None info.instructions;
  check string "fastmcp_version" Fastmcp.Version.current info.fastmcp_version;
  check bool "mcp_version not empty" true (String.length info.mcp_version > 0);
  check string "server_version" "1.0" info.server_version;
  check int "tools empty" 0 (List.length info.tools);
  check int "prompts empty" 0 (List.length info.prompts);
  check int "resources empty" 0 (List.length info.resources);
  check int "templates empty" 0 (List.length info.templates);
  
  let has_capability key =
    match info.capabilities with
    | `Assoc caps -> List.mem_assoc key caps
    | _ -> false
  in
  check bool "has tools capability" true (has_capability "tools");
  
  Lwt.return_unit

(** Test FastMCP 1.x with tools *)
let test_fastmcp1x_with_tools () =
  let%lwt mcp = FastMCP1x.create ~name:"Test1x" () in
  
  (* Add tools using v1 API *)
  let add_numbers_tool = Tool1x.create
    ~name:"add_numbers"
    ~description:"Add two numbers"
    ~handler:(fun args ->
      let a = Yojson.Safe.Util.member "a" args |> Yojson.Safe.Util.to_int in
      let b = Yojson.Safe.Util.member "b" args |> Yojson.Safe.Util.to_int in
      Lwt.return (`String (string_of_int (a + b)))
    )
    ()
  in
  
  let greet_tool = Tool1x.create
    ~name:"greet"
    ~description:"Greet someone"
    ~handler:(fun args ->
      let name = Yojson.Safe.Util.member "name" args |> Yojson.Safe.Util.to_string in
      Lwt.return (`String ("Hello, " ^ name ^ "!"))
    )
    ()
  in
  
  FastMCP1x.add_tool mcp add_numbers_tool;
  FastMCP1x.add_tool mcp greet_tool;
  
  let%lwt info = inspect_fastmcp_v1 mcp in
  
  check string "name" "Test1x" info.name;
  check int "tools count" 2 (List.length info.tools);
  
  let tool_names = List.map (fun tool -> tool.name) info.tools in
  check bool "has add_numbers" true (List.mem "add_numbers" tool_names);
  check bool "has greet" true (List.mem "greet" tool_names);
  
  Lwt.return_unit

(** Test dispatcher with FastMCP 1.x *)
let test_dispatcher_with_fastmcp1x () =
  let%lwt mcp = FastMCP1x.create ~name:"Test1x" () in
  
  let test_tool = Tool1x.create
    ~name:"test_tool"
    ~description:"Test tool"
    ~handler:(fun _ -> Lwt.return (`String "test"))
    ()
  in
  
  FastMCP1x.add_tool mcp test_tool;
  
  let%lwt info = inspect_fastmcp mcp in
  
  check string "name" "Test1x" info.name;
  check int "tools count" 1 (List.length info.tools);
  
  let tool_names = List.map (fun tool -> tool.name) info.tools in
  check bool "has test_tool" true (List.mem "test_tool" tool_names);
  check int "no templates" 0 (List.length info.templates);
  
  Lwt.return_unit

(** Test dispatcher with FastMCP 2.x *)
let test_dispatcher_with_fastmcp2x () =
  let%lwt mcp = FastMCP.create ~name:"Test2x" () in
  
  let test_tool = Tool.create
    ~name:"test_tool"
    ~description:"Test tool"
    ~handler:(fun _ -> Lwt.return (`String "test"))
    ~input_schema:(`Assoc [])
    ()
  in
  
  FastMCP.add_tool mcp test_tool;
  
  let%lwt info = inspect_fastmcp mcp in
  
  check string "name" "Test2x" info.name;
  check int "tools count" 1 (List.length info.tools);
  
  let tool_names = List.map (fun tool -> tool.name) info.tools in
  check bool "has test_tool" true (List.mem "test_tool" tool_names);
  
  Lwt.return_unit

(** Test FastMCP 1.x vs 2.x comparison *)
let test_fastmcp1x_vs_fastmcp2x_comparison () =
  let%lwt mcp1x = FastMCP1x.create ~name:"Test1x" () in
  let%lwt mcp2x = FastMCP.create ~name:"Test2x" () in
  
  (* Add tools to both *)
  let tool1x = Tool1x.create
    ~name:"tool1x"
    ~description:"1.x tool"
    ~handler:(fun _ -> Lwt.return (`String "1x"))
    ()
  in
  
  let tool2x = Tool.create
    ~name:"tool2x"
    ~description:"2.x tool"
    ~handler:(fun _ -> Lwt.return (`String "2x"))
    ~input_schema:(`Assoc [])
    ()
  in
  
  FastMCP1x.add_tool mcp1x tool1x;
  FastMCP.add_tool mcp2x tool2x;
  
  let%lwt info1x = inspect_fastmcp mcp1x in
  let%lwt info2x = inspect_fastmcp mcp2x in
  
  check string "1x name" "Test1x" info1x.name;
  check string "2x name" "Test2x" info2x.name;
  check int "1x tools count" 1 (List.length info1x.tools);
  check int "2x tools count" 1 (List.length info2x.tools);
  
  let tool1x_names = List.map (fun tool -> tool.name) info1x.tools in
  let tool2x_names = List.map (fun tool -> tool.name) info2x.tools in
  check bool "has tool1x" true (List.mem "tool1x" tool1x_names);
  check bool "has tool2x" true (List.mem "tool2x" tool2x_names);
  
  (* Check server versions *)
  check string "1x server version" "1.0" info1x.server_version;
  check string "2x server version" Fastmcp.Version.current info2x.server_version;
  
  (* No templates added in these tests *)
  check int "1x no templates" 0 (List.length info1x.templates);
  check int "2x no templates" 0 (List.length info2x.templates);
  
  Lwt.return_unit

(** Async test runner helper *)
let test_async name test_fn = test_case name `Quick (fun () -> Lwt_main.run (test_fn ()))

(** Main test suite *)
let () =
  run "Inspect Module Tests" [
    ("FastMCPInfo Data Structure", [
      test_case "FastMCPInfo creation" `Quick test_fastmcp_info_creation;
      test_case "FastMCPInfo with None instructions" `Quick test_fastmcp_info_with_none_instructions;
    ]);
    ("Inspect FastMCP Function", [
      test_async "Empty server" test_empty_server;
      test_async "Server with tools" test_server_with_tools;
      test_async "Server with resources" test_server_with_resources;
      test_async "Server with prompts" test_server_with_prompts;
      test_async "Comprehensive server" test_comprehensive_server;
      test_async "Server no instructions" test_server_no_instructions;
      test_async "Server with client integration" test_server_with_client_integration;
    ]);
    ("FastMCP 1.x Compatibility", [
      test_async "FastMCP 1.x detection" test_fastmcp1x_detection;
      test_async "FastMCP 1.x empty server" test_fastmcp1x_empty_server;
      test_async "FastMCP 1.x with tools" test_fastmcp1x_with_tools;
      test_async "Dispatcher with FastMCP 1.x" test_dispatcher_with_fastmcp1x;
      test_async "Dispatcher with FastMCP 2.x" test_dispatcher_with_fastmcp2x;
      test_async "FastMCP 1.x vs 2.x comparison" test_fastmcp1x_vs_fastmcp2x_comparison;
    ]);
  ] 