(** Demo script showing MCP client-server interaction *)

open Ox_fast_mcp

let demo_client_server () =
  Printf.printf "\n🚀 Ox Fast MCP Demo\n\n";

  (* Create a server *)
  let server = Fast_mcp_server.create ~name:"Demo MCP Server" () in

  (* Register tools *)
  Fast_mcp_server.register_tool server ~name:"greet" ~description:"Greet a user"
    ~func:(fun _params -> Mcp_types.String "Hello from MCP!");

  Fast_mcp_server.register_tool server ~name:"add" ~description:"Add numbers"
    ~func:(fun _params -> Mcp_types.String "42");

  (* Register a resource *)
  Fast_mcp_server.register_resource server ~uri:"demo://info" ~name:"Demo Info"
    ~description:"Demo information" ~func:(fun () ->
      Mcp_types.String "This is a demo MCP server");

  (* Register a prompt *)
  Fast_mcp_server.register_prompt server ~name:"hello"
    ~description:"Hello prompt" ~func:(fun _params -> "Say hello!");

  Printf.printf "📦 Server created: %s\n" (Fast_mcp_server.get_name server);
  Printf.printf "   Tools: %d\n" (Fast_mcp_server.tool_count server);
  Printf.printf "   Resources: %d\n" (Fast_mcp_server.resource_count server);
  Printf.printf "   Prompts: %d\n" (Fast_mcp_server.prompt_count server);

  (* Create a client *)
  let client = Mcp_client.create_in_memory server in
  Printf.printf "\n🔌 Client connected: %b\n" (Mcp_client.is_connected client);

  (* Test client operations *)
  Printf.printf "\n📋 Testing client operations:\n";

  (* List tools *)
  (match Mcp_client.list_tools client with
  | Ok tools ->
    Printf.printf "✅ Available tools: %s\n"
      (String.concat ", "
         (List.map (fun (t : Mcp_types.tool_def) -> t.name) tools))
  | Error err -> Printf.printf "❌ Failed to list tools: %s\n" err);

  (* List resources *)
  (match Mcp_client.list_resources client with
  | Ok resources ->
    Printf.printf "✅ Available resources: %s\n"
      (String.concat ", "
         (List.map (fun (r : Mcp_types.resource_def) -> r.uri) resources))
  | Error err -> Printf.printf "❌ Failed to list resources: %s\n" err);

  (* Call a tool *)
  (match Mcp_client.call_tool client "greet" (Mcp_types.String "test") with
  | Ok (Mcp_types.Text result) ->
    Printf.printf "✅ Tool call result: %s\n" result
  | Error err -> Printf.printf "❌ Tool call failed: %s\n" err
  | _ -> Printf.printf "❌ Unexpected result format\n");

  (* Read a resource *)
  (match Mcp_client.read_resource client "demo://info" with
  | Ok (Mcp_types.Text content) ->
    Printf.printf "✅ Resource content: %s\n" content
  | Error err -> Printf.printf "❌ Resource read failed: %s\n" err
  | _ -> Printf.printf "❌ Unexpected content format\n");

  (* Get a prompt *)
  (match Mcp_client.get_prompt client "hello" (Mcp_types.String "test") with
  | Ok prompt -> Printf.printf "✅ Prompt: %s\n" prompt
  | Error err -> Printf.printf "❌ Prompt failed: %s\n" err);

  (* Close client *)
  Mcp_client.close client;
  Printf.printf "\n🔌 Client disconnected: %b\n"
    (not (Mcp_client.is_connected client));

  Printf.printf "\n✨ Demo completed successfully!\n"

let () = demo_client_server ()
