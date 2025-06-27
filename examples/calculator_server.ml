open Ox_fast_mcp

(** Simple Calculator Server Example *)

let () =
  let server = Fast_mcp_server.create ~name:"Calculator Server" () in

  (* Register some tools *)
  Fast_mcp_server.register_tool server ~name:"add"
    ~description:"Add two numbers" ~func:(fun _json ->
      (* Simplified: just return the operation name *)
      Mcp_types.String "Addition result");

  Fast_mcp_server.register_resource server ~uri:"calc://version" ~name:"Version"
    ~description:"Calculator version" ~func:(fun () -> Mcp_types.String "1.0.0");

  Fast_mcp_server.register_prompt server ~name:"calculate"
    ~description:"Calculation prompt" ~func:(fun _json ->
      "Please provide numbers to calculate");

  Printf.printf "✅ Calculator server created successfully!\n";
  Printf.printf "   Name: %s\n" (Fast_mcp_server.get_name server);
  Printf.printf "   Tools: %d\n" (Fast_mcp_server.tool_count server);
  Printf.printf "   Resources: %d\n" (Fast_mcp_server.resource_count server);
  Printf.printf "   Prompts: %d\n" (Fast_mcp_server.prompt_count server)
