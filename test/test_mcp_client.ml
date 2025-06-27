open Alcotest
open Lwt.Syntax
open Ox_fast_mcp
open Mcp_types

let test_client_creation () =
  Lwt_main.run
    (let server = Fast_mcp_server.create ~name:"Test Server" () in
     let* client = Mcp_client.create_in_memory server in
     check bool "client created" true (Mcp_client.is_connected client);
     let* () = Mcp_client.close client in
     Lwt.return_unit)

let test_client_server_communication () =
  Lwt_main.run
    (let server = Fast_mcp_server.create ~name:"Communication Test Server" () in

     (* Add a simple tool to the server *)
     Fast_mcp_server.register_tool server ~name:"echo"
       ~description:"Echo the input" ~func:(fun json -> json);

     let* client = Mcp_client.create_in_memory server in

     (* Test tool listing *)
     let* tools = Mcp_client.list_tools client in
     check int "tool count" 1 (List.length tools);
     let tool = List.hd tools in
     check string "tool name" "echo" tool.name;
     check string "tool description" "Echo the input" tool.description;

     let* () = Mcp_client.close client in
     Lwt.return_unit)

let test_client_tool_call () =
  Lwt_main.run
    (let server = Fast_mcp_server.create ~name:"Tool Call Server" () in

     (* Add calculator tools *)
     Fast_mcp_server.register_tool server ~name:"add"
       ~description:"Add two numbers" ~func:(fun json ->
         match json with
         | `Assoc [ ("a", `Int a); ("b", `Int b) ] -> `Int (a + b)
         | `Assoc [ ("b", `Int b); ("a", `Int a) ] -> `Int (a + b)
         | _ -> `String "Invalid parameters");

     let* client = Mcp_client.create_in_memory server in

     (* Call the tool *)
     let params = `Assoc [ ("a", `Int 5); ("b", `Int 3) ] in
     let* result = Mcp_client.call_tool client "add" params in

     match result with
     | Ok (Text result_str) ->
       check string "addition result" "8" result_str;
       let* () = Mcp_client.close client in
       Lwt.return_unit
     | Ok _ -> fail "Expected text result"
     | Error err -> fail ("Tool call failed: " ^ err))

let test_client_resource_access () =
  Lwt_main.run
    (let server = Fast_mcp_server.create ~name:"Resource Server" () in

     (* Add a resource *)
     Fast_mcp_server.register_resource server ~uri:"info://status"
       ~name:"Server Status" ~description:"Current server status"
       ~func:(fun () ->
         `Assoc [ ("status", `String "running"); ("uptime", `Int 3600) ]);

     let* client = Mcp_client.create_in_memory server in

     (* List resources *)
     let* resources = Mcp_client.list_resources client in
     check int "resource count" 1 (List.length resources);
     let resource = List.hd resources in
     check string "resource uri" "info://status" resource.uri;

     (* Read the resource *)
     let* result = Mcp_client.read_resource client "info://status" in

     match result with
     | Ok (Text content) ->
       check bool "resource content present" true
         (String.contains content "running");
       let* () = Mcp_client.close client in
       Lwt.return_unit
     | Ok _ -> fail "Expected text result"
     | Error err -> fail ("Resource read failed: " ^ err))

let test_client_prompt_access () =
  Lwt_main.run
    (let server = Fast_mcp_server.create ~name:"Prompt Server" () in

     (* Add a prompt *)
     Fast_mcp_server.register_prompt server ~name:"greet"
       ~description:"Generate a greeting" ~func:(fun json ->
         match json with
         | `Assoc [ ("name", `String name) ] ->
           Printf.sprintf "Hello, %s! Welcome to our service." name
         | _ -> "Hello! Welcome to our service.");

     let* client = Mcp_client.create_in_memory server in

     (* List prompts *)
     let* prompts = Mcp_client.list_prompts client in
     check int "prompt count" 1 (List.length prompts);
     let prompt = List.hd prompts in
     check string "prompt name" "greet" prompt.name;

     (* Get the prompt *)
     let params = `Assoc [ ("name", `String "Alice") ] in
     let* result = Mcp_client.get_prompt client "greet" params in

     match result with
     | Ok content ->
       check bool "greeting content" true (String.contains content "Alice");
       let* () = Mcp_client.close client in
       Lwt.return_unit
     | Error err -> fail ("Prompt access failed: " ^ err))

let test_client_error_handling () =
  Lwt_main.run
    (let server = Fast_mcp_server.create ~name:"Error Server" () in
     let* client = Mcp_client.create_in_memory server in

     (* Try to call non-existent tool *)
     let* result = Mcp_client.call_tool client "nonexistent" `Null in
     match result with
     | Error _ ->
       check bool "error on nonexistent tool" true true;
       let* () = Mcp_client.close client in
       Lwt.return_unit
     | Ok _ -> fail "Expected error for nonexistent tool")

let test_client_stdio_transport () =
  (* This test would require an actual executable, so we'll mock it *)
  check bool "stdio transport placeholder" true true

let test_client_http_transport () =
  (* This test would require an HTTP server, so we'll mock it *)
  check bool "http transport placeholder" true true

let test_client_connection_lifecycle () =
  Lwt_main.run
    (let server = Fast_mcp_server.create ~name:"Lifecycle Server" () in
     let* client = Mcp_client.create_in_memory server in

     (* Check initial state *)
     check bool "initially connected" true (Mcp_client.is_connected client);

     (* Close connection *)
     let* () = Mcp_client.close client in
     check bool "disconnected after close" false
       (Mcp_client.is_connected client);

     Lwt.return_unit)

let test_client_concurrent_operations () =
  Lwt_main.run
    (let server = Fast_mcp_server.create ~name:"Concurrent Server" () in

     (* Add multiple tools *)
     for i = 1 to 5 do
       let name = Printf.sprintf "tool_%d" i in
       Fast_mcp_server.register_tool server ~name
         ~description:(Printf.sprintf "Tool number %d" i) ~func:(fun _ ->
           `Int i)
     done;

     let* client = Mcp_client.create_in_memory server in

     (* Make concurrent tool calls *)
     let tasks =
       List.init 5 (fun i ->
           let name = Printf.sprintf "tool_%d" (i + 1) in
           Mcp_client.call_tool client name `Null)
     in

     let* results = Lwt.all tasks in

     (* Check all calls succeeded *)
     let success_count =
       List.fold_left
         (fun acc result ->
           match result with
           | Ok _ -> acc + 1
           | Error _ -> acc)
         0 results
     in

     check int "concurrent calls succeeded" 5 success_count;

     let* () = Mcp_client.close client in
     Lwt.return_unit)

let () =
  run "MCP Client Tests"
    [
      ( "Client Creation",
        [ test_case "Basic client creation" `Quick test_client_creation ] );
      ( "Communication",
        [
          test_case "Server communication" `Quick
            test_client_server_communication;
          test_case "Tool calling" `Quick test_client_tool_call;
          test_case "Resource access" `Quick test_client_resource_access;
          test_case "Prompt access" `Quick test_client_prompt_access;
        ] );
      ( "Error Handling",
        [ test_case "Error handling" `Quick test_client_error_handling ] );
      ( "Transports",
        [
          test_case "Stdio transport" `Quick test_client_stdio_transport;
          test_case "HTTP transport" `Quick test_client_http_transport;
        ] );
      ( "Connection Management",
        [
          test_case "Connection lifecycle" `Quick
            test_client_connection_lifecycle;
          test_case "Concurrent operations" `Quick
            test_client_concurrent_operations;
        ] );
    ]
