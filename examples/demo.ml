(** Comprehensive Demo MCP Server
    
    Demonstrates multiple OxFastMCP features including tools and resources.
    Run with: dune exec demo *)

open! Core
open! Async

let main () =
  let open Deferred.Let_syntax in
  
  (* Create server with metadata *)
  let server = Server.Ox_fast_mcp.create 
    ~name:"demo-server"
    ~version:"1.0.0"
    ~instructions:"A comprehensive demonstration of OxFastMCP features"
    ()
  in
  
  (* Tool examples *)
  Server.Ox_fast_mcp.add_simple_tool
    ~name:"greet"
    ~description:"Greet a person by name"
    ~handler:(fun params ->
      match params with
      | `Assoc fields ->
        let name = List.Assoc.find fields ~equal:String.equal "name" |> Option.value ~default:(`String "World") in
        (match name with
         | `String n -> return (`Assoc [("message", `String (sprintf "Hello, %s!" n))])
         | _ -> return (`Assoc [("message", `String "Hello, World!")]))
      | _ -> return (`Assoc [("message", `String "Hello, World!")]))
    server;
  
  Server.Ox_fast_mcp.add_simple_tool
    ~name:"echo"
    ~description:"Echo back the input"
    ~handler:(fun params -> return (`Assoc [("echoed", params)]))
    server;
  
  Server.Ox_fast_mcp.add_simple_tool
    ~name:"get_time"
    ~description:"Get current server time"
    ~handler:(fun _params ->
      let now = Time_ns.now () |> Time_ns.to_string in
      return (`Assoc [("time", `String now)]))
    server;
  
  (* Resource example *)
  Server.Ox_fast_mcp.add_simple_resource
    ~uri:"demo://info"
    ~name:"Server Info"
    ~description:"Information about this demo server"
    ~mime_type:"text/plain"
    ~reader:(fun () ->
      let info = sprintf "OxFastMCP Demo Server\nVersion: 1.0.0\nTools: %d" 
        (Hashtbl.length (Server.Ox_fast_mcp.get_tools server)) in
      return info)
    server;
  
  Log.Global.info "🚀 Demo server starting...";
  Log.Global.info "Tools: greet, echo, get_time";
  Log.Global.info "Resources: demo://info";
  
  (* Run with STDIO transport *)
  Server.Ox_fast_mcp.run_async server ~transport:Stdio ()

let () =
  Command.async
    ~summary:"Comprehensive demo MCP server showing multiple features"
    (Command.Param.return main)
  |> Command_unix.run
