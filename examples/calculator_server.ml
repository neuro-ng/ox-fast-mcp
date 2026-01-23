(** Calculator MCP Server Example
    
    A simple server demonstrating basic MCP tool usage with arithmetic operations.
    Run with: dune exec calculator-server *)

open! Core
open! Async

let extract_number json =
  match json with
  | `Float f -> f
  | `Int i -> Float.of_int i
  | _ -> 0.0

let main () =
  let open Deferred.Let_syntax in
  
  (* Create server *)
  let server = Server.Ox_fast_mcp.create 
    ~name:"calculator-server"
    ~version:"1.0.0"
    ~instructions:"A simple calculator with basic arithmetic operations"
    ()
  in
  
  (* Add arithmetic operation tools *)
  Server.Ox_fast_mcp.add_simple_tool
    ~name:"add"
    ~description:"Add two numbers together"
    ~handler:(fun params ->
      match params with
      | `Assoc fields ->
        let a = List.Assoc.find fields ~equal:String.equal "a" |> Option.value ~default:(`Float 0.0) |> extract_number in
        let b = List.Assoc.find fields ~equal:String.equal "b" |> Option.value ~default:(`Float 0.0) |> extract_number in
        return (`Assoc [("result", `Float (a +. b))])
      | _ -> return (`Assoc [("error", `String "Expected object with 'a' and 'b' fields")]))
    server;
  
  Server.Ox_fast_mcp.add_simple_tool
    ~name:"subtract"
    ~description:"Subtract b from a"
    ~handler:(fun params ->
      match params with
      | `Assoc fields ->
        let a = List.Assoc.find fields ~equal:String.equal "a" |> Option.value ~default:(`Float 0.0) |> extract_number in
        let b = List.Assoc.find fields ~equal:String.equal "b" |> Option.value ~default:(`Float 0.0) |> extract_number in
        return (`Assoc [("result", `Float (a -. b))])
      | _ -> return (`Assoc [("error", `String "Expected object")]))
    server;
  
  Server.Ox_fast_mcp.add_simple_tool
    ~name:"multiply"
    ~description:"Multiply two numbers"
    ~handler:(fun params ->
      match params with
      | `Assoc fields ->
        let a = List.Assoc.find fields ~equal:String.equal "a" |> Option.value ~default:(`Float 0.0) |> extract_number in
        let b = List.Assoc.find fields ~equal:String.equal "b" |> Option.value ~default:(`Float 0.0) |> extract_number in
        return (`Assoc [("result", `Float (a *. b))])
      | _ -> return (`Assoc [("error", `String "Expected object")]))
    server;
  
  Server.Ox_fast_mcp.add_simple_tool
    ~name:"divide"
    ~description:"Divide a by b (returns error if b is zero)"
    ~handler:(fun params ->
      match params with
      | `Assoc fields ->
        let a = List.Assoc.find fields ~equal:String.equal "a" |> Option.value ~default:(`Float 0.0) |> extract_number in
        let b = List.Assoc.find fields ~equal:String.equal "b" |> Option.value ~default:(`Float 1.0) |> extract_number in
        if Float.(abs b < 0.0001) then
          return (`Assoc [("error", `String "Division by zero")])
        else
          return (`Assoc [("result", `Float (a /. b))])
      | _ -> return (`Assoc [("error", `String "Expected object")]))
    server;
  
  Log.Global.info "🧮 Calculator server starting...";
  Log.Global.info "Available tools: add, subtract, multiply, divide";
  
  (* Run with STDIO transport *)
  Server.Ox_fast_mcp.run_async server ~transport:Stdio ()

let () =
  Command.async
    ~summary:"Calculator MCP server with basic arithmetic operations"
    (Command.Param.return main)
  |> Command_unix.run
