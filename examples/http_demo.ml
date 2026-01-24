(** Simple HTTP Server Example

    Demonstrates how to use OxFastMCP's HTTP server with SSE transport using the
    server's built-in run_async method. *)

open! Core
open! Async
module OxServer = Server.Ox_fast_mcp

let () =
  Command.async ~summary:"Start HTTP server with SSE transport"
    (Command.Param.return (fun () ->
         let open Deferred.Let_syntax in
         (* Create a server with some example tools *)
         let server = OxServer.create ~name:"demo-server" () in

         (* Add an example tool *)
         OxServer.add_simple_tool ~name:"greet" ~description:"Greet someone"
           ~handler:(fun params ->
             let name =
               match params with
               | `Assoc fields -> (
                 match List.Assoc.find fields ~equal:String.equal "name" with
                 | Some (`String n) -> n
                 | _ -> "World")
               | _ -> "World"
             in
             return
               (`Assoc
                 [ ("result", `String (sprintf "Hello, %s from SSE!" name)) ]))
           server;

         Log.Global.info "Starting HTTP server on http://127.0.0.1:8000";
         Log.Global.info "SSE endpoint will be at: http://127.0.0.1:8000/sse";
         Log.Global.info
           "Message endpoint will be at: http://127.0.0.1:8000/messages";

         (* Run server with SSE transport using built-in run_async *)
         OxServer.run_async server ~transport:Server.Transport.Sse
           ~host:"127.0.0.1" ~port:8000 ()))
  |> Command_unix.run
