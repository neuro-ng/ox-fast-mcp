(** Simple HTTP Server Example

    Demonstrates how to use OxFastMCP's HTTP server with SSE transport.
*)

open! Core
open! Async

let () =
  Command.async
    ~summary:"Start HTTP server with SSE transport"
    (Command.Param.return (fun () ->
         let open Deferred.Let_syntax in
         
         (* Create SSE app *)
         let app =
           Http.create_sse_app
             ~server:(`Assoc [("name", `String "demo-server")])
             ~message_path:"/messages"
             ~sse_path:"/sse"
             ()
         in
         
         (* Configure server *)
         let config = Http.Server_config.{
           host = "0.0.0.0";
           port = 8000;
           backlog = 10;
         } in
         
         let%bind () =
           Log.Global.info "Starting HTTP server on http://%s:%d" config.host config.port;
           Deferred.unit
         in
         
         (* Start server *)
         let%bind _server = Http.start_http_server ~config ~app () in
         
         let%bind () =
           Log.Global.info "Server running. SSE endpoint: http://%s:%d/sse" config.host config.port;
           Deferred.unit
         in
         
         (* Keep running *)
         Deferred.never ()))
  |> Command_unix.run
