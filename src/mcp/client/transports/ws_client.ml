open Core
open Async
module Types = Mcp.Types

(** WebSocket client transport for OxFastMCP using websocket-async *)

type config = { url : string; subprotocol : string [@default "mcp"] }
[@@deriving sexp_of]

let create_config ~url ?(subprotocol = "mcp") () = { url; subprotocol }

(* Import Websocket library modules with explicit paths to avoid conflicts *)
module Ws_frame = Websocket.Frame
module Ws_async = Websocket_async

type t = {
  config : config;
  read_stream :
    [ `Message of Types.jsonrpc_message | `Error of Error.t ] Pipe.Reader.t;
  write_stream : Types.jsonrpc_message Pipe.Writer.t;
  mutable is_connected : bool;
  close_ivar : unit Ivar.t;
}

(** Parse WebSocket frame content as JSON-RPC message *)
let parse_json_rpc content =
  try
    let json = Yojson.Safe.from_string content in
    match Types.jsonrpc_message_of_yojson json with
    | msg -> Ok msg
    | exception exn ->
      Error
        (Error.create_s
           [%message "Failed to parse JSON-RPC" (content : string) (exn : exn)])
  with exn -> Error (Error.of_exn exn)

(** Serialize JSON-RPC message to string *)
let serialize_message msg =
  let json = Types.jsonrpc_message_to_yojson msg in
  Yojson.Safe.to_string json

(** Connect to WebSocket endpoint using websocket-async *)
let connect config =
  let read_reader, read_writer = Pipe.create () in
  let write_reader, write_writer = Pipe.create () in
  let close_ivar = Ivar.create () in

  let t =
    {
      config;
      read_stream = read_reader;
      write_stream = write_writer;
      is_connected = false;
      close_ivar;
    }
  in

  (* Parse URI and extract host/port *)
  let uri = Uri.of_string config.url in
  let host = Uri.host uri |> Option.value ~default:"localhost" in
  let port =
    match Uri.port uri with
    | Some p -> p
    | None -> (
      match Uri.scheme uri with
      | Some "wss" -> 443
      | _ -> 80)
  in
  let resource = Uri.path uri in

  don't_wait_for
    (Monitor.try_with (fun () ->
         Logs.info (fun m ->
             m "Connecting to WebSocket: %s:%d%s" host port resource);

         (* Establish TCP connection *)
         let%bind _socket, reader, writer =
           Tcp.connect (Tcp.Where_to_connect.of_host_and_port { host; port })
         in

         (* Create pipes for WebSocket protocol *)
         let app_to_ws_r, app_to_ws_w = Pipe.create () in
         let ws_to_app_r, ws_to_app_w = Pipe.create () in

         (* Build WebSocket client request with subprotocol *)
         let extra_headers =
           Cohttp.Header.of_list
             [ ("Sec-WebSocket-Protocol", config.subprotocol) ]
         in

         (* Start WebSocket protocol handler *)
         let%bind protocol_result =
           Ws_async.client ~extra_headers ~app_to_ws:app_to_ws_r
             ~ws_to_app:ws_to_app_w ~net_to_ws:reader ~ws_to_net:writer uri
         in

         match protocol_result with
         | Error e ->
           Logs.err (fun m ->
               m "WebSocket protocol error: %s" (Error.to_string_hum e));
           Pipe.write_without_pushback read_writer
             (`Error (Error.tag e ~tag:"WebSocket protocol failed"));
           return ()
         | Ok () ->
           t.is_connected <- true;
           Logs.info (fun m -> m "WebSocket connected successfully");

           (* Reader: ws_to_app -> read_stream (WebSocket frames to JSON-RPC
              messages) *)
           don't_wait_for
             ( Pipe.iter ws_to_app_r ~f:(fun frame ->
                   let open Ws_frame in
                   match frame.opcode with
                   | Opcode.Text -> (
                     match parse_json_rpc frame.content with
                     | Ok msg -> Pipe.write read_writer (`Message msg)
                     | Error e -> Pipe.write read_writer (`Error e))
                   | Opcode.Close ->
                     Logs.info (fun m -> m "WebSocket close frame received");
                     Pipe.close_read ws_to_app_r;
                     return ()
                   | Opcode.Binary ->
                     Pipe.write read_writer
                       (`Error
                         (Error.of_string
                            "Received binary frame (expected text)"))
                   | _ ->
                     Logs.debug (fun m ->
                         m "Ignoring WebSocket frame with opcode: %s"
                           (Opcode.to_string frame.opcode));
                     return ())
             >>| fun () ->
               Pipe.close read_writer;
               Pipe.close_read ws_to_app_r );

           (* Writer: write_stream -> app_to_ws (JSON-RPC messages to WebSocket
              frames) *)
           let%bind _close_reason =
             Deferred.choose
               [
                 Deferred.choice (Ivar.read close_ivar) (fun () -> `Close);
                 Deferred.choice
                   (Pipe.iter write_reader ~f:(fun msg ->
                        let json_str = serialize_message msg in
                        let frame =
                          Ws_frame.create ~opcode:Ws_frame.Opcode.Text
                            ~content:json_str ()
                        in
                        Pipe.write app_to_ws_w frame))
                   (fun () -> `Done);
               ]
           in

           (* Cleanup *)
           t.is_connected <- false;
           Pipe.close_read read_reader;
           Pipe.close write_writer;
           Pipe.close app_to_ws_w;
           Pipe.close_read ws_to_app_r;
           return ())
     >>| function
     | Ok () -> Logs.debug (fun m -> m "WebSocket connection closed cleanly")
     | Error exn ->
       Logs.err (fun m ->
           m "WebSocket connection error: %s" (Exn.to_string exn));
       t.is_connected <- false;
       Pipe.write_without_pushback read_writer (`Error (Error.of_exn exn));
       Pipe.close_read read_reader;
       Pipe.close write_writer);

  t

(** Close WebSocket connection *)
let close t =
  Logs.info (fun m -> m "Closing WebSocket connection");
  t.is_connected <- false;
  Ivar.fill_if_empty t.close_ivar ();
  Pipe.close_read t.read_stream;
  Pipe.close t.write_stream;
  return ()

(** Check if connected *)
let is_connected t = t.is_connected

(** Access read stream *)
let read_stream t = t.read_stream

(** Access write stream *)
let write_stream t = t.write_stream
