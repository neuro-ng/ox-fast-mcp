(** In-memory transports for client-server communication.

    Provides bi-directional pipe infrastructure for running MCP client and
    server in the same process. Useful for testing and embedding. *)

open Core
open Async
module Message = Mcp_shared.Message

type message_stream =
  Message.session_message Pipe.Reader.t * Message.session_message Pipe.Writer.t
(** Stream type for bi-directional communication *)

(** Create paired bi-directional memory streams for client-server communication.

    Returns [(client_streams, server_streams)] where:
    - [client_streams] = (read from server, write to server)
    - [server_streams] = (read from client, write to client)

    Example:
    {[
      let client_streams, server_streams = create_client_server_memory_streams () in
      let client_read, client_write = client_streams in
      let server_read, server_write = server_streams in
      (* Start server on server_streams *)
      (* Connect client on client_streams *)
    ]} *)
let create_client_server_memory_streams () : message_stream * message_stream =
  (* Create two pipes for bidirectional communication *)
  (* Pipe 1: Server -> Client *)
  let server_to_client_read, server_to_client_write = Pipe.create () in
  (* Pipe 2: Client -> Server *)
  let client_to_server_read, client_to_server_write = Pipe.create () in

  (* Client reads from server, writes to server *)
  let client_streams = (server_to_client_read, client_to_server_write) in
  (* Server reads from client, writes to client *)
  let server_streams = (client_to_server_read, server_to_client_write) in

  (client_streams, server_streams)

(** Close all pipes in a stream pair *)
let close_streams (read_pipe, write_pipe) =
  Pipe.close_read read_pipe;
  Pipe.close write_pipe
