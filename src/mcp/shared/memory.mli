(** In-memory transports for client-server communication.

    Provides bi-directional pipe infrastructure for running MCP client and
    server in the same process. Useful for testing and embedding. *)

open Async
module Message = Mcp_shared.Message

type message_stream =
  Message.session_message Pipe.Reader.t * Message.session_message Pipe.Writer.t
(** Stream type for bi-directional communication *)

val create_client_server_memory_streams :
  unit -> message_stream * message_stream
(** Create paired bi-directional memory streams for client-server communication.

    Returns [(client_streams, server_streams)] where:
    - [client_streams] = (read from server, write to server)
    - [server_streams] = (read from client, write to client) *)

val close_streams : message_stream -> unit
(** Close all pipes in a stream pair *)
