(**
   Stdio Server Transport Module

   This module provides functionality for creating an stdio-based transport layer
   that can be used to communicate with an MCP client through standard input/output
   streams.
*)

(** The type representing a stdio server with read and write streams *)
type stdio_server = {
  read_stream: (Mcp.Shared.Message.session_message, [> `Error of exn ]) result Lwt_stream.t;
  write_stream: Mcp.Shared.Message.session_message -> unit Lwt.t;
}

(** Create a new stdio server with optional custom input/output channels *)
val create_stdio_server :
  ?stdin:Lwt_io.input_channel ->
  ?stdout:Lwt_io.output_channel ->
  unit ->
  stdio_server Lwt.t

(** Run a function with a stdio server, providing read and write streams.
    The server will be automatically cleaned up when the function completes. *)
val with_server :
  ?stdin:Lwt_io.input_channel ->
  ?stdout:Lwt_io.output_channel ->
  (read_stream:(Mcp.Shared.Message.session_message, [> `Error of exn ]) result Lwt_stream.t ->
   write_stream:(Mcp.Shared.Message.session_message -> unit Lwt.t) ->
   'a Lwt.t) ->
  'a Lwt.t 