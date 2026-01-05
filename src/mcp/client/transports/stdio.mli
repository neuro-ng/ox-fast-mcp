(** Stdio transport for MCP client.

    This module provides a transport that spawns a server process and
    communicates with it over stdin/stdout using JSON-RPC messages. *)

open Async

(** Platform type for OS detection *)
type platform = Unix | Windows [@@deriving sexp, compare]

type encoding_error_handler =
  [ `Strict  (** Raise exception on encoding errors *)
  | `Ignore  (** Ignore encoding errors *)
  | `Replace  (** Replace invalid sequences with replacement character *) ]
[@@deriving sexp, compare]
(** Encoding error handling strategy *)

type stdio_server_parameters = {
  command : string;  (** The executable to run to start the server *)
  args : string list;  (** Command line arguments to pass to the executable *)
  env : (string * string) list option;
      (** Environment variables. If None, uses get_default_environment () *)
  cwd : string option;  (** Working directory for the process *)
  encoding : string;  (** Text encoding (default: "utf-8") *)
  encoding_error_handler : encoding_error_handler;
      (** How to handle encoding errors (default: `Strict) *)
}
[@@deriving sexp, compare]
(** Parameters for spawning a stdio server *)

val detect_platform : unit -> platform
(** Detect the current platform *)

val get_default_environment : unit -> (string * string) list
(** Get default environment variables for the current platform.

    Windows: APPDATA, HOMEDRIVE, HOMEPATH, LOCALAPPDATA, PATH, PATHEXT,
    PROCESSOR_ARCHITECTURE, SYSTEMDRIVE, SYSTEMROOT, TEMP, USERNAME, USERPROFILE

    Unix: HOME, LOGNAME, PATH, SHELL, TERM, USER *)

val stdio_client :
  stdio_server_parameters ->
  stderr:Writer.t ->
  (Mcp_shared.Message.session_message Pipe.Reader.t
  * Mcp_shared.Message.session_message Pipe.Writer.t)
  Deferred.t
(** Create a stdio client connection to a server.

    Spawns a server process and returns pipes for reading SessionMessages from
    the server and writing SessionMessages to the server.

    The process will be spawned with:
    - stdin attached to our writer
    - stdout attached to our reader
    - stderr redirected to the provided Writer.t

    On cleanup, performs graceful shutdown:
    1. Close stdin to server
    2. Wait up to 2 seconds for server to exit
    3. Send SIGTERM if still running
    4. Send SIGKILL if still running after 2 more seconds

    @param params Server spawn parameters
    @param stderr Writer for server's stderr output
    @return
      (read_pipe, write_pipe) where:
      - read_pipe receives SessionMessages from server
      - write_pipe sends SessionMessages to server *)
