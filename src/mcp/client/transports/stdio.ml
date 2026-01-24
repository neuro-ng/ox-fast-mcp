(** Stdio transport for MCP client - Simplified Implementation

    This is a minimal viable implementation that provides basic stdio transport
    functionality. It uses Core_unix for process creation due to limitations in
    Async's Process API.

    Known limitations:
    - Does not implement full Windows Job Object support (uses standard process
      creation)
    - Process group termination is limited to sending signals to the process PID
    - Encoding/error handling is simplified (assumes UTF-8) *)

open Core
open Async

(** Platform type for OS detection *)
type platform = Unix | Windows [@@deriving sexp, compare]

type encoding_error_handler = [ `Strict | `Ignore | `Replace ]
[@@deriving sexp, compare]
(** Encoding error handling strategy *)

type stdio_server_parameters = {
  command : string;
  args : string list;
  env : (string * string) list option;
  cwd : string option;
  encoding : string;
  encoding_error_handler : encoding_error_handler;
}
[@@deriving sexp, compare]
(** Parameters for spawning a stdio server *)

(** Default environment variable names by platform *)
let default_env_vars_windows =
  [
    "APPDATA";
    "HOMEDRIVE";
    "HOMEPATH";
    "LOCALAPPDATA";
    "PATH";
    "PATH EXT";
    "PROCESSOR_ARCHITECTURE";
    "SYSTEMDRIVE";
    "SYSTEMROOT";
    "TEMP";
    "USERNAME";
    "USERPROFILE";
  ]

let default_env_vars_unix =
  [ "HOME"; "LOGNAME"; "PATH"; "SHELL"; "TERM"; "USER" ]

(** Timeout for process termination before force kill *)
let _process_termination_timeout = Time_float.Span.of_sec 2.0

(** Detect the current platform *)
let detect_platform () =
  match Sys.os_type with
  | "Win32" | "Cygwin" -> Windows
  | _ -> Unix

(** Get default environment variables for current platform *)
let get_default_environment () =
  let var_names =
    match detect_platform () with
    | Windows -> default_env_vars_windows
    | Unix -> default_env_vars_unix
  in
  List.filter_map var_names ~f:(fun key ->
      match Sys.getenv key with
      | Some value ->
        (* Skip shell functions (security risk) *)
        if String.is_prefix value ~prefix:"()" then None else Some (key, value)
      | None -> None)

(** Get platform-appropriate executable command *)
let _get_executable_command command =
  match detect_platform () with
  | Windows ->
    (* On Windows, may need to resolve .cmd, .bat, .exe extensions *)
    (* TODO: Implement Windows executable resolution *)
    command
  | Unix -> command

(** Spawn a process using Core_unix fork/exec with fd redirection *)
let spawn_process ~command ~args ~env ~cwd ~stderr_writer =
  let open Deferred.Let_syntax in
  (* Create pipes for stdin/stdout communication *)
  let stdin_read_fd, stdin_write_fd = Core_unix.pipe () in
  let stdout_read_fd, stdout_write_fd = Core_unix.pipe () in

  (* Prepare environment *)
  let env_array =
    match env with
    | Some env_list ->
      Array.of_list (List.map env_list ~f:(fun (k, v) -> k ^ "=" ^ v))
    | None -> Core_unix.environment ()
  in

  (* Fork and exec *)
  match Core_unix.fork () with
  | `In_the_child ->
    (* Child process: set up fd redirections and exec *)
    (* Redirect stdin *)
    Core_unix.dup2 ~src:stdin_read_fd ~dst:Core_unix.stdin ();
    Core_unix.close stdin_read_fd;
    Core_unix.close stdin_write_fd;
    (* Redirect stdout *)
    Core_unix.dup2 ~src:stdout_write_fd ~dst:Core_unix.stdout ();
    Core_unix.close stdout_read_fd;
    Core_unix.close stdout_write_fd;
    (* Redirect stderr *)
    let stderr_fd_unix = Fd.file_descr_exn (Writer.fd stderr_writer) in
    Core_unix.dup2 ~src:stderr_fd_unix ~dst:Core_unix.stderr ();
    (* Change directory if specified *)
    Option.iter cwd ~f:Core_unix.chdir;
    (* Exec the program *)
    (* Set environment variables - safe in child before exec *)
    let[@alert "-unsafe_multidomain"] () =
      Array.iter env_array ~f:(fun env_var ->
          match String.lsplit2 env_var ~on:'=' with
          | Some (key, value) -> Core_unix.putenv ~key ~data:value
          | None -> ())
    in
    (* Exec - this will replace the process image *)
    never_returns (Core_unix.exec ~prog:command ~argv:(command :: args) ())
  | `In_the_parent pid ->
    (* Parent process: close child's ends and convert to Async *)
    Core_unix.close stdin_read_fd;
    Core_unix.close stdout_write_fd;

    (* Convert fds to Async Reader/Writer *)
    let stdin_fd =
      Fd.create Fd.Kind.Fifo stdin_write_fd (Info.of_string "process stdin")
    in
    let stdout_fd =
      Fd.create Fd.Kind.Fifo stdout_read_fd (Info.of_string "process stdout")
    in

    let process_stdin = Writer.create stdin_fd in
    let process_stdout = Reader.create stdout_fd in

    return (pid, process_stdin, process_stdout)

(** Read lines from stdout and parse as JSON-RPC SessionMessages *)
let stdout_reader process_stdout read_pipe_writer =
  let open Deferred.Let_syntax in
  let rec read_loop () =
    match%bind Reader.read_line process_stdout with
    | `Ok line -> (
      try
        let json = Yojson.Safe.from_string line in
        let jsonrpc_msg = Mcp.Types.jsonrpc_message_of_yojson json in
        let session_msg =
          Mcp_shared.Message.{ message = jsonrpc_msg; metadata = None }
        in
        let%bind () = Pipe.write read_pipe_writer session_msg in
        read_loop ()
      with exn ->
        Logs.warn (fun m ->
            m "Failed to parse JSON-RPC message: %s (line: %s)"
              (Exn.to_string exn) line);
        read_loop ())
    | `Eof ->
      Pipe.close read_pipe_writer;
      return ()
  in
  read_loop ()

(** Write SessionMessages to stdin as JSON-RPC *)
let stdin_writer process_stdin write_pipe_reader =
  let open Deferred.Let_syntax in
  let rec write_loop () =
    match%bind Pipe.read write_pipe_reader with
    | `Ok session_msg ->
      let json =
        Mcp.Types.jsonrpc_message_to_yojson
          session_msg.Mcp_shared.Message.message
      in
      let json_str = Yojson.Safe.to_string json ^ "\n" in
      Writer.write process_stdin json_str;
      let%bind () = Writer.flushed process_stdin in
      write_loop ()
    | `Eof -> return ()
  in
  write_loop ()

(** Graceful shutdown with timeout-based SIGTERM/SIGKILL escalation *)
let graceful_shutdown pid stdin_writer =
  let open Deferred.Let_syntax in
  let timeout = _process_termination_timeout in

  (* 1. Close stdin to signal server to exit *)
  Logs.debug (fun m -> m "Closing stdin to signal server shutdown");
  let%bind () = Writer.close stdin_writer in

  (* 2. Wait with timeout for natural exit *)
  let wait_result = Clock.with_timeout timeout (Unix.waitpid pid) in
  match%bind wait_result with
  | `Result _ ->
    Logs.debug (fun m -> m "Process exited gracefully");
    return ()
  | `Timeout -> (
    (* 3. Send SIGTERM *)
    Logs.debug (fun m -> m "Process did not exit, sending SIGTERM");
    ignore (Signal_unix.send_i Signal.term (`Pid pid));

    let wait_result = Clock.with_timeout timeout (Unix.waitpid pid) in
    match%bind wait_result with
    | `Result _ ->
      Logs.debug (fun m -> m "Process exited after SIGTERM");
      return ()
    | `Timeout ->
      (* 4. Send SIGKILL *)
      Logs.debug (fun m -> m "Process did not exit, sending SIGKILL");
      ignore (Signal_unix.send_i Signal.kill (`Pid pid));
      let%bind _ = Unix.waitpid pid in
      Logs.debug (fun m -> m "Process killed with SIGKILL");
      return ())

(** Main stdio_client function *)
let stdio_client params ~stderr =
  let open Deferred.Let_syntax in
  (* Create pipes for message passing *)
  let read_pipe_r, read_pipe_w = Pipe.create () in
  let write_pipe_r, write_pipe_w = Pipe.create () in

  (* Determine environment *)
  let env =
    match params.env with
    | Some _ -> params.env
    | None -> Some (get_default_environment ())
  in

  (* Spawn the process *)
  let%bind pid, process_stdin, process_stdout =
    spawn_process ~command:params.command ~args:params.args ~env ~cwd:params.cwd
      ~stderr_writer:stderr
  in

  (* Start reader and writer tasks *)
  let reader_task = stdout_reader process_stdout read_pipe_w in
  let writer_task = stdin_writer process_stdin write_pipe_r in

  (* Set up cleanup on completion *)
  let cleanup = Deferred.all_unit [ reader_task; writer_task ] in
  upon cleanup (fun () -> don't_wait_for (graceful_shutdown pid process_stdin));

  return (read_pipe_r, write_pipe_w)
