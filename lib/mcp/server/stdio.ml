(**
   Stdio Server Transport Module

   This module provides functionality for creating an stdio-based transport layer
   that can be used to communicate with an MCP client through standard input/output
   streams.

   Example usage:
   {[
     let run_server () =
       Stdio_server.with_server (fun ~read_stream ~write_stream ->
         let* server = create_my_server () in
         server.run ~read_stream ~write_stream ~init_options)

     let () = Lwt_main.run (run_server ())
   ]}
*)

open Core
open Lwt.Syntax
open Mcp.Types

type stdio_server = {
  read_stream: (Mcp.Shared.Message.session_message, [> `Error of exn ]) result Lwt_stream.t;
  write_stream: Mcp.Shared.Message.session_message -> unit Lwt.t;
}

let create_stdio_server ?stdin ?stdout () =
  let stdin = Option.value stdin ~default:(Lwt_io.stdin) in
  let stdout = Option.value stdout ~default:(Lwt_io.stdout) in

  let (read_stream, read_push) = Lwt_stream.create () in
  let (write_stream, write_push) = Lwt_stream.create () in

  let rec stdin_reader () =
    let* line = Lwt_io.read_line stdin in
    let* () =
      try
        let json = Yojson.Safe.from_string line in
        let message = jsonrpc_message_of_yojson json in
        let session_message = Mcp.Shared.Message.create message in
        read_push (Some (Ok session_message));
        Lwt.return_unit
      with exn ->
        read_push (Some (Error (`Error exn)));
        Lwt.return_unit
    in
    stdin_reader ()
  in

  let rec stdout_writer () =
    let* msg_opt = Lwt_stream.get write_stream in
    match msg_opt with
    | None -> Lwt.return_unit
    | Some session_message ->
        let json = 
          session_message.message
          |> jsonrpc_message_to_yojson
          |> Yojson.Safe.to_string
        in
        let* () = Lwt_io.write_line stdout json in
        let* () = Lwt_io.flush stdout in
        stdout_writer ()
  in

  let write msg =
    write_push (Some msg);
    Lwt.return_unit
  in

  let* () = Lwt.join [
    Lwt.catch
      (fun () -> stdin_reader ())
      (function
        | End_of_file -> Lwt.return_unit
        | exn -> Lwt.fail exn);
    stdout_writer ();
  ] in

  Lwt.return { read_stream; write_stream = write }

let with_server ?stdin ?stdout f =
  let* server = create_stdio_server ?stdin ?stdout () in
  f ~read_stream:server.read_stream ~write_stream:server.write_stream 