open! Core
open Async

let main () =
  let process =
    Process.create_exn
      ~prog:"/home/neu/workspace/ox-fast-mcp/debug_calculator.sh" ~args:[] ()
  in
  let%bind process = process in
  let writer = Process.stdin process in
  let reader = Process.stdout process in

  (* let stderr = Process.stderr process in *)

  (* Helper to read response *)
  let read_response () =
    match%bind Reader.read_line reader with
    | `Ok line -> (
      try
        let json = Yojson.Safe.from_string line in
        printf "Received: %s\n" (Yojson.Safe.to_string json);
        return ()
      with _ ->
        printf "Received raw: %s\n" line;
        return ())
    | `Eof ->
      printf "EOF\n";
      return ()
  in

  (* 1. Initialize *)
  let init_req =
    "{\"jsonrpc\": \"2.0\", \"id\": 0, \"method\": \"initialize\", \"params\": \
     {\"protocolVersion\": \"2024-11-05\", \"capabilities\": {}, \
     \"clientInfo\": {\"name\": \"test\", \"version\": \"1.0\"}}}"
  in
  printf "Sending: %s\n" init_req;
  Writer.write_line writer init_req;
  let%bind () = read_response () in

  (* 2. Initialized *)
  let initialized =
    "{\"jsonrpc\": \"2.0\", \"method\": \"notifications/initialized\"}"
  in
  printf "Sending: %s\n" initialized;
  Writer.write_line writer initialized;

  (* 3. List Tools *)
  let tools_req =
    "{\"jsonrpc\": \"2.0\", \"id\": 1, \"method\": \"tools/list\"}"
  in
  printf "Sending: %s\n" tools_req;
  Writer.write_line writer tools_req;
  let%bind () = read_response () in

  (* 4. Call Tool *)
  let call_req =
    "{\"jsonrpc\": \"2.0\", \"id\": 2, \"method\": \"tools/call\", \"params\": \
     {\"name\": \"add\", \"arguments\": {\"a\": 4, \"b\": 6}}}"
  in
  printf "Sending: %s\n" call_req;
  Writer.write_line writer call_req;
  let%bind () = read_response () in

  let%bind _ = Process.collect_output_and_wait process in
  return ()

let () =
  Command.async ~summary:"Test MCP session" (Command.Param.return main)
  |> Command_unix.run
