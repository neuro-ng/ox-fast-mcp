open! Core
open! Async
open Mcp_client

let%expect_test "debug initialize response" =
  (* Minimal test to debug JSON structure *)
  let client_read, server_write = Pipe.create () in
  let server_read, client_write = Pipe.create () in

  let session =
    Session.create_from_pipes ~read_stream:client_read
      ~write_stream:client_write ()
  in

  (* Simplified mock server *)
  don't_wait_for
    (let%bind () =
       match%bind Pipe.read server_read with
       | `Eof -> return ()
       | `Ok msg -> (
         match msg.Mcp_shared.Message.message with
         | `Request req ->
           let result_json =
             `Assoc
               [
                 ("protocolVersion", `String "2024-11-05");
                 ("capabilities", `Assoc []);
                 ( "serverInfo",
                   `Assoc
                     [
                       ("version", `String "1.0.0");
                       ( "base_metadata",
                         `Assoc [ ("name", `String "TestServer") ] );
                     ] );
                 ("result", `Assoc [ ("_meta", `Assoc []) ]);
               ]
           in
           let response =
             {
               Mcp_shared.Message.message =
                 `Response
                   { jsonrpc = "2.0"; id = req.id; result = result_json };
               metadata = None;
             }
           in
           let%bind () = Pipe.write server_write response in
           let%bind _ = Pipe.read server_read in
           (* initialized notification *)
           return ()
         | _ -> return ())
     in
     Pipe.close_read server_read;
     return ());

  let%bind result = Session.initialize session in
  printf "Success: %s\n" result.server_info.base_metadata.name;
  [%expect {| Success: TestServer |}];
  return ()
