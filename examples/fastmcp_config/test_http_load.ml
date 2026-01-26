open! Core
open! Async
open Cohttp_async
open Fastmcp_config_lib
[@@@alert "-unsafe_multidomain"]

let config_json =
  {|
{
  "command": "echo",
  "args": ["hello"],
  "deployment": {
    "env": {
      "TEST_VAR": "${TEST_ENV_VAR}"
    }
  }
}
|}

let handler ~body:_ _sock _req = Server.respond_string config_json

let%expect_test "load config from http" =
  let port = 8089 in
  let%bind server =
    Server.create ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_port port)
      handler
  in

  (* let _port = Server.listening_on server in *)

  (* Set environment variable for interpolation *)
  Unix.putenv ~key:"TEST_ENV_VAR" ~data:"SUCCESS";

  let url = sprintf "http://localhost:%d/config.json" port in
  let%bind config_opt = Config.load ~path:url () in

  (match config_opt with
  | Some config -> (
    print_endline "Config loaded successfully";
    match config.Config.Types.deployment with
    | Some { env = Some vars; _ } ->
      List.iter vars ~f:(fun (k, v) -> printf "%s=%s\n" k v)
    | _ -> print_endline "No env vars found")
  | None -> print_endline "Failed to load config");

  [%expect {|
    Config loaded successfully
    TEST_VAR=SUCCESS |}];

  Server.close server
