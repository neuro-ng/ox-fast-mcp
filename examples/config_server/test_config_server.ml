open Core
open Async
open Config_server_lib

let sexp_of_json json = Sexp.Atom (Yojson.Safe.to_string json)

let%expect_test "default config" =
  let server = Config_server.create ~name:"MyServer" ~debug:false () in

  (* Check status *)
  let%bind status =
    Server.Ox_fast_mcp.call_tool server ~name:"get_status"
      ~arguments:(`Assoc [])
  in
  print_s [%sexp (status : json)];
  [%expect
    {| "{\"server_name\":\"MyServer\",\"debug_mode\":false,\"original_name\":\"MyServer\"}" |}];

  (* Check echo *)
  let%bind echo =
    Server.Ox_fast_mcp.call_tool server ~name:"echo_message"
      ~arguments:(`Assoc [ ("message", `String "Hello") ])
  in
  (match echo with
  | `String s -> print_endline s
  | _ -> print_endline "Unexpected result");
  [%expect {| Hello |}];
  return ()

let%expect_test "debug config" =
  let server = Config_server.create ~name:"MyServer" ~debug:true () in

  (* Check status *)
  let%bind status =
    Server.Ox_fast_mcp.call_tool server ~name:"get_status"
      ~arguments:(`Assoc [])
  in
  (* Verify server name changed *)
  (match status with
  | `Assoc fields ->
    let name = List.Assoc.find_exn fields ~equal:String.equal "server_name" in
    print_s [%sexp (name : json)]
  | _ -> ());
  [%expect {| "\"MyServer (Debug)\"" |}];

  (* Check echo *)
  let%bind echo =
    Server.Ox_fast_mcp.call_tool server ~name:"echo_message"
      ~arguments:(`Assoc [ ("message", `String "Hello") ])
  in
  (match echo with
  | `String s -> print_endline s
  | _ -> print_endline "Unexpected result");
  [%expect {| [DEBUG] Echoing: Hello |}];
  return ()
