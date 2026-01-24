(** Tests for OxFastMCP HTTP Application State Management *)

open! Core
open! Async
module Http = Server__Http

(** {1 Test: SSE App Sets Server State} *)

let%expect_test "create_sse_app sets fastmcp_server state" =
  let open Async.Deferred.Let_syntax in
  (* Create a mock server as JSON *)
  let server_json = `Assoc [ ("name", `String "StateTest") ] in

  (* Create SSE app *)
  let app =
    Http.create_sse_app ~server:server_json ~message_path:"/message"
      ~sse_path:"/sse/" ()
  in

  (* Verify the server is set in app state *)
  let stored_server = Http.App_state.get_fastmcp_server app.state in
  print_s [%sexp (Option.is_some stored_server : bool)];
  [%expect {| true |}];

  (* Verify the stored server matches what we set *)
  (match stored_server with
  | Some json -> print_endline (Yojson.Safe.to_string json)
  | None -> print_endline "No server found");
  [%expect {| {"name":"StateTest"} |}];
  return ()

(** {1 Test: Streamable HTTP App Sets Server State} *)

let%expect_test "create_streamable_http_app sets fastmcp_server state" =
  let open Async.Deferred.Let_syntax in
  (* Create a mock server as JSON *)
  let server_json = `Assoc [ ("name", `String "StateTest") ] in

  (* Create Streamable HTTP app *)
  let app =
    Http.create_streamable_http_app ~server:server_json
      ~streamable_http_path:"/mcp/" ()
  in

  (* Verify the server is set in app state *)
  let stored_server = Http.App_state.get_fastmcp_server app.state in
  print_s [%sexp (Option.is_some stored_server : bool)];
  [%expect {| true |}];

  (* Verify the stored server matches what we set *)
  (match stored_server with
  | Some json -> print_endline (Yojson.Safe.to_string json)
  | None -> print_endline "No server found");
  [%expect {| {"name":"StateTest"} |}];
  return ()

(** {1 Test: SSE App Sets Path State} *)

let%expect_test "create_sse_app sets path state" =
  let open Async.Deferred.Let_syntax in
  let server_json = `Assoc [ ("name", `String "StateTest") ] in
  let app =
    Http.create_sse_app ~server:server_json ~message_path:"/message"
      ~sse_path:"/sse/" ()
  in

  (* Verify the path is set in app state *)
  let stored_path = Http.App_state.get_path app.state in
  print_s [%sexp (stored_path : string option)];
  [%expect {| (/sse/) |}];
  return ()

(** {1 Test: Streamable HTTP App Sets Path State} *)

let%expect_test "create_streamable_http_app sets path state" =
  let open Async.Deferred.Let_syntax in
  let server_json = `Assoc [ ("name", `String "StateTest") ] in
  let app =
    Http.create_streamable_http_app ~server:server_json
      ~streamable_http_path:"/mcp/" ()
  in

  (* Verify the path is set in app state *)
  let stored_path = Http.App_state.get_path app.state in
  print_s [%sexp (stored_path : string option)];
  [%expect {| (/mcp/) |}];
  return ()

(** {1 Test: App State Custom Data} *)

let%expect_test "app state can store and retrieve custom data" =
  let open Async.Deferred.Let_syntax in
  (* Create a new app state *)
  let state = Http.App_state.create () in

  (* Store custom data *)
  Http.App_state.set_data state ~key:"custom_key" ~data:(`String "custom_value");

  (* Retrieve custom data *)
  let retrieved = Http.App_state.get_data state ~key:"custom_key" in
  print_s [%sexp (Option.is_some retrieved : bool)];
  (match retrieved with
  | Some json -> print_endline (Yojson.Safe.to_string json)
  | None -> print_endline "None");
  [%expect {|
    true
    "custom_value" |}];
  return ()

let%expect_test "app state returns None for missing keys" =
  let open Async.Deferred.Let_syntax in
  let state = Http.App_state.create () in

  (* Try to retrieve non-existent key *)
  let retrieved = Http.App_state.get_data state ~key:"nonexistent" in
  print_s [%sexp (Option.is_some retrieved : bool)];
  [%expect {| false |}];
  return ()

let%expect_test "app state can overwrite custom data" =
  let open Async.Deferred.Let_syntax in
  let state = Http.App_state.create () in

  (* Store initial value *)
  Http.App_state.set_data state ~key:"key" ~data:(`String "value1");

  (* Overwrite withnew value *)
  Http.App_state.set_data state ~key:"key" ~data:(`String "value2");

  (* Retrieve should get the new value *)
  let retrieved = Http.App_state.get_data state ~key:"key" in
  (match retrieved with
  | Some json -> print_endline (Yojson.Safe.to_string json)
  | None -> print_endline "None");
  [%expect {| "value2" |}];
  return ()

(** {1 Test: Multiple Apps Have Independent State} *)

let%expect_test "multiple apps have independent state" =
  let open Async.Deferred.Let_syntax in
  let server1_json = `Assoc [ ("name", `String "Server1") ] in
  let server2_json = `Assoc [ ("name", `String "Server2") ] in

  let app1 =
    Http.create_sse_app ~server:server1_json ~message_path:"/msg1"
      ~sse_path:"/sse1/" ()
  in
  let app2 =
    Http.create_streamable_http_app ~server:server2_json
      ~streamable_http_path:"/mcp2/" ()
  in

  (* Each app should have its own server *)
  let server1 = Http.App_state.get_fastmcp_server app1.state in
  let server2 = Http.App_state.get_fastmcp_server app2.state in

  (match (server1, server2) with
  | Some s1, Some s2 ->
    print_endline (Yojson.Safe.to_string s1);
    print_endline (Yojson.Safe.to_string s2)
  | _ -> print_endline "Missing servers");
  [%expect {|
    {"name":"Server1"}
    {"name":"Server2"} |}];
  return ()
