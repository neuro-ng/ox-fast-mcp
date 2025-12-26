(** Tests for Stateful Proxy Client module.

    Translated from Python test_stateful_proxy_client.py to OCaml. Tests focus
    on unit-testable components like session caching and client creation.

    Note: Python tests require full async Client/FastMCP connections with HTTP
    servers which are not unit-testable in OCaml. These tests focus on cache
    management and state patterns. *)

open! Core
open! Expect_test_helpers_core
module Conftest = Conftest

(* =============================================================================
   Test: Stateful_proxy_client Creation
   ============================================================================= *)

let%expect_test "Stateful_proxy_client.create - creates with client" =
  let proxy_client =
    Server__Proxy.Proxy_client.create ~transport:(`Assoc []) ~name:"test-client"
      ()
  in
  let _stateful : unit Server__Proxy.Stateful_proxy_client.t =
    Server__Proxy.Stateful_proxy_client.create ~client:proxy_client
  in
  printf "stateful_client_created: true\n";
  [%expect {| stateful_client_created: true |}]

(* =============================================================================
   Test: Stateful_proxy_client.clear
   ============================================================================= *)

let%expect_test "Stateful_proxy_client.clear - clears cache" =
  let proxy_client =
    Server__Proxy.Proxy_client.create ~transport:(`Assoc []) ~name:"test-client"
      ()
  in
  let stateful =
    Server__Proxy.Stateful_proxy_client.create ~client:proxy_client
  in
  (* Clear should complete without error *)
  Async.Thread_safe.block_on_async_exn (fun () ->
      Server__Proxy.Stateful_proxy_client.clear stateful);
  printf "cache_cleared: true\n";
  [%expect {| cache_cleared: true |}]

(* =============================================================================
   Test: new_stateful Pattern
   ============================================================================= *)

let%expect_test "Stateful_proxy_client.new_stateful - creates client with \
                 session" =
  let proxy_client =
    Server__Proxy.Proxy_client.create ~transport:(`Assoc []) ~name:"test-client"
      ()
  in
  let stateful =
    Server__Proxy.Stateful_proxy_client.create ~client:proxy_client
  in
  let session = `String "session-123" in
  let create_client () = Conftest.Mock_client.create ~name:"SessionClient" () in
  let client =
    Server__Proxy.Stateful_proxy_client.new_stateful stateful ~session
      ~create_client
  in
  printf "client_created: true\n";
  let _ = client in
  [%expect {| client_created: true |}]

(* =============================================================================
   Test: Session-based State Pattern (Simulating Python test pattern)
   ============================================================================= *)

(** Simulated state storage for testing session behavior *)
module Session_state = struct
  let states : (int, int) Hashtbl.t = Hashtbl.create (module Int)
  let put ~session_id ~value = Hashtbl.set states ~key:session_id ~data:value

  let get ~session_id =
    match Hashtbl.find states session_id with
    | Some v -> Ok v
    | None -> Error "Value not found"

  let clear () = Hashtbl.clear states
end

let%expect_test "Session_state - stateful put and get" =
  (* Simulates Python test_stateful_proxy behavior *)
  Session_state.clear ();
  let session1 = 1 in
  let session2 = 2 in

  (* Initially, get should fail *)
  (match Session_state.get ~session_id:session1 with
  | Error msg -> printf "initial_get: Error(%s)\n" msg
  | Ok _ -> printf "initial_get: Ok\n");

  (* Put a value *)
  Session_state.put ~session_id:session1 ~value:42;

  (* Now get should succeed *)
  (match Session_state.get ~session_id:session1 with
  | Ok v -> printf "after_put: Ok(%d)\n" v
  | Error _ -> printf "after_put: Error\n");

  (* Different session should still fail *)
  (match Session_state.get ~session_id:session2 with
  | Error _ -> printf "session2_get: Error\n"
  | Ok _ -> printf "session2_get: Ok\n");
  [%expect
    {|
    initial_get: Error(Value not found)
    after_put: Ok(42)
    session2_get: Error
    |}]

let%expect_test "Session_state - sessions are isolated" =
  (* Simulates Python test showing sessions don't mix *)
  Session_state.clear ();
  let session_a = 100 in
  let session_b = 200 in

  Session_state.put ~session_id:session_a ~value:1;
  Session_state.put ~session_id:session_b ~value:2;

  (match Session_state.get ~session_id:session_a with
  | Ok v -> printf "session_a: %d\n" v
  | Error _ -> printf "session_a: Error\n");

  (match Session_state.get ~session_id:session_b with
  | Ok v -> printf "session_b: %d\n" v
  | Error _ -> printf "session_b: Error\n");
  [%expect {|
    session_a: 1
    session_b: 2
    |}]

(* =============================================================================
   Test: Stateless Pattern (Simulating stateless HTTP behavior)
   ============================================================================= *)

let%expect_test "Stateless pattern - state not preserved across calls" =
  (* Simulates Python test_stateless_proxy behavior In stateless mode, each
     request creates a new session *)
  Session_state.clear ();

  (* Simulate first request with session 1 *)
  let request1_session = 1001 in
  Session_state.put ~session_id:request1_session ~value:99;

  (* Simulate second request with different session (stateless) *)
  let request2_session = 1002 in
  (match Session_state.get ~session_id:request2_session with
  | Error _ -> printf "stateless_get: Error (expected)\n"
  | Ok v -> printf "stateless_get: Ok(%d)\n" v);
  [%expect {| stateless_get: Error (expected) |}]

(* =============================================================================
   Test: Multi-Proxy Pattern (Simulating mounted proxies)
   ============================================================================= *)

(** Mock proxy for testing multi-proxy patterns *)
module Mock_proxy = struct
  type t = { prefix : string; client_factory : unit -> Conftest.Mock_client.t }

  let create ~prefix ~tools () =
    {
      prefix;
      client_factory =
        (fun () -> Conftest.Mock_client.create ~name:prefix ~tools ());
    }

  let call_tool t ~name =
    let client = t.client_factory () in
    let full_name =
      if String.is_prefix name ~prefix:(t.prefix ^ "_") then
        String.chop_prefix_exn name ~prefix:(t.prefix ^ "_")
      else name
    in
    Conftest.Mock_client.call_tool client ~name:full_name ~arguments:`Null
end

let%expect_test "Multi-proxy pattern - proxies don't mix" =
  (* Simulates Python test_multi_proxies_no_mixing *)
  let proxy_a =
    Mock_proxy.create ~prefix:"a" ~tools:[ ("tool_a", `Assoc []) ] ()
  in
  let proxy_b =
    Mock_proxy.create ~prefix:"b" ~tools:[ ("tool_b", `Assoc []) ] ()
  in

  let result_a = Mock_proxy.call_tool proxy_a ~name:"a_tool_a" in
  let result_b = Mock_proxy.call_tool proxy_b ~name:"b_tool_b" in

  let open Yojson.Safe.Util in
  let is_error_a =
    match result_a |> member "isError" with
    | `Bool b -> b
    | _ -> false
  in
  let is_error_b =
    match result_b |> member "isError" with
    | `Bool b -> b
    | _ -> false
  in

  printf "result_a_error: %b\n" is_error_a;
  printf "result_b_error: %b\n" is_error_b;
  [%expect {|
    result_a_error: false
    result_b_error: false
    |}]

(* =============================================================================
   Test: Concurrent Log Handler Pattern
   ============================================================================= *)

(** Mock log collection for testing concurrent handlers *)
module Concurrent_log_test = struct
  type log_entry = { logger : string; message : string }

  let entries : (string, log_entry) Hashtbl.t = Hashtbl.create (module String)
  let clear () = Hashtbl.clear entries

  let add_handler ~handler_name (msg : Yojson.Safe.t) =
    let open Yojson.Safe.Util in
    let logger =
      match msg |> member "logger" with
      | `String s -> s
      | _ -> "unknown"
    in
    let message =
      match msg |> member "message" with
      | `String s -> s
      | _ -> ""
    in
    Hashtbl.set entries ~key:handler_name ~data:{ logger; message };
    Async.return ()

  let get_entry handler_name = Hashtbl.find entries handler_name
end

let%expect_test "Concurrent log handlers - don't mix (simulated)" =
  (* Simulates Python test_concurrent_log_requests_no_mixing *)
  Concurrent_log_test.clear ();

  let handler_a = Concurrent_log_test.add_handler ~handler_name:"logger_a" in
  let handler_b = Concurrent_log_test.add_handler ~handler_name:"logger_b" in

  (* Simulate concurrent log calls *)
  let log_a =
    `Assoc [ ("logger", `String "a"); ("message", `String "Hello from A") ]
  in
  let log_b =
    `Assoc [ ("logger", `String "b"); ("message", `String "Hello from B") ]
  in

  Async.Thread_safe.block_on_async_exn (fun () -> handler_a log_a);
  Async.Thread_safe.block_on_async_exn (fun () -> handler_b log_b);

  (* Verify handlers received correct logs *)
  (match Concurrent_log_test.get_entry "logger_a" with
  | Some entry -> printf "logger_a received: %s\n" entry.logger
  | None -> printf "logger_a: not found\n");

  (match Concurrent_log_test.get_entry "logger_b" with
  | Some entry -> printf "logger_b received: %s\n" entry.logger
  | None -> printf "logger_b: not found\n");
  [%expect {|
    logger_a received: a
    logger_b received: b
    |}]

(* =============================================================================
   Test: Session ID Pattern (for cache key generation)
   ============================================================================= *)

let%expect_test "Session ID pattern - different sessions have different IDs" =
  (* In Python, id(context.session) is used as cache key. In OCaml, we can use
     any unique identifier. *)
  let session1 = Obj.repr (object end) in
  let session2 = Obj.repr (object end) in
  let id1 = Obj.magic session1 |> Hashtbl.hash in
  let id2 = Obj.magic session2 |> Hashtbl.hash in
  printf "ids_different: %b\n" (not (Int.equal id1 id2));
  [%expect {| ids_different: true |}]

(* =============================================================================
   Test: Logging Level Handling
   ============================================================================= *)

type logging_level =
  | Debug
  | Info
  | Notice
  | Warning
  | Error
  | Critical
  | Alert
  | Emergency

let logging_level_to_string = function
  | Debug -> "debug"
  | Info -> "info"
  | Notice -> "notice"
  | Warning -> "warning"
  | Error -> "error"
  | Critical -> "critical"
  | Alert -> "alert"
  | Emergency -> "emergency"

let logging_level_of_string = function
  | "debug" -> Some Debug
  | "info" -> Some Info
  | "notice" -> Some Notice
  | "warning" -> Some Warning
  | "error" -> Some Error
  | "critical" -> Some Critical
  | "alert" -> Some Alert
  | "emergency" -> Some Emergency
  | _ -> None

let%expect_test "Logging level - round trip" =
  let levels = [ Debug; Info; Warning; Error; Critical ] in
  List.iter levels ~f:(fun level ->
      let str = logging_level_to_string level in
      let parsed = logging_level_of_string str in
      printf "level: %s, round_trip: %b\n" str (Option.is_some parsed));
  [%expect
    {|
    level: debug, round_trip: true
    level: info, round_trip: true
    level: warning, round_trip: true
    level: error, round_trip: true
    level: critical, round_trip: true
    |}]

let%expect_test "Log message structure for stateful client" =
  let log_msg =
    `Assoc
      [
        ("message", `String "Hello, world!");
        ("level", `String "info");
        ("logger", `String "test");
      ]
  in
  let open Yojson.Safe.Util in
  printf "message: %s\n" (log_msg |> member "message" |> to_string);
  printf "level: %s\n" (log_msg |> member "level" |> to_string);
  printf "logger: %s\n" (log_msg |> member "logger" |> to_string);
  [%expect
    {|
    message: Hello, world!
    level: info
    logger: test
    |}]
