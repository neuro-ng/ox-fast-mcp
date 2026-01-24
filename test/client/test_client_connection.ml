open Core
open Async
open Alcotest_async

(* Test client connection idempotency *)
let test_client_connection switch () =
  let server = Server.create ~name:"TestServer" () in
  let client = Client.create (Transports.FastMCP_transport server) in

  let%bind () =
    Client.with_client client (fun () ->
        check bool "is connected" true (Client.is_connected client);
        let%bind () = Client.ping client in
        return ())
  in
  check bool "is disconnected" false (Client.is_connected client);
  return ()

(* Test initialize called once *)
let test_initialize_called_once switch () =
  let server = Server.create ~name:"TestServer" () in
  let initialize_count = ref 0 in
  let mock_initialize _session =
    initialize_count := !initialize_count + 1;
    return
      (Types.Initialize_result.create
         ~server_info:
           (Types.Server_info.create ~name:"TestServer" ~version:"1.0.0" ()))
  in

  let client =
    Client.create ~initialize:mock_initialize
      (Transports.FastMCP_transport server)
  in

  let%bind () =
    Client.with_client client (fun () ->
        check int "initialize count" 1 !initialize_count;
        return ())
  in
  return ()

(* Test initialize_result behavior *)
let test_initialize_result switch () =
  let server = Server.create ~name:"TestServer" () in
  let client = Client.create (Transports.FastMCP_transport server) in

  (* Should raise before connection *)
  check_raises "not connected error" (Failure "Client is not connected")
    (fun () -> ignore (Client.initialize_result client));

  let%bind () =
    Client.with_client client (fun () ->
        let result = Client.initialize_result client in
        check string "server name" "TestServer" result.Types.server_info.name;
        check bool "has version" true
          (Option.is_some result.Types.server_info.version);
        return ())
  in

  (* Should raise after disconnection *)
  check_raises "not connected error" (Failure "Client is not connected")
    (fun () -> ignore (Client.initialize_result client));
  return ()

(* Test server info custom version *)
let test_server_info_custom_version switch () =
  (* Test with custom version *)
  let server_with_version =
    Server.create ~name:"CustomVersionServer" ~version:"1.2.3" ()
  in
  let client =
    Client.create (Transports.FastMCP_transport server_with_version)
  in

  let%bind () =
    Client.with_client client (fun () ->
        let result = Client.initialize_result client in
        check string "server name" "CustomVersionServer"
          result.Types.server_info.name;
        check string "server version" "1.2.3"
          (Option.value_exn result.Types.server_info.version);
        return ())
  in

  (* Test without version *)
  let server_without_version = Server.create ~name:"DefaultVersionServer" () in
  let client =
    Client.create (Transports.FastMCP_transport server_without_version)
  in

  let%bind () =
    Client.with_client client (fun () ->
        let result = Client.initialize_result client in
        check string "server name" "DefaultVersionServer"
          result.Types.server_info.name;
        check bool "has default version" true
          (Option.is_some result.Types.server_info.version);
        check bool "version differs" true
          (Option.value_exn result.Types.server_info.version <> "1.2.3");
        return ())
  in
  return ()

(* Test nested context managers *)
let test_client_nested_context_manager switch () =
  let server = Server.create ~name:"TestServer" () in
  let client = Client.create (Transports.FastMCP_transport server) in

  (* Before connection *)
  check bool "not connected initially" false (Client.is_connected client);
  check bool "no session initially" true (Option.is_none client.session);

  let%bind () =
    Client.with_client client (fun () ->
        check bool "connected in outer context" true
          (Client.is_connected client);
        check bool "has session in outer context" true
          (Option.is_some client.session);
        let session = client.session in

        let%bind () =
          Client.with_client client (fun () ->
              check bool "connected in inner context 1" true
                (Client.is_connected client);
              check bool "same session in inner context 1" true
                (Option.equal phys_equal client.session session);
              return ())
        in

        let%bind () =
          Client.with_client client (fun () ->
              check bool "connected in inner context 2" true
                (Client.is_connected client);
              check bool "same session in inner context 2" true
                (Option.equal phys_equal client.session session);
              return ())
        in
        return ())
  in

  (* After connection *)
  check bool "not connected after" false (Client.is_connected client);
  check bool "no session after" true (Option.is_none client.session);
  return ()

(* Test concurrent client usage *)
let test_concurrent_client_context_managers switch () =
  let server = Server.create ~name:"TestServer" () in

  (* Add a tool *)
  let echo_tool =
    Tool.create ~name:"echo" ~f:(fun args ->
        let text =
          List.Assoc.find_exn args ~equal:String.equal "text"
          |> Yojson.Safe.to_string
        in
        return [ Content_block.text text ])
  in
  Server.add_tool server echo_tool;

  let client = Client.create (Transports.FastMCP_transport server) in

  (* Track results *)
  let results = Hashtbl.create (module String) in
  let errors = ref [] in

  let use_client task_id delay =
    try%bind
      Client.with_client client (fun () ->
          let%bind () = Clock.after (Time.Span.of_sec delay) in
          let%bind tools = Client.list_tools client in
          Hashtbl.set results ~key:task_id ~data:(List.length tools);
          return ())
    with e ->
      errors := (task_id, Exn.to_string e) :: !errors;
      return ()
  in

  (* Run multiple tasks concurrently *)
  let%bind () =
    Deferred.all_unit
      [
        use_client "task1" 0.0;
        use_client "task2" 0.01;
        (* Slight delay to ensure overlap *)
        use_client "task3" 0.02;
      ]
  in

  check int "error count" 0 (List.length !errors);
  check int "result count" 3 (Hashtbl.length results);
  Hashtbl.iter results ~f:(fun count -> check int "tool count" 1 count);
  return ()

(* Test timeout tool call overrides client timeout *)
let test_timeout_tool_call_overrides_client_timeout switch () =
  let server = Server.create ~name:"TestServer" () in

  (* Add a sleep tool *)
  let sleep_tool =
    Tool.create ~name:"sleep" ~f:(fun args ->
        let seconds =
          List.Assoc.find_exn args ~equal:String.equal "seconds"
          |> Yojson.Safe.to_float
        in
        let%bind () = Clock.after (Time.Span.of_sec seconds) in
        return [ Content_block.text (sprintf "Slept for %f seconds" seconds) ])
  in
  Server.add_tool server sleep_tool;

  let client =
    Client.create ~timeout:(Time.Span.of_sec 2.0)
      (Transports.FastMCP_transport server)
  in

  let%bind () =
    Client.with_client client (fun () ->
        match%bind
          Client.call_tool ~timeout:(Time.Span.of_sec 0.01) client ~name:"sleep"
            ~arguments:[ ("seconds", `Float 0.1) ]
            ()
        with
        | Ok _ -> failwith "Expected timeout error but got success"
        | Error e ->
          check bool "error contains timeout message" true
            (String.is_substring
               ~substring:"Timed out while waiting for response"
               (Error.to_string_hum e));
          return ())
  in
  return ()

(* Test timeout tool call overrides client timeout even if lower *)
let test_timeout_tool_call_overrides_client_timeout_even_if_lower switch () =
  let server = Server.create ~name:"TestServer" () in

  (* Add a sleep tool *)
  let sleep_tool =
    Tool.create ~name:"sleep" ~f:(fun args ->
        let seconds =
          List.Assoc.find_exn args ~equal:String.equal "seconds"
          |> Yojson.Safe.to_float
        in
        let%bind () = Clock.after (Time.Span.of_sec seconds) in
        return [ Content_block.text (sprintf "Slept for %f seconds" seconds) ])
  in
  Server.add_tool server sleep_tool;

  let client =
    Client.create ~timeout:(Time.Span.of_sec 0.01)
      (Transports.FastMCP_transport server)
  in

  let%bind () =
    Client.with_client client (fun () ->
        let%bind result =
          Client.call_tool ~timeout:(Time.Span.of_sec 2.0) client ~name:"sleep"
            ~arguments:[ ("seconds", `Float 0.1) ]
            ()
        in
        match result with
        | Ok content ->
          check bool "completed successfully" true
            (match List.hd_exn content with
            | Types.Text_content { text; _ } ->
              String.is_substring ~substring:"Slept for 0.1 seconds" text
            | _ -> false);
          return ()
        | Error e -> failwith (Error.to_string_hum e))
  in
  return ()

let () =
  run "Client Connection Tests"
    [
      ( "connection",
        [
          test_case "client connection" `Quick test_client_connection;
          test_case "initialize called once" `Quick test_initialize_called_once;
          test_case "initialize result" `Quick test_initialize_result;
          test_case "server info custom version" `Quick
            test_server_info_custom_version;
          test_case "nested context manager" `Quick
            test_client_nested_context_manager;
          test_case "concurrent client usage" `Quick
            test_concurrent_client_context_managers;
          test_case "timeout tool call overrides client timeout" `Quick
            test_timeout_tool_call_overrides_client_timeout;
          test_case "timeout tool call overrides client timeout even if lower"
            `Quick test_timeout_tool_call_overrides_client_timeout_even_if_lower;
        ] );
    ]
