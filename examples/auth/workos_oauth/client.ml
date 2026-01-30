open Core
open Async
open Ox_fast_mcp_client

let server_url = Uri.of_string "http://localhost:8000/mcp"

let main () =
  (* The client will use the 'OAuth' flow. *)
  let transport_result =
    Ox_fast_mcp_client.Transports.create_streamable_http_transport ~auth:OAuth
      (Uri.to_string server_url)
  in
  match transport_result with
  | Error e ->
    print_endline
      (sprintf "❌ Failed to create transport: %s" (Error.to_string_hum e));
    return ()
  | Ok transport -> (
    let client = Client.create transport in
    print_endline "✅ Connected to WorkOS OAuth Protected Server";

    match%bind Client.ping client with
    | false ->
      print_endline "❌ Ping failed";
      return ()
    | true ->
      print_endline "✅ Ping successful";

      let%bind tools = Client.list_tools client in
      (match tools with
      | [] -> print_endline "🔧 Available tools: 0"
      | tools ->
        print_endline (sprintf "🔧 Available tools: %d" (List.length tools));
        List.iter tools ~f:(fun t ->
            print_endline
              (sprintf "  - %s: %s" t.base_metadata.name
                 (Option.value t.description ~default:""))));

      let%bind result =
        Async.try_with (fun () ->
            Client.call_tool client ~name:"echo"
              ~arguments:[ ("message", `String "Hello WorkOS") ]
              ())
      in
      (match result with
      | Ok [ `Text { text; _ } ] -> print_endline ("🎯 Echo result: " ^ text)
      | Ok _ -> print_endline "🎯 Echo result: [Non-text response]"
      | Error e -> print_endline ("❌ Failed to call echo: " ^ Exn.to_string e));

      let%bind auth_status =
        Async.try_with (fun () ->
            Client.call_tool client ~name:"auth_status" ~arguments:[] ())
      in
      (match auth_status with
      | Ok [ `Text { text; _ } ] -> print_endline ("👤 Auth status: " ^ text)
      | Ok _ -> print_endline "👤 Auth status: [Non-text response]"
      | Error e ->
        print_endline ("❌ Failed to call auth_status: " ^ Exn.to_string e));

      return ())

let () =
  Command.async ~summary:"WorkOS OAuth Client Example"
    (Command.Param.return main)
  |> Command_unix.run
