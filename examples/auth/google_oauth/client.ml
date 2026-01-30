open Core
open Async
open Ox_fast_mcp_client

let main () =
  (* 1. Create Transport *)
  let transport_result =
    Ox_fast_mcp_client.Transports.create_streamable_http_transport ~auth:OAuth
      (* Note: For full OAuth flow (opening browser), we rely on the transport
         or external helper. *)
      "http://localhost:8000/mcp"
  in

  match transport_result with
  | Error e ->
    print_endline
      (sprintf "❌ Failed to create transport: %s" (Error.to_string_hum e));
    return ()
  | Ok transport -> (
    (* 2. Create Client *)
    let client = Client.create transport in

    (* 3. Ping *)
    print_endline "🔌 Connecting to Google OAuth Protected Server...";
    match%bind Client.ping client with
    | false ->
      print_endline "❌ Failed to connect/ping server";
      return ()
    | true -> (
      print_endline "✅ Successfully authenticated and connected!";

      (* 4. List Tools *)
      let%bind tools = Client.list_tools client in
      print_endline (sprintf "🔧 Available tools (%d):" (List.length tools));
      List.iter tools ~f:(fun tool ->
          print_endline
            (sprintf "   - %s: %s" tool.base_metadata.name
               (Option.value tool.description ~default:"(no description)")));

      print_endline "🔒 Calling protected tool: get_access_token_claims";
      match%bind
        Async.try_with (fun () ->
            Client.call_tool client ~name:"get_access_token_claims"
              ~arguments:[] ())
      with
      | Error exn ->
        print_endline (sprintf "❌ Tool call failed: %s" (Exn.to_string exn));
        return ()
      | Ok result ->
        (* Parse result content *)
        let content =
          match result with
          | [ `Text { text; _ } ] -> text
          | _ -> "Unexpected content format"
        in
        print_endline "📄 Available access token claims (or stub):";
        print_endline content;
        return ()))

let () =
  Command.async ~summary:"Google OAuth Client Example"
    (Command.Param.return main)
  |> Command_unix.run
