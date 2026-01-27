open Core
open Async

let () = Log.Global.set_level `Debug

module Client = Ox_fast_mcp_client

let main () =
  let url = "http://localhost:8000/mcp/v1" in
  (* Load token from environment *)
  let token =
    match Sys.getenv "FASTMCP_CLIENT_ACCESS_TOKEN" with
    | Some t -> t
    | None ->
      printf "Warning: FASTMCP_CLIENT_ACCESS_TOKEN not set. Using mock token.\n";
      "mock_authkit_token"
  in

  printf "Connecting to server at %s with token...\n" url;

  (* 1. Create Transport with Auth *)
  let transport_result =
    Client.Transports.create_streamable_http_transport
      ~auth:(Client.Transports.Bearer token) url
  in

  match transport_result with
  | Error e ->
    printf "Failed to create transport: %s\n" (Error.to_string_hum e);
    Deferred.unit
  | Ok transport -> (
    (* 2. Create Client *)
    let client = Client.Client.create transport in

    (* 3. Connect *)
    try
      let%bind () = Client.Client.connect client in
      printf "Connected! Available tools:\n";
      let%bind tools = Client.Client.list_tools client in
      List.iter tools ~f:(fun t ->
          printf "- %s: %s\n" t.base_metadata.name
            (Option.value t.description ~default:"(no description)"));

      printf "\nCalling 'echo' tool...\n";
      let args = [ ("message", `String "Hello form AuthKit Client!") ] in
      let%bind _ =
        Client.Client.call_tool client ~name:"echo" ~arguments:args ()
      in
      print_endline "Tool execution successful!";

      Deferred.unit
    with exn ->
      printf "Connection/Interaction failed: %s\n" (Exn.to_string exn);
      Deferred.unit)

let () =
  Command.async ~summary:"AuthKit DCR Example Client"
    (Command.Param.return main)
  |> Command_unix.run
