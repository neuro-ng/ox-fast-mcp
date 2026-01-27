open Core
open Async
open Cohttp_async
open Ox_fast_mcp_server

module Async_workos_verifier = struct
  type t = { userinfo_url : string }

  let create ~userinfo_url = { userinfo_url }

  let verify_token t token =
    (* Verify token by calling userinfo endpoint *)
    match%bind
      try_with (fun () ->
          let headers =
            Cohttp.Header.init_with "Authorization" ("Bearer " ^ token)
          in
          let%bind response, body =
            Client.get ~headers (Uri.of_string t.userinfo_url)
          in
          match Cohttp.Response.status response with
          | `OK -> return (Ok ())
          | status ->
            let%bind body_str = Body.to_string body in
            return
              (Error
                 (sprintf "Token validation failed. Status: %s. Body: %s"
                    (Cohttp.Code.string_of_status status)
                    body_str)))
    with
    | Ok (Ok ()) -> return (Ok ())
    | Ok (Error msg) -> return (Error msg)
    | Error exn -> return (Error (Exn.to_string exn))
end

let () =
  let _ = Logging.configure_logging ~level:Logging.Level.Debug () in
  Logging.Global.configure ~with_timestamp:true ()

let main () =
  let%bind server_and_config =
    (* Create the WorkOS provider to get configuration details *)
    let provider_result =
      Server_auth_providers.Workos.Workos_provider.create ()
    in
    let provider =
      match provider_result with
      | Ok p -> p
      | Error e ->
        failwith
          (sprintf "Failed to create WorkOS provider: %s"
             (Error.to_string_hum e))
    in

    let token_verifier_config =
      Server_auth_providers.Workos.Workos_provider.token_verifier provider
    in
    let userinfo_url =
      Server_auth_providers.Workos.Workos_token_verifier.userinfo_url
        token_verifier_config
    in

    let verifier = Async_workos_verifier.create ~userinfo_url in

    let auth_config =
      {
        Ox_fast_mcp_server.Http.Auth_config.required_scopes =
          Server_auth_providers.Workos.Workos_provider.required_scopes provider;
        get_middleware = (fun () -> []);
        get_routes = (fun ~mcp_path:_ -> []);
        get_resource_url = (fun _ -> None);
        verify_token = Async_workos_verifier.verify_token verifier;
      }
    in

    let server = Server.Ox_fast_mcp.create ~name:"WorkOS OAuth Example" () in

    Server.Ox_fast_mcp.add_simple_tool server ~name:"echo"
      ~description:"Echo the message"
      ~parameters:
        (`Assoc [ ("message", `Assoc [ ("type", `String "string") ]) ])
      ~handler:(fun args ->
        let message = Yojson.Safe.Util.(member "message" args |> to_string) in
        return
          (`Assoc
            [
              ( "content",
                `List
                  [
                    `Assoc
                      [ ("type", `String "text"); ("text", `String message) ];
                  ] );
            ]));

    Server.Ox_fast_mcp.add_simple_tool server ~name:"auth_status"
      ~description:"Show authentication status" ~handler:(fun _ ->
        return
          (`Assoc
            [
              ( "content",
                `List
                  [
                    `Assoc
                      [
                        ("type", `String "text");
                        ("text", `String "Authenticated via WorkOS!");
                      ];
                  ] );
            ]));

    return (server, auth_config)
  in
  let server, auth_config = server_and_config in
  let%bind () =
    Server.Ox_fast_mcp.run_async server ~port:8000 ~auth:auth_config ()
  in
  Deferred.never ()

let () =
  Command.async ~summary:"WorkOS OAuth Server Example"
    (Command.Param.return main)
  |> Command_unix.run
