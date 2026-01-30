open Core
open Async
open Cohttp_async
open Ox_fast_mcp_server

module Async_google_verifier = struct
  let user_agent = "OxFastMCP-Google-OAuth"
  let google_tokeninfo_api = "https://www.googleapis.com/oauth2/v1/tokeninfo"

  let verify_token ~required_scopes:_ token =
    (* Google tokeninfo endpoint typically takes access_token as a query
       parameter *)
    let uri = Uri.of_string google_tokeninfo_api in
    let uri = Uri.add_query_param' uri ("access_token", token) in
    let headers =
      Cohttp.Header.of_list
        [ ("User-Agent", user_agent); ("Accept", "application/json") ]
    in
    match%bind try_with (fun () -> Client.get ~headers uri) with
    | Error exn ->
      return
        (Error (sprintf "Google API request failed: %s" (Exn.to_string exn)))
    | Ok (response, body) -> (
      match Cohttp.Response.status response with
      | `OK ->
        let%bind body_str = Body.to_string body in
        (* TODO: parse body to check scopes if strict checking is needed *)
        (* struct: { "issued_to": "...", "audience": "...", "user_id": "...", "scope": "...", "expires_in": 123, ... } *)
        let _ = body_str in
        return (Ok ())
      | status ->
        let%bind body_str = Body.to_string body in
        return
          (Error
             (sprintf "Google API returned status: %s - %s"
                (Cohttp.Code.string_of_status status)
                body_str)))
end

let () =
  let _ = Logging.configure_logging ~level:Logging.Level.Debug () in
  Logging.Global.configure ~with_timestamp:true ()

let run () =
  let%bind provider_result =
    Server_auth_providers.Google.Google_provider.create () |> return
  in

  let%bind provider =
    match provider_result with
    | Error e ->
      Logging.Logger.error
        (Logging.Logger.get_logger "google_oauth_server")
        (sprintf "Failed to initialize Google OAuth provider: %s"
           (Error.to_string_hum e));
      exit 1
    | Ok provider -> return provider
  in

  let server =
    Server.Ox_fast_mcp.create ~name:"Google OAuth Example Server" ()
  in

  let auth_config =
    let required_scopes =
      Server_auth_providers.Google.Google_provider.required_scopes provider
    in
    {
      Ox_fast_mcp_server.Http.Auth_config.required_scopes;
      get_middleware = (fun () -> []);
      get_routes = (fun ~mcp_path:_ -> []);
      get_resource_url = (fun _ -> None);
      verify_token = Async_google_verifier.verify_token ~required_scopes;
    }
  in

  let () =
    Server.Ox_fast_mcp.add_simple_tool server ~name:"echo"
      ~description:"Echo the provided message"
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
            ]))
  in

  let () =
    Server.Ox_fast_mcp.add_simple_tool server ~name:"get_access_token_claims"
      ~description:"Get the authenticated user's access token claims"
      ~handler:(fun _args ->
        (* Context access stub *)
        return
          (`Assoc
            [
              ( "content",
                `List
                  [
                    `Assoc
                      [
                        ("type", `String "text");
                        ( "text",
                          `String "Claims access not implemented in OCaml yet"
                        );
                      ];
                  ] );
            ]))
  in

  let port = 8000 in
  Logging.Logger.info
    (Logging.Logger.get_logger "google_oauth_server")
    (sprintf "Starting server on port %d" port);
  Server.Ox_fast_mcp.run_async server ~port ~auth:auth_config ()

let () =
  Command.async ~summary:"Google OAuth Example Server"
    (Command.Param.return run)
  |> Command_unix.run
