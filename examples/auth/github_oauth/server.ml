open Core
open Async
open Cohttp_async
open Ox_fast_mcp_server

module Async_github_verifier = struct
  let user_agent = "OxFastMCP-GitHub-OAuth"
  let github_user_api = "https://api.github.com/user"

  let verify_token ~required_scopes:_ token =
    let headers =
      Cohttp.Header.of_list
        [
          ("Authorization", "Bearer " ^ token);
          ("User-Agent", user_agent);
          ("Accept", "application/json");
        ]
    in
    match%bind
      try_with (fun () -> Client.get ~headers (Uri.of_string github_user_api))
    with
    | Error exn ->
      return
        (Error (sprintf "GitHub API request failed: %s" (Exn.to_string exn)))
    | Ok (response, body) -> (
      match Cohttp.Response.status response with
      | `OK ->
        (* thorough scope check would parse X-OAuth-Scopes header *)
        (* let scopes_header = Cohttp.Header.get (Cohttp.Response.headers response) "x-oauth-scopes" in *)
        (* For now, we assume if GitHub accepts the token for /user, it's a valid user token *)
        let%bind body_str = Body.to_string body in
        let _ = body_str in
        (* could parse user info here *)
        return (Ok ())
      | status ->
        return
          (Error
             (sprintf "GitHub API returned status: %s"
                (Cohttp.Code.string_of_status status))))
end

let () =
  let _ = Logging.configure_logging ~level:Logging.Level.Debug () in
  Logging.Global.configure ~with_timestamp:true ()

let run () =
  let%bind provider_result =
    Server_auth_providers.Github.Github_provider.create () |> return
  in

  let%bind provider =
    match provider_result with
    | Error e ->
      Logging.Logger.error
        (Logging.Logger.get_logger "github_oauth_server")
        (sprintf "Failed to initialize GitHub OAuth provider: %s"
           (Error.to_string_hum e));
      exit 1
    | Ok provider -> return provider
  in

  let server =
    Server.Ox_fast_mcp.create ~name:"GitHub OAuth Example Server" ()
  in

  let auth_config =
    let required_scopes =
      Server_auth_providers.Github.Github_provider.required_scopes provider
    in
    {
      Ox_fast_mcp_server.Http.Auth_config.required_scopes;
      get_middleware = (fun () -> []);
      get_routes = (fun ~mcp_path:_ -> []);
      get_resource_url = (fun _ -> None);
      verify_token = Async_github_verifier.verify_token ~required_scopes;
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
    (Logging.Logger.get_logger "github_oauth_server")
    (sprintf "Starting server on port %d" port);
  Server.Ox_fast_mcp.run_async server ~port ~auth:auth_config ()

let () =
  Command.async ~summary:"GitHub OAuth Example Server"
    (Command.Param.return run)
  |> Command_unix.run
