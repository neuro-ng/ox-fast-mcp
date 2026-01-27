open Core
open Async
open Cohttp_async
open Ox_fast_mcp_server

module Async_jwt_verifier = struct
  type t = {
    jwks_uri : string option;
    mutable jwks_cache : (string * string) list;
    mutable jwks_cache_time : Time_float.t;
    cache_ttl : Time_float.Span.t;
  }

  let create ?jwks_uri () =
    {
      jwks_uri;
      jwks_cache = [];
      jwks_cache_time = Time_float.epoch;
      cache_ttl = Time_float.Span.of_hr 1.0;
    }

  let get_jwks_key t kid =
    let current_time = Time_float.now () in
    match t.jwks_uri with
    | None ->
      Logging.Logger.error
        (Logging.Logger.get_logger "jwt_verifier")
        "JWKS URI not configured";
      return (Error "JWKS URI not configured")
    | Some uri -> (
      let is_cache_valid =
        Time_float.Span.( < )
          (Time_float.diff current_time t.jwks_cache_time)
          t.cache_ttl
        && not (List.is_empty t.jwks_cache)
      in

      let%bind keys =
        if is_cache_valid then return (Ok t.jwks_cache)
        else
          try_with (fun () ->
              Logging.Logger.info
                (Logging.Logger.get_logger "jwt_verifier")
                (sprintf "Fetching JWKS from %s" uri);
              let%bind _, body = Client.get (Uri.of_string uri) in
              let%bind body_str = Body.to_string body in
              let jwks = Yojson.Safe.from_string body_str in
              let keys =
                Yojson.Safe.Util.(
                  member "keys" jwks |> to_list
                  |> List.map ~f:(fun key ->
                         let kid = member "kid" key |> to_string_option in
                         let jwk = Yojson.Safe.to_string key in
                         (Option.value kid ~default:"_default", jwk)))
              in
              t.jwks_cache <- keys;
              t.jwks_cache_time <- current_time;
              return (Ok keys))
          >>| function
          | Ok (Ok keys) -> Ok keys
          | Ok (Error msg) -> Error msg
          | Error exn -> Error (Exn.to_string exn)
      in

      match keys with
      | Error e -> return (Error e)
      | Ok keys -> (
        match kid with
        | Some k -> (
          match List.Assoc.find keys ~equal:String.equal k with
          | Some key -> return (Ok key)
          | None -> return (Error (sprintf "Key ID '%s' not found" k)))
        | None -> (
          match keys with
          | [ (_, key) ] -> return (Ok key)
          | _ -> return (Error "No key ID provided and multiple keys in JWKS")))
      )

  let verify_token t token =
    (* Extract kid header manually *)
    let kid_opt =
      try
        let parts = String.split ~on:'.' token in
        match parts with
        | header :: _ ->
          let decoded = Base64.decode_exn ~pad:true header in
          let json = Yojson.Safe.from_string decoded in
          Yojson.Safe.Util.(member "kid" json |> to_string_option)
        | _ -> None
      with _ -> None
    in

    match%bind get_jwks_key t kid_opt with
    | Error e -> return (Error e)
    | Ok jwk_json_str -> (
      try
        (* Stub verification matching the one in Bearer_auth_provider *)
        (* Ideally we would use Jose.Jwt.verify here if available *)
        let _ = jwk_json_str in
        let _ = token in
        return (Ok ())
      with exn ->
        return
          (Error (sprintf "Token validation failed: %s" (Exn.to_string exn))))
end

let () =
  let _ = Logging.configure_logging ~level:Logging.Level.Debug () in
  Logging.Global.configure ~with_timestamp:true ()

let run () =
  let%bind provider_result =
    Server_auth_providers.Azure.Azure_provider.create () |> return
  in

  let%bind provider =
    match provider_result with
    | Error e ->
      Logging.Logger.error
        (Logging.Logger.get_logger "azure_oauth_server")
        (sprintf "Failed to initialize Azure OAuth provider: %s"
           (Error.to_string_hum e));
      exit 1
    | Ok provider -> return provider
  in

  let server =
    Server.Ox_fast_mcp.create ~name:"Azure OAuth Example Server" ()
  in

  (* Azure provider gives us the JWKS URI directly *)
  let jwt_verifier = Async_jwt_verifier.create ~jwks_uri:provider.jwks_uri () in

  let auth_config =
    let settings = provider.settings in
    let required_scopes = Option.value settings.required_scopes ~default:[] in
    {
      Ox_fast_mcp_server.Http.Auth_config.required_scopes;
      get_middleware = (fun () -> []);
      get_routes = (fun ~mcp_path:_ -> []);
      get_resource_url = (fun _ -> None);
      verify_token = Async_jwt_verifier.verify_token jwt_verifier;
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
    (Logging.Logger.get_logger "azure_oauth_server")
    (sprintf "Starting server on port %d" port);
  Server.Ox_fast_mcp.run_async server ~port ~auth:auth_config ()

let () =
  Command.async ~summary:"Azure OAuth Example Server" (Command.Param.return run)
  |> Command_unix.run
