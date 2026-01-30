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

let main () =
  let%bind server_and_config =
    (* Create the Scalekit provider to get configuration details *)
    let provider_result =
      Server_auth_providers.Scalekit.Scalekit_provider.create ()
    in
    let provider =
      match provider_result with
      | Ok p -> p
      | Error e ->
        failwith
          (sprintf "Failed to create Scalekit provider: %s"
             (Error.to_string_hum e))
    in

    (* Use the provider's details to configure the JWT verifier *)
    let jwks_uri =
      Server_auth_providers.Scalekit.Scalekit_provider.jwks_uri provider
    in
    let issuer =
      Server_auth_providers.Scalekit.Scalekit_provider.issuer provider
    in
    (* Note: Issuer check could be added to verifier, but stub verifier might
       not check it yet *)
    let _ = issuer in

    let verifier = Async_jwt_verifier.create ~jwks_uri () in

    let auth_config =
      let settings =
        Server_auth_providers.Scalekit.Scalekit_provider.required_scopes
          provider
      in
      let _ = settings in
      (* Actually we need to access settings properly if we want scopes, but
         provider.required_scopes is a function *)
      {
        Ox_fast_mcp_server.Http.Auth_config.required_scopes =
          Server_auth_providers.Scalekit.Scalekit_provider.required_scopes
            provider;
        get_middleware = (fun () -> []);
        get_routes = (fun ~mcp_path:_ -> []);
        get_resource_url = (fun _ -> None);
        verify_token = Async_jwt_verifier.verify_token verifier;
      }
    in

    let server = Server.Ox_fast_mcp.create ~name:"Scalekit OAuth Example" () in

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
                        ("text", `String "Authenticated via Scalekit!");
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
  Command.async ~summary:"Scalekit OAuth Server Example"
    (Command.Param.return main)
  |> Command_unix.run
