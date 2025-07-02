open Core
open Lwt.Syntax
open Shared.Auth.Provider
open Shared.Auth.Settings
open Mcp.Server.Auth.Auth
open Utilities.Logging

(** JSON Web Key data structure *)
type jwk_data = {
  kty: string;
  kid: string option;
  use: string option;
  alg: string option;
  n: string option;
  e: string option;
  x5c: string list option;
  x5t: string option;
}

(** JSON Web Key Set data structure *)
type jwks_data = {
  keys: jwk_data list;
}

(** RSA Key Pair module *)
module RSA_key_pair = struct
  type t = {
    private_key: string;
    public_key: string;
  }

  let generate () =
    let key = Nocrypto.Rsa.generate 2048 in
    let priv_pem = X509.Private_key.encode_pem (`RSA key) in
    let pub_pem = X509.Public_key.encode_pem (`RSA (Nocrypto.Rsa.pub_of_priv key)) in
    { private_key = priv_pem; public_key = pub_pem }

  let create_token t ?subject:(sub="fastmcp-user") ?issuer:(iss="https://fastmcp.example.com")
      ?audience ?scopes ?expires_in_seconds:(exp=3600) ?additional_claims ?kid () =
    let now = int_of_float (Unix.time ()) in
    let claims = [
      ("iss", `String iss);
      ("sub", `String sub);
      ("iat", `Int now);
      ("exp", `Int (now + exp));
    ] in
    let claims = match audience with
      | None -> claims
      | Some aud -> ("aud", `List (List.map aud ~f:(fun a -> `String a))) :: claims
    in
    let claims = match scopes with
      | None -> claims
      | Some s -> ("scope", `String (String.concat ~sep:" " s)) :: claims
    in
    let claims = match additional_claims with
      | None -> claims
      | Some c -> c @ claims
    in
    let header = match kid with
      | None -> [("alg", `String "RS256")]
      | Some k -> [("alg", `String "RS256"); ("kid", `String k)]
    in
    Jose.sign ~header ~key:(`RSA_priv t.private_key) claims
    |> Result.ok_or_failwith
end

(** Bearer Auth Provider module *)
module Bearer_auth_provider = struct
  type t = {
    public_key: string option;
    jwks_uri: string option;
    issuer: string option;
    audience: string list option;
    required_scopes: string list option;
    mutable jwks_cache: (string * string) list;
    mutable jwks_cache_time: float;
    cache_ttl: float;
  }

  let state = ref None
  let logger = get_logger "Bearer_auth_provider"

  let get_state () =
    match !state with
    | Some s -> s
    | None -> failwith "Bearer_auth_provider not initialized"

  let extract_scopes claims =
    match Yojson.Safe.Util.member "scope" claims with
    | `String s -> String.split ~on:' ' s
    | `List l -> List.filter_map l ~f:(function
        | `String s -> Some s
        | _ -> None)
    | _ -> []

  let extract_kid token =
    try
      let parts = String.split ~on:'.' token in
      let header = Base64.decode_exn (List.hd_exn parts) in
      let json = Yojson.Safe.from_string header in
      Yojson.Safe.Util.(member "kid" json |> to_string_option)
    with _ -> None

  let get_jwks_key t kid =
    let current_time = Unix.time () in
    let* () = Lwt.return () in
    match t.jwks_uri with
    | None -> 
      let* () = logger.debug "JWKS URI not configured" in
      Lwt.fail (Failure "JWKS URI not configured")
    | Some uri ->
      if current_time -. t.jwks_cache_time < t.cache_ttl then
        match kid with
        | Some k -> 
          (match List.Assoc.find t.jwks_cache ~equal:String.equal k with
           | Some key -> Lwt.return key
           | None -> 
             let msg = Printf.sprintf "Key ID '%s' not found in cache" k in
             let* () = logger.debug "%s" msg in
             Lwt.fail (Failure msg))
        | None ->
          match t.jwks_cache with
          | [(_, key)] -> Lwt.return key
          | _ -> 
            let* () = logger.debug "No key ID provided and multiple keys in cache" in
            Lwt.fail (Failure "No key ID provided and multiple keys in cache")
      else
        let* () = logger.debug "Fetching JWKS from %s" uri in
        let* response = Cohttp_lwt_unix.Client.get (Uri.of_string uri) in
        let* body = Cohttp_lwt.Body.to_string response.body in
        let jwks = Yojson.Safe.from_string body in
        let keys = Yojson.Safe.Util.(
          member "keys" jwks |> to_list |> List.map ~f:(fun key ->
            let kid = member "kid" key |> to_string_option in
            let jwk = Jose.Jwk.of_json key in
            (Option.value kid ~default:"_default", jwk)
          )
        ) in
        t.jwks_cache <- keys;
        t.jwks_cache_time <- current_time;
        match kid with
        | Some k ->
          (match List.Assoc.find keys ~equal:String.equal k with
           | Some key -> Lwt.return key
           | None -> 
             let msg = Printf.sprintf "Key ID '%s' not found in JWKS" k in
             let* () = logger.debug "%s" msg in
             Lwt.fail (Failure msg))
        | None ->
          match keys with
          | [(_, key)] -> Lwt.return key
          | _ -> 
            let* () = logger.debug "No key ID provided and multiple keys in JWKS" in
            Lwt.fail (Failure "No key ID provided and multiple keys in JWKS")

  let get_verification_key t token =
    match t.public_key with
    | Some key -> Lwt.return key
    | None ->
      let kid = extract_kid token in
      get_jwks_key t kid

  let create ?public_key ?jwks_uri ?issuer ?audience ?required_scopes () =
    if Option.is_none public_key && Option.is_none jwks_uri then
      failwith "Either public_key or jwks_uri must be provided";
    if Option.is_some public_key && Option.is_some jwks_uri then
      failwith "Provide either public_key or jwks_uri, not both";

    let t = {
      public_key;
      jwks_uri;
      issuer;
      audience;
      required_scopes;
      jwks_cache = [];
      jwks_cache_time = 0.0;
      cache_ttl = 3600.0;
    } in
    state := Some t;

    (module struct
      let get_client _ = Lwt.return None

      let register_client _ = Lwt.fail (Failure "Client registration not supported")

      let authorize _ _ = Lwt.fail (Failure "Authorization flow not supported")

      let load_authorization_code _ _ = Lwt.return None

      let exchange_authorization_code _ _ = Lwt.fail (Failure "Authorization code exchange not supported")

      let load_refresh_token _ _ = Lwt.return None

      let exchange_refresh_token _ _ _ = Lwt.fail (Failure "Refresh token exchange not supported")

      let load_access_token token =
        let t = get_state () in
        let* key = get_verification_key t token in
        match Jose.verify ~key token with
        | Error _ -> 
          let* () = logger.debug "Token validation failed: JWT signature/format invalid" in
          Lwt.return None
        | Ok claims ->
          let client_id = 
            match Yojson.Safe.Util.(member "client_id" claims |> to_string_option) with
            | Some id -> id
            | None -> 
              match Yojson.Safe.Util.(member "sub" claims |> to_string_option) with
              | Some sub -> sub
              | None -> "unknown"
          in
          let exp = Yojson.Safe.Util.(member "exp" claims |> to_int_option) in
          let now = int_of_float (Unix.time ()) in
          
          (* Check expiration *)
          match exp with
          | Some e when e < now -> 
            let* () = logger.debug "Token validation failed: expired token for client %s" client_id in
            let* () = logger.info "Bearer token rejected for client %s" client_id in
            Lwt.return None
          | _ ->
            (* Check issuer *)
            let iss = Yojson.Safe.Util.(member "iss" claims |> to_string_option) in
            match t.issuer, iss with
            | Some expected, Some actual when expected <> actual -> 
              let* () = logger.debug "Token validation failed: issuer mismatch for client %s" client_id in
              let* () = logger.info "Bearer token rejected for client %s" client_id in
              Lwt.return None
            | _ ->
              (* Check audience *)
              let aud = Yojson.Safe.Util.(member "aud" claims |> to_list |> List.filter_map ~f:to_string_option) in
              match t.audience with
              | Some expected ->
                if List.exists aud ~f:(fun a -> List.mem expected a ~equal:String.equal) then
                  let scopes = extract_scopes claims in
                  let+ () = Lwt.return () in
                  Some {
                    token;
                    client_id;
                    scopes;
                    expires_at = exp;
                    resource = None;
                  }
                else 
                  let* () = logger.debug "Token validation failed: audience mismatch for client %s" client_id in
                  let* () = logger.info "Bearer token rejected for client %s" client_id in
                  Lwt.return None
              | None ->
                let scopes = extract_scopes claims in
                let+ () = Lwt.return () in
                Some {
                  token;
                  client_id;
                  scopes;
                  expires_at = exp;
                  resource = None;
                }

      let revoke_token _ = Lwt.fail (Failure "Token revocation not supported")

      let verify_token token = load_access_token token
    end : OAUTH_AUTHORIZATION_SERVER_PROVIDER)
end 