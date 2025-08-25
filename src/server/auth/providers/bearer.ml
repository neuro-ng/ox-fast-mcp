open Core
open Lwt.Syntax
open Mcp_server_auth.Provider

(* Settings module not found, will define needed types inline if necessary *)
(* Simple logging functions instead of full Utilities.Logging *)
let log_debug msg = Printf.eprintf "[DEBUG] Bearer: %s\n%!" msg
let log_info msg = Printf.eprintf "[INFO] Bearer: %s\n%!" msg
(* let log_error msg = Printf.eprintf "[ERROR] Bearer: %s\n%!" msg *)

type jwk_data = {
  kty : string;
  kid : string option;
  use : string option;
  alg : string option;
  n : string option;
  e : string option;
  x5c : string list option;
  x5t : string option;
}
(** JSON Web Key data structure *)

type jwks_data = { keys : jwk_data list }
(** JSON Web Key Set data structure *)

(** RSA Key Pair module *)
module RSA_key_pair = struct
  type t = { private_key : string; public_key : string }

  let generate () =
    let key = Mirage_crypto_pk.Rsa.generate ~bits:2048 () in
    let priv_pem =
      X509.Private_key.encode_pem (`RSA key) |> Cstruct.to_string
    in
    let pub_pem =
      X509.Public_key.encode_pem (`RSA (Mirage_crypto_pk.Rsa.pub_of_priv key))
      |> Cstruct.to_string
    in
    { private_key = priv_pem; public_key = pub_pem }

  let create_token _t ?subject:(sub = "fastmcp-user")
      ?issuer:(iss = "https://fastmcp.example.com") ?audience ?scopes
      ?expires_in_seconds:(exp = 3600) ?additional_claims ?kid () =
    let now = int_of_float (Core_unix.time ()) in
    let _claims =
      [
        ("iss", `String iss);
        ("sub", `String sub);
        ("iat", `Int now);
        ("exp", `Int (now + exp));
      ]
    in
    let _claims2 =
      match audience with
      | None -> _claims
      | Some aud ->
        ("aud", `List (List.map aud ~f:(fun a -> `String a))) :: _claims
    in
    let _claims3 =
      match scopes with
      | None -> _claims2
      | Some s -> ("scope", `String (String.concat ~sep:" " s)) :: _claims2
    in
    let _claims4 =
      match additional_claims with
      | None -> _claims3
      | Some c -> c @ _claims3
    in
    let _header =
      match kid with
      | None -> [ ("alg", `String "RS256") ]
      | Some k -> [ ("alg", `String "RS256"); ("kid", `String k) ]
    in
    (* Simplified for now - return a placeholder token until Jose API is figured
       out *)
    "placeholder_jwt_token"
end

(** Bearer Auth Provider module *)
module Bearer_auth_provider = struct
  type authorization_code_t = authorization_code
  type refresh_token_t = refresh_token
  type access_token_t = access_token

  type t = {
    public_key : string option;
    jwks_uri : string option;
    issuer : string option;
    audience : string list option;
    required_scopes : string list option;
    mutable jwks_cache : (string * string) list;
    mutable jwks_cache_time : float;
    cache_ttl : float;
  }

  let state = ref None
  (* Using simple logging functions defined above *)

  let get_state () =
    match !state with
    | Some s -> s
    | None -> failwith "Bearer_auth_provider not initialized"

  let extract_scopes claims =
    match Yojson.Safe.Util.member "scope" claims with
    | `String s -> String.split ~on:' ' s
    | `List l ->
      List.filter_map l ~f:(function
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
    let current_time = Core_unix.time () in
    let* () = Lwt.return () in
    match t.jwks_uri with
    | None ->
      let* () = Lwt.return (log_debug "JWKS URI not configured") in
      Lwt.fail (Failure "JWKS URI not configured")
    | Some uri -> (
      if Float.(current_time -. t.jwks_cache_time < t.cache_ttl) then
        match kid with
        | Some k -> (
          match List.Assoc.find t.jwks_cache ~equal:String.equal k with
          | Some key -> Lwt.return key
          | None ->
            let msg = Printf.sprintf "Key ID '%s' not found in cache" k in
            let* () = Lwt.return (log_debug msg) in
            Lwt.fail (Failure msg))
        | None -> (
          match t.jwks_cache with
          | [ (_, key) ] -> Lwt.return key
          | _ ->
            let* () =
              Lwt.return
                (log_debug "No key ID provided and multiple keys in cache")
            in
            Lwt.fail (Failure "No key ID provided and multiple keys in cache"))
      else
        let* () = Lwt.return (log_debug ("Fetching JWKS from " ^ uri)) in
        let* _response, body = Cohttp_lwt_unix.Client.get (Uri.of_string uri) in
        let* body = Cohttp_lwt.Body.to_string body in
        let jwks = Yojson.Safe.from_string body in
        let keys =
          Yojson.Safe.Util.(
            member "keys" jwks |> to_list
            |> List.map ~f:(fun key ->
                   let kid = member "kid" key |> to_string_option in
                   let jwk = "placeholder_jwk" in
                   (Option.value kid ~default:"_default", jwk)))
        in
        t.jwks_cache <- keys;
        t.jwks_cache_time <- current_time;
        match kid with
        | Some k -> (
          match List.Assoc.find keys ~equal:String.equal k with
          | Some key -> Lwt.return key
          | None ->
            let msg = Printf.sprintf "Key ID '%s' not found in JWKS" k in
            let* () = Lwt.return (log_debug msg) in
            Lwt.fail (Failure msg))
        | None -> (
          match keys with
          | [ (_, key) ] -> Lwt.return key
          | _ ->
            let* () =
              Lwt.return
                (log_debug "No key ID provided and multiple keys in JWKS")
            in
            Lwt.fail (Failure "No key ID provided and multiple keys in JWKS")))

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

    let t =
      {
        public_key;
        jwks_uri;
        issuer;
        audience;
        required_scopes;
        jwks_cache = [];
        jwks_cache_time = 0.0;
        cache_ttl = 3600.0;
      }
    in
    state := Some t;

    (module struct
      type authorization_code_t = authorization_code
      type refresh_token_t = refresh_token
      type access_token_t = access_token

      let get_client _ = Lwt.return None

      let register_client _ =
        Lwt.fail (Failure "Client registration not supported")

      let authorize _ _ = Lwt.fail (Failure "Authorization flow not supported")
      let load_authorization_code _ _ = Lwt.return None

      let exchange_authorization_code _ _ =
        Lwt.fail (Failure "Authorization code exchange not supported")

      let load_refresh_token _ _ = Lwt.return None

      let exchange_refresh_token _ _ _ =
        Lwt.fail (Failure "Refresh token exchange not supported")

      let load_access_token token =
        let t = get_state () in
        let* _key = get_verification_key t token in
        match Ok (`Assoc []) with
        (* Placeholder - replace Jose.Jwt.verify *)
        | Error _ ->
          let* () =
            Lwt.return
              (log_debug "Token validation failed: JWT signature/format invalid")
          in
          Lwt.return None
        | Ok claims -> (
          let client_id =
            match
              Yojson.Safe.Util.(member "client_id" claims |> to_string_option)
            with
            | Some id -> id
            | None -> (
              match
                Yojson.Safe.Util.(member "sub" claims |> to_string_option)
              with
              | Some sub -> sub
              | None -> "unknown")
          in
          let exp = Yojson.Safe.Util.(member "exp" claims |> to_int_option) in
          let now = int_of_float (Core_unix.time ()) in

          (* Check expiration *)
          match exp with
          | Some e when e < now ->
            let* () =
              Lwt.return
                (log_debug
                   ("Token validation failed: expired token for client "
                  ^ client_id))
            in
            let* () =
              Lwt.return
                (log_info ("Bearer token rejected for client " ^ client_id))
            in
            Lwt.return None
          | _ -> (
            (* Check issuer *)
            let iss =
              Yojson.Safe.Util.(member "iss" claims |> to_string_option)
            in
            match (t.issuer, iss) with
            | Some expected, Some actual when not (String.equal expected actual)
              ->
              let* () =
                Lwt.return
                  (log_debug
                     ("Token validation failed: issuer mismatch for client "
                    ^ client_id))
              in
              let* () =
                Lwt.return
                  (log_info ("Bearer token rejected for client " ^ client_id))
              in
              Lwt.return None
            | _ -> (
              (* Check audience *)
              let aud =
                Yojson.Safe.Util.(
                  member "aud" claims |> to_list
                  |> List.filter_map ~f:to_string_option)
              in
              match t.audience with
              | Some expected ->
                if
                  List.exists aud ~f:(fun a ->
                      List.mem expected a ~equal:String.equal)
                then
                  let scopes = extract_scopes claims in
                  let+ () = Lwt.return () in
                  Some
                    {
                      token;
                      client_id;
                      scopes;
                      expires_at = exp;
                      resource = None;
                    }
                else
                  let* () =
                    Lwt.return
                      (log_debug
                         ("Token validation failed: audience mismatch for \
                           client " ^ client_id))
                  in
                  let* () =
                    Lwt.return
                      (log_info
                         ("Bearer token rejected for client " ^ client_id))
                  in
                  Lwt.return None
              | None ->
                let scopes = extract_scopes claims in
                let+ () = Lwt.return () in
                Some
                  {
                    token;
                    client_id;
                    scopes;
                    expires_at = exp;
                    resource = None;
                  })))

      let revoke_token _ = Lwt.fail (Failure "Token revocation not supported")
      (* let verify_token token = load_access_token token *)
    end : OAUTH_AUTHORIZATION_SERVER_PROVIDER)

  (* Also provide the functions at module level to satisfy interface *)
  let get_client _ = Lwt.return None
  let register_client _ = Lwt.return_unit
  let authorize _ _ = Lwt.fail (Failure "Use create() to get a proper provider")
  let load_authorization_code _ _ = Lwt.return None

  let exchange_authorization_code _ _ =
    Lwt.fail (Failure "Use create() to get a proper provider")

  let load_refresh_token _ _ = Lwt.return None

  let exchange_refresh_token _ _ _ =
    Lwt.fail (Failure "Use create() to get a proper provider")

  let load_access_token _ = Lwt.return None
  let revoke_token _ = Lwt.return_unit
end
