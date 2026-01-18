(** OAuth2 Client Credentials Extension - RFC 7523

    JWT bearer grant type implementation for OAuth2 client credentials flow.
    Supports both predefined JWT assertions and dynamic JWT generation.

    Translation from Python mcp/client/auth/extensions/client_credentials.py *)

open Core
open Async

(** {1 JWT Parameters} *)

type jwt_parameters = {
  assertion : string option;  (** Predefined JWT assertion *)
  issuer : string option;  (** Issuer for JWT assertions *)
  subject : string option;  (** Subject identifier *)
  audience : string option;  (** Audience for JWT assertions *)
  claims : (string * Yojson.Safe.t) list option;  (** Additional claims *)
  jwt_signing_algorithm : string option;
      (** Algorithm for signing (default: RS256) *)
  jwt_signing_key : string option;  (** Private key for JWT signing *)
  jwt_lifetime_seconds : int;  (** Lifetime of generated JWT in seconds *)
}
[@@deriving fields]
(** JWT configuration for client authentication *)

let default_jwt_parameters =
  {
    assertion = None;
    issuer = None;
    subject = None;
    audience = None;
    claims = None;
    jwt_signing_algorithm = Some "RS256";
    jwt_signing_key = None;
    jwt_lifetime_seconds = 300;
  }

(** Create JWT parameters *)
let create_jwt_parameters ?assertion ?issuer ?subject ?audience ?claims
    ?(jwt_signing_algorithm = "RS256") ?jwt_signing_key
    ?(jwt_lifetime_seconds = 300) () =
  {
    assertion;
    issuer;
    subject;
    audience;
    claims;
    jwt_signing_algorithm = Some jwt_signing_algorithm;
    jwt_signing_key;
    jwt_lifetime_seconds;
  }

(** Convert JWT parameters to assertion string

    @param with_audience_fallback Fallback audience if not specified
    @return JWT assertion string
    @raise Oauth_flow_error if required fields are missing *)
let to_assertion jwt_params ~with_audience_fallback =
  match jwt_params.assertion with
  | Some assertion ->
    (* Predefined JWT (e.g. acquired out-of-band) *)
    return (Ok assertion)
  | None -> (
    (* Generate JWT *)
    match jwt_params.jwt_signing_key with
    | None ->
      return
        (Error (Error.of_string "Missing signing key for JWT bearer grant"))
    | Some signing_key -> (
      match jwt_params.issuer with
      | None ->
        return (Error (Error.of_string "Missing issuer for JWT bearer grant"))
      | Some issuer -> (
        match jwt_params.subject with
        | None ->
          return
            (Error (Error.of_string "Missing subject for JWT bearer grant"))
        | Some subject -> (
          let audience =
            Option.value jwt_params.audience ~default:with_audience_fallback
          in
          if String.is_empty audience then
            return
              (Error (Error.of_string "Missing audience for JWT bearer grant"))
          else
            let now =
              Time_ns.to_span_since_epoch (Time_ns.now ())
              |> Time_ns.Span.to_int_sec
            in
            (* Generate UUID v4-like jti *)
            let jti =
              sprintf "%08x-%04x-4%03x-%04x-%012x" (Random.int 0x100000000)
                (Random.int 0x10000) (Random.int 0x1000) (Random.int 0x10000)
                (Random.int 0x1000000000000)
            in
            (* Build JWT claims as Yojson payload *)
            let base_claims =
              [
                ("iss", `String issuer);
                ("sub", `String subject);
                ("aud", `String audience);
                ("exp", `Int (now + jwt_params.jwt_lifetime_seconds));
                ("iat", `Int now);
                ("jti", `String jti);
              ]
            in
            (* Merge additional claims *)
            let all_claims =
              match jwt_params.claims with
              | None -> base_claims
              | Some extra ->
                let claim_map = String.Map.of_alist_exn base_claims in
                let updated_map =
                  List.fold extra ~init:claim_map ~f:(fun acc (k, v) ->
                      Map.set acc ~key:k ~data:v)
                in
                Map.to_alist updated_map
            in
            let payload = `Assoc all_claims in

            (* Determine algorithm and sign *)
            let alg =
              Option.value jwt_params.jwt_signing_algorithm ~default:"RS256"
            in
            let jwt_result =
              if String.is_prefix alg ~prefix:"HS" then
                (* HMAC symmetric key - use oct *)
                let jwk = Jose.Jwk.make_oct ~use:`Sig signing_key in
                Jose.Jwt.sign ~payload jwk
              else
                (* RSA asymmetric key - parse as PEM *)
                match Jose.Jwk.of_priv_pem ~use:`Sig signing_key with
                | Ok jwk -> Jose.Jwt.sign ~payload jwk
                | Error (`Msg msg) ->
                  Error (`Msg (sprintf "Failed to parse signing key: %s" msg))
                | Error `Unsupported_kty -> Error (`Msg "Unsupported key type")
            in

            match jwt_result with
            | Ok jwt -> return (Ok (Jose.Jwt.to_string jwt))
            | Error (`Msg msg) ->
              return
                (Error (Error.of_string (sprintf "JWT signing failed: %s" msg)))
          ))))

(** {1 RFC 7523 OAuth Client Provider} *)

type t = {
  mutable base_provider : Mcp_client_auth.Oauth2.t;  (** Base OAuth2 provider *)
  mutable jwt_parameters : jwt_parameters option;  (** JWT configuration *)
}
(** OAuth client provider for RFC 7523 (JWT bearer grants) *)

(** Create RFC 7523 OAuth client provider *)
let create ~server_url ~client_metadata ~storage ?redirect_handler
    ?callback_handler ?(timeout = 300.0) ?jwt_parameters () =
  let base_provider =
    Mcp_client_auth.Oauth2.create ~server_url ~client_metadata ~storage
      ?redirect_handler ?callback_handler ~timeout ()
  in
  { base_provider; jwt_parameters }

(** Add client authentication JWT to token data

    @param token_data Token request form data (mutable)
    @raise Oauth_token_error if JWT parameters or metadata missing *)
let add_client_authentication_jwt t ~token_data =
  match t.jwt_parameters with
  | None ->
    return
      (Error (Error.of_string "Missing JWT parameters for private_key_jwt flow"))
  | Some jwt_params -> (
    let context = Mcp_client_auth.Oauth2.get_context t.base_provider in
    match context.oauth_metadata with
    | None ->
      return
        (Error
           (Error.of_string "Missing OAuth metadata for private_key_jwt flow"))
    | Some metadata -> (
      (* Audience is the issuer identifier of the authorization server *)
      (* https://datatracker.ietf.org/doc/html/draft-ietf-oauth-rfc7523bis-01#name-updates-to-rfc-7523 *)
      let issuer = metadata.issuer in
      let%bind assertion_result =
        to_assertion jwt_params ~with_audience_fallback:issuer
      in
      match assertion_result with
      | Error e -> return (Error e)
      | Ok assertion ->
        (* RFC 7523 Section 2.2: client_assertion for private_key_jwt *)
        let token_data =
          ("client_assertion", assertion)
          :: ( "client_assertion_type",
               "urn:ietf:params:oauth:client-assertion-type:jwt-bearer" )
          :: (* Audience is the resource server *)
             ("audience", Mcp_client_auth.Oauth2.get_resource_url context)
          :: token_data
        in
        return (Ok token_data)))

(** Build token exchange request for JWT bearer grant

    @return HTTP request for token endpoint
    @raise Oauth_flow_error if client info or JWT parameters missing *)
let exchange_token_jwt_bearer t =
  let context = Mcp_client_auth.Oauth2.get_context t.base_provider in
  match context.client_info with
  | None -> return (Error (Error.of_string "Missing client info"))
  | Some _client_info -> (
    match t.jwt_parameters with
    | None -> return (Error (Error.of_string "Missing JWT parameters"))
    | Some jwt_params -> (
      match context.oauth_metadata with
      | None -> return (Error (Error.of_string "Missing OAuth metadata"))
      | Some metadata -> (
        (* Audience is the issuer identifier of the authorization server *)
        let issuer = metadata.issuer in
        let%bind assertion_result =
          to_assertion jwt_params ~with_audience_fallback:issuer
        in
        match assertion_result with
        | Error e -> return (Error e)
        | Ok assertion ->
          let token_data =
            [
              ("grant_type", "urn:ietf:params:oauth:grant-type:jwt-bearer");
              ("assertion", assertion);
            ]
          in

          (* Add resource parameter if protocol supports it *)
          let token_data =
            if
              Mcp_client_auth.Oauth2.should_include_resource_param context
                context.protocol_version
            then
              ("resource", Mcp_client_auth.Oauth2.get_resource_url context)
              :: token_data
            else token_data
          in

          (* Add scope if specified *)
          let token_data =
            match context.client_metadata.scope with
            | Some scope -> ("scope", scope) :: token_data
            | None -> token_data
          in

          (* Get token endpoint *)
          let token_url = Mcp_client_auth.Oauth2.get_token_endpoint context in

          (* Build HTTP request - STUBBED (needs HTTP client) *)
          Logs.info (fun m -> m "JWT bearer token request: POST %s" token_url);
          Logs.info (fun m ->
              m "Token data: %s"
                (String.concat ~sep:"&"
                   (List.map token_data ~f:(fun (k, v) -> sprintf "%s=%s" k v))));

          return
            (Error
               (Error.of_string
                  "HTTP request building stubbed - needs HTTP client \
                   integration")))))

(** {1 Provider Methods} *)

(** Initialize provider - load stored tokens/client info *)
let initialize t = Mcp_client_auth.Oauth2.initialize t.base_provider

(** Add authorization header to HTTP request headers *)
let add_auth_header t headers =
  Mcp_client_auth.Oauth2.add_auth_header t.base_provider headers
