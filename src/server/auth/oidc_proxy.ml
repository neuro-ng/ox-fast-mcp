(** OIDC Proxy Provider for OxFastMCP.

    This provider acts as a transparent proxy to an upstream OIDC compliant
    Authorization Server. It leverages the Oauth_proxy module to handle Dynamic
    Client Registration and forwarding of all OAuth flows.

    This implementation is based on:
    - OpenID Connect Discovery 1.0:
      https://openid.net/specs/openid-connect-discovery-1_0.html
    - OAuth 2.0 Authorization Server Metadata:
      https://datatracker.ietf.org/doc/html/rfc8414 *)

open! Core
open! Logging
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

let logger = Logger.get_logger "OIDCProxy"

(** OIDC Configuration.

    See:
    - https://openid.net/specs/openid-connect-discovery-1_0.html#ProviderMetadata
    - https://datatracker.ietf.org/doc/html/rfc8414#section-2 *)
module Oidc_configuration = struct
  type t = {
    strict : bool; [@default true]
    (* OpenID Connect Discovery 1.0 *)
    issuer : string option; [@yojson.option] (* Strict *)
    authorization_endpoint : string option; [@yojson.option] (* Strict *)
    token_endpoint : string option; [@yojson.option] (* Strict *)
    userinfo_endpoint : string option; [@yojson.option]
    jwks_uri : string option; [@yojson.option] (* Strict *)
    registration_endpoint : string option; [@yojson.option]
    scopes_supported : string list option; [@yojson.option]
    response_types_supported : string list option; [@yojson.option] (* Strict *)
    response_modes_supported : string list option; [@yojson.option]
    grant_types_supported : string list option; [@yojson.option]
    acr_values_supported : string list option; [@yojson.option]
    subject_types_supported : string list option; [@yojson.option] (* Strict *)
    id_token_signing_alg_values_supported : string list option; [@yojson.option]
        (* Strict *)
    id_token_encryption_alg_values_supported : string list option;
        [@yojson.option]
    id_token_encryption_enc_values_supported : string list option;
        [@yojson.option]
    userinfo_signing_alg_values_supported : string list option; [@yojson.option]
    userinfo_encryption_alg_values_supported : string list option;
        [@yojson.option]
    userinfo_encryption_enc_values_supported : string list option;
        [@yojson.option]
    request_object_signing_alg_values_supported : string list option;
        [@yojson.option]
    request_object_encryption_alg_values_supported : string list option;
        [@yojson.option]
    request_object_encryption_enc_values_supported : string list option;
        [@yojson.option]
    token_endpoint_auth_methods_supported : string list option; [@yojson.option]
    token_endpoint_auth_signing_alg_values_supported : string list option;
        [@yojson.option]
    display_values_supported : string list option; [@yojson.option]
    claim_types_supported : string list option; [@yojson.option]
    claims_supported : string list option; [@yojson.option]
    service_documentation : string option; [@yojson.option]
    claims_locales_supported : string list option; [@yojson.option]
    ui_locales_supported : string list option; [@yojson.option]
    claims_parameter_supported : bool option; [@yojson.option]
    request_parameter_supported : bool option; [@yojson.option]
    request_uri_parameter_supported : bool option; [@yojson.option]
    require_request_uri_registration : bool option; [@yojson.option]
    op_policy_uri : string option; [@yojson.option]
    op_tos_uri : string option; [@yojson.option]
    (* OAuth 2.0 Authorization Server Metadata *)
    revocation_endpoint : string option; [@yojson.option]
    revocation_endpoint_auth_methods_supported : string list option;
        [@yojson.option]
    revocation_endpoint_auth_signing_alg_values_supported : string list option;
        [@yojson.option]
    introspection_endpoint : string option; [@yojson.option]
    introspection_endpoint_auth_methods_supported : string list option;
        [@yojson.option]
    introspection_endpoint_auth_signing_alg_values_supported :
      string list option;
        [@yojson.option]
    code_challenge_methods_supported : string list option; [@yojson.option]
    signed_metadata : string option; [@yojson.option]
  }
  [@@deriving yojson, compare, sexp]

  (** Known field names for filtering unknown fields from OIDC configs *)
  let known_fields =
    [
      "strict";
      "issuer";
      "authorization_endpoint";
      "token_endpoint";
      "userinfo_endpoint";
      "jwks_uri";
      "registration_endpoint";
      "scopes_supported";
      "response_types_supported";
      "response_modes_supported";
      "grant_types_supported";
      "acr_values_supported";
      "subject_types_supported";
      "id_token_signing_alg_values_supported";
      "id_token_encryption_alg_values_supported";
      "id_token_encryption_enc_values_supported";
      "userinfo_signing_alg_values_supported";
      "userinfo_encryption_alg_values_supported";
      "userinfo_encryption_enc_values_supported";
      "request_object_signing_alg_values_supported";
      "request_object_encryption_alg_values_supported";
      "request_object_encryption_enc_values_supported";
      "token_endpoint_auth_methods_supported";
      "token_endpoint_auth_signing_alg_values_supported";
      "display_values_supported";
      "claim_types_supported";
      "claims_supported";
      "service_documentation";
      "claims_locales_supported";
      "ui_locales_supported";
      "claims_parameter_supported";
      "request_parameter_supported";
      "request_uri_parameter_supported";
      "require_request_uri_registration";
      "op_policy_uri";
      "op_tos_uri";
      "revocation_endpoint";
      "revocation_endpoint_auth_methods_supported";
      "revocation_endpoint_auth_signing_alg_values_supported";
      "introspection_endpoint";
      "introspection_endpoint_auth_methods_supported";
      "introspection_endpoint_auth_signing_alg_values_supported";
      "code_challenge_methods_supported";
      "signed_metadata";
    ]

  (** Filter JSON object to only include known fields *)
  let filter_known_fields json =
    match json with
    | `Assoc fields ->
      let known_set = Set.of_list (module String) known_fields in
      let filtered =
        List.filter fields ~f:(fun (k, _) -> Set.mem known_set k)
      in
      `Assoc filtered
    | other -> other

  (** Validate that a value is a URL *)
  let is_valid_url value =
    try
      let uri = Uri.of_string value in
      let scheme = Uri.scheme uri in
      match scheme with
      | Some "http" | Some "https" -> true
      | _ -> false
    with _ -> false

  (** Enforce strict validation rules *)
  let validate_strict t =
    if not t.strict then Ok t
    else
      (* Enforce URL string option fields *)
      let enforce_url attr value =
        match value with
        | None ->
          let message =
            sprintf "Missing required configuration metadata: %s" attr
          in
          Logger.error logger message;
          Error message
        | Some v when not (is_valid_url v) ->
          let message =
            sprintf "Invalid URL for configuration metadata: %s" attr
          in
          Logger.error logger message;
          Error message
        | Some _ -> Ok ()
      in
      (* Enforce presence of list option fields *)
      let enforce_list attr value =
        match value with
        | None ->
          let message =
            sprintf "Missing required configuration metadata: %s" attr
          in
          Logger.error logger message;
          Error message
        | Some _ -> Ok ()
      in
      match enforce_url "issuer" t.issuer with
      | Error e -> Error e
      | Ok () -> (
        match enforce_url "authorization_endpoint" t.authorization_endpoint with
        | Error e -> Error e
        | Ok () -> (
          match enforce_url "token_endpoint" t.token_endpoint with
          | Error e -> Error e
          | Ok () -> (
            match enforce_url "jwks_uri" t.jwks_uri with
            | Error e -> Error e
            | Ok () -> (
              match
                enforce_list "response_types_supported"
                  t.response_types_supported
              with
              | Error e -> Error e
              | Ok () -> (
                match
                  enforce_list "subject_types_supported"
                    t.subject_types_supported
                with
                | Error e -> Error e
                | Ok () -> (
                  match
                    enforce_list "id_token_signing_alg_values_supported"
                      t.id_token_signing_alg_values_supported
                  with
                  | Error e -> Error e
                  | Ok () -> Ok t))))))

  (** Create configuration from Yojson *)
  let of_yojson_strict json ~strict =
    let filtered_json = filter_known_fields json in
    match t_of_yojson filtered_json with
    | t ->
      let t = { t with strict } in
      validate_strict t
    | exception exn -> Error (Exn.to_string exn)

  (** Get the OIDC configuration from a configuration URL.

      @param config_url The OIDC config URL
      @param strict The strict flag for the configuration
      @param timeout_seconds HTTP request timeout in seconds *)
  let get_oidc_configuration ~config_url ~strict ~timeout_seconds:_ =
    let open Lwt.Syntax in
    let uri = Uri.of_string config_url in
    Lwt.catch
      (fun () ->
        let* _resp, body = Cohttp_lwt_unix.Client.get uri in
        let* body_str = Cohttp_lwt.Body.to_string body in
        let json = Yojson.Safe.from_string body_str in
        let strict_val = Option.value strict ~default:true in
        match of_yojson_strict json ~strict:strict_val with
        | Ok config -> Lwt.return_ok config
        | Error msg -> Lwt.return_error msg)
      (fun exn ->
        let msg =
          sprintf "Unable to get OIDC configuration for config url: %s - %s"
            config_url (Exn.to_string exn)
        in
        Logger.error logger msg;
        Lwt.return_error msg)
    |> Lwt_main.run
    |> (* Force synchronous for now to match Python behavior *)
    function
    | Ok config -> config
    | Error msg -> failwith msg
end

(** OIDC Proxy Provider.

    OAuth provider that wraps Oauth_proxy to provide configuration via an OIDC
    configuration URL.

    This provider makes it easier to add OAuth protection for any upstream
    provider that is OIDC compliant.

    Example:
    {[
      let auth =
        Oidc_proxy.create ~config_url:"https://oidc.config.url"
          ~client_id:"your-oidc-client-id"
          ~client_secret:"your-oidc-client-secret"
          ~base_url:"https://your.server.url" ()
      in
      (* Use auth with your OxFastMCP server *)
    ]} *)
module Oidc_proxy = struct
  type t = { oidc_config : Oidc_configuration.t; proxy : Oauth_proxy.t }

  (** Create a new OIDC proxy provider.

      @param config_url URL of upstream configuration
      @param strict Optional strict flag for the configuration
      @param client_id Client ID registered with upstream server
      @param client_secret Client secret for upstream server
      @param audience Audience for upstream server
      @param timeout_seconds HTTP request timeout in seconds
      @param token_verifier
        Optional custom token verifier. If not provided, a default will be
        created using the OIDC configuration. Cannot be used with algorithm or
        required_scopes parameters.
      @param algorithm
        Token verifier algorithm (only used if token_verifier is not provided)
      @param required_scopes
        Required scopes for token validation (only used if token_verifier is not
        provided)
      @param base_url
        Public URL where OAuth endpoints will be accessible (includes any mount
        path)
      @param issuer_url Issuer URL for OAuth metadata (defaults to base_url)
      @param redirect_path
        Redirect path configured in upstream OAuth app (defaults to
        "/auth/callback")
      @param allowed_client_redirect_uris
        List of allowed redirect URI patterns for MCP clients
      @param jwt_signing_key Secret for signing OxFastMCP JWT tokens
      @param token_endpoint_auth_method
        Token endpoint authentication method for upstream server
      @param require_authorization_consent
        Whether to require user consent before authorizing clients
      @param consent_csp_policy Content Security Policy for the consent page
      @param extra_authorize_params
        Additional parameters to forward to the upstream authorization endpoint
      @param extra_token_params
        Additional parameters to forward to the upstream token endpoint *)
  let create ~config_url ~client_id ~client_secret ~base_url ?strict ?audience
      ?timeout_seconds ?token_verifier ?algorithm ?required_scopes ?issuer_url
      ?redirect_path ?allowed_client_redirect_uris ?jwt_signing_key
      ?token_endpoint_auth_method ?(require_authorization_consent = true)
      ?consent_csp_policy ?extra_authorize_params ?extra_token_params () =
    (* Validate required parameters *)
    if String.is_empty config_url then failwith "Missing required config URL";
    if String.is_empty client_id then failwith "Missing required client id";
    if String.is_empty client_secret then
      failwith "Missing required client secret";
    if String.is_empty base_url then failwith "Missing required base URL";

    (* Validate that verifier-specific parameters are not used with custom
       verifier *)
    (match token_verifier with
    | Some _ when Option.is_some algorithm ->
      failwith
        "Cannot specify 'algorithm' when providing a custom token_verifier. \
         Configure the algorithm on your token verifier instead."
    | Some _ when Option.is_some required_scopes ->
      failwith
        "Cannot specify 'required_scopes' when providing a custom \
         token_verifier. Configure required scopes on your token verifier \
         instead."
    | _ -> ());

    (* Fetch OIDC configuration *)
    let oidc_config =
      Oidc_configuration.get_oidc_configuration ~config_url ~strict
        ~timeout_seconds
    in

    (* Validate required OIDC endpoints *)
    (match (oidc_config.authorization_endpoint, oidc_config.token_endpoint) with
    | None, _ | _, None ->
      Logger.debug logger
        (sprintf "Invalid OIDC Configuration: %s"
           (Oidc_configuration.sexp_of_t oidc_config |> Sexp.to_string_hum));
      failwith "Missing required OIDC endpoints"
    | _ -> ());

    let upstream_authorization_endpoint =
      Option.value_exn oidc_config.authorization_endpoint
    in
    let upstream_token_endpoint = Option.value_exn oidc_config.token_endpoint in

    (* Build extra params, merging audience with user-provided params *)
    let final_authorize_params =
      let base =
        match audience with
        | Some aud -> [ ("audience", aud) ]
        | None -> []
      in
      let extra = Option.value extra_authorize_params ~default:[] in
      (* User params override audience if there's a conflict *)
      List.fold extra ~init:base ~f:(fun acc (k, v) ->
          List.Assoc.add acc k v ~equal:String.equal)
    in
    let final_token_params =
      let base =
        match audience with
        | Some aud -> [ ("audience", aud) ]
        | None -> []
      in
      let extra = Option.value extra_token_params ~default:[] in
      List.fold extra ~init:base ~f:(fun acc (k, v) ->
          List.Assoc.add acc k v ~equal:String.equal)
    in

    (* Create the OAuth proxy *)
    let extra_authorize =
      if List.is_empty final_authorize_params then None
      else Some final_authorize_params
    in
    let extra_token =
      if List.is_empty final_token_params then None else Some final_token_params
    in
    let proxy =
      Oauth_proxy.create ~upstream_authorization_endpoint
        ~upstream_token_endpoint ~upstream_client_id:client_id
        ~upstream_client_secret:client_secret
        ?upstream_revocation_endpoint:oidc_config.revocation_endpoint ~base_url
        ?redirect_path ?issuer_url
        ?service_documentation_url:oidc_config.service_documentation
        ?allowed_client_redirect_uris ?jwt_signing_key
        ?token_endpoint_auth_method ~require_authorization_consent
        ?consent_csp_policy ?extra_authorize_params:extra_authorize
        ?extra_token_params:extra_token
        ~required_scopes:(Option.value required_scopes ~default:[])
        ()
    in

    (* Set token verifier if provided *)
    Option.iter token_verifier ~f:(fun verifier ->
        Oauth_proxy.set_token_verifier proxy verifier);

    { oidc_config; proxy }

  (** Get the underlying OAuth proxy for direct access *)
  let get_proxy t = t.proxy

  (** Get the OIDC configuration *)
  let get_oidc_config t = t.oidc_config

  (** Get OAuth routes for this proxy *)
  let get_routes t = Oauth_proxy.get_routes t.proxy

  (** Get client information by ID *)
  let get_client t ~client_id = Oauth_proxy.get_client t.proxy ~client_id

  (** Register a client locally with DCR *)
  let register_client t ~client_id ?client_secret ~redirect_uris ?client_name ()
      =
    Oauth_proxy.register_client t.proxy ~client_id ?client_secret ~redirect_uris
      ?client_name ()

  (** Start OAuth transaction - returns consent page URL *)
  let authorize t ~client_id ~redirect_uri ~state ~code_challenge
      ?code_challenge_method ~scopes ?resource () =
    Oauth_proxy.authorize t.proxy ~client_id ~redirect_uri ~state
      ~code_challenge ?code_challenge_method ~scopes ?resource ()

  (** Exchange authorization code for tokens *)
  let exchange_authorization_code t ~client_id ~code ~code_verifier =
    Oauth_proxy.exchange_authorization_code t.proxy ~client_id ~code
      ~code_verifier

  (** Exchange refresh token for new access token *)
  let exchange_refresh_token t ~client_id ~refresh_token ~scopes =
    Oauth_proxy.exchange_refresh_token t.proxy ~client_id ~refresh_token ~scopes

  (** Load access token *)
  let load_access_token t ~token = Oauth_proxy.load_access_token t.proxy ~token

  (** Revoke token *)
  let revoke_token t ~token = Oauth_proxy.revoke_token t.proxy ~token
end
