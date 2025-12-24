(** OIDC Proxy Provider for OxFastMCP.

    This provider acts as a transparent proxy to an upstream OIDC compliant
    Authorization Server. It leverages the Oauth_proxy module to handle Dynamic
    Client Registration and forwarding of all OAuth flows.

    This implementation is based on:
    - OpenID Connect Discovery 1.0:
      https://openid.net/specs/openid-connect-discovery-1_0.html
    - OAuth 2.0 Authorization Server Metadata:
      https://datatracker.ietf.org/doc/html/rfc8414 *)

(** OIDC Configuration.

    See:
    - https://openid.net/specs/openid-connect-discovery-1_0.html#ProviderMetadata
    - https://datatracker.ietf.org/doc/html/rfc8414#section-2 *)
module Oidc_configuration : sig
  type t = {
    strict : bool;
    (* OpenID Connect Discovery 1.0 *)
    issuer : string option;
    authorization_endpoint : string option;
    token_endpoint : string option;
    userinfo_endpoint : string option;
    jwks_uri : string option;
    registration_endpoint : string option;
    scopes_supported : string list option;
    response_types_supported : string list option;
    response_modes_supported : string list option;
    grant_types_supported : string list option;
    acr_values_supported : string list option;
    subject_types_supported : string list option;
    id_token_signing_alg_values_supported : string list option;
    id_token_encryption_alg_values_supported : string list option;
    id_token_encryption_enc_values_supported : string list option;
    userinfo_signing_alg_values_supported : string list option;
    userinfo_encryption_alg_values_supported : string list option;
    userinfo_encryption_enc_values_supported : string list option;
    request_object_signing_alg_values_supported : string list option;
    request_object_encryption_alg_values_supported : string list option;
    request_object_encryption_enc_values_supported : string list option;
    token_endpoint_auth_methods_supported : string list option;
    token_endpoint_auth_signing_alg_values_supported : string list option;
    display_values_supported : string list option;
    claim_types_supported : string list option;
    claims_supported : string list option;
    service_documentation : string option;
    claims_locales_supported : string list option;
    ui_locales_supported : string list option;
    claims_parameter_supported : bool option;
    request_parameter_supported : bool option;
    request_uri_parameter_supported : bool option;
    require_request_uri_registration : bool option;
    op_policy_uri : string option;
    op_tos_uri : string option;
    (* OAuth 2.0 Authorization Server Metadata *)
    revocation_endpoint : string option;
    revocation_endpoint_auth_methods_supported : string list option;
    revocation_endpoint_auth_signing_alg_values_supported : string list option;
    introspection_endpoint : string option;
    introspection_endpoint_auth_methods_supported : string list option;
    introspection_endpoint_auth_signing_alg_values_supported :
      string list option;
    code_challenge_methods_supported : string list option;
    signed_metadata : string option;
  }
  [@@deriving yojson, compare, sexp]

  val validate_strict : t -> (t, string) result
  (** Validate that strict fields are present and valid URLs *)

  val of_yojson_strict : Yojson.Safe.t -> strict:bool -> (t, string) result
  (** Create configuration from Yojson with strict validation *)

  val get_oidc_configuration :
    config_url:string -> strict:bool option -> timeout_seconds:int option -> t
  (** Get the OIDC configuration from a configuration URL.

      @param config_url The OIDC config URL
      @param strict The strict flag for the configuration
      @param timeout_seconds HTTP request timeout in seconds *)
end

(** OIDC Proxy Provider.

    OAuth provider that wraps Oauth_proxy to provide configuration via an OIDC
    configuration URL. *)
module Oidc_proxy : sig
  type t

  val create :
    config_url:string ->
    client_id:string ->
    client_secret:string ->
    base_url:string ->
    ?strict:bool ->
    ?audience:string ->
    ?timeout_seconds:int ->
    ?token_verifier:(module Oauth_proxy.TOKEN_VERIFIER) ->
    ?algorithm:string ->
    ?required_scopes:string list ->
    ?issuer_url:string ->
    ?redirect_path:string ->
    ?allowed_client_redirect_uris:string list ->
    ?jwt_signing_key:string ->
    ?token_endpoint_auth_method:string ->
    ?require_authorization_consent:bool ->
    ?consent_csp_policy:string ->
    ?extra_authorize_params:(string * string) list ->
    ?extra_token_params:(string * string) list ->
    unit ->
    t
  (** Create a new OIDC proxy provider.

      @param config_url URL of upstream configuration
      @param strict Optional strict flag for the configuration
      @param client_id Client ID registered with upstream server
      @param client_secret Client secret for upstream server
      @param audience Audience for upstream server
      @param timeout_seconds HTTP request timeout in seconds
      @param token_verifier
        Optional custom token verifier. If not provided, a default will be
        created using the OIDC configuration.
      @param algorithm
        Token verifier algorithm (only used if token_verifier is not provided)
      @param required_scopes
        Required scopes for token validation (only used if token_verifier is not
        provided)
      @param base_url Public URL where OAuth endpoints will be accessible
      @param issuer_url Issuer URL for OAuth metadata (defaults to base_url)
      @param redirect_path Redirect path configured in upstream OAuth app
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

  val get_proxy : t -> Oauth_proxy.t
  (** Get the underlying OAuth proxy *)

  val get_oidc_config : t -> Oidc_configuration.t
  (** Get the OIDC configuration *)

  val get_routes : t -> Oauth_proxy.route list
  (** Get OAuth routes for this proxy *)

  val get_client :
    t -> client_id:string -> Oauth_proxy.proxy_dcr_client option Lwt.t
  (** Get client information by ID *)

  val register_client :
    t ->
    client_id:string ->
    ?client_secret:string ->
    redirect_uris:string list ->
    ?client_name:string ->
    unit ->
    Oauth_proxy.proxy_dcr_client Lwt.t
  (** Register a client locally with DCR *)

  val authorize :
    t ->
    client_id:string ->
    redirect_uri:string ->
    state:string ->
    code_challenge:string option ->
    ?code_challenge_method:string ->
    scopes:string list ->
    ?resource:string ->
    unit ->
    string Lwt.t
  (** Start OAuth transaction - returns consent page URL *)

  val exchange_authorization_code :
    t ->
    client_id:string ->
    code:string ->
    code_verifier:string ->
    (Yojson.Safe.t, string) result Lwt.t
  (** Exchange authorization code for tokens *)

  val exchange_refresh_token :
    t ->
    client_id:string ->
    refresh_token:string ->
    scopes:string list ->
    (Yojson.Safe.t, string) result Lwt.t
  (** Exchange refresh token for new access token *)

  val load_access_token :
    t -> token:string -> Mcp_server_auth.Provider.access_token option Lwt.t
  (** Load access token *)

  val revoke_token : t -> token:string -> unit Lwt.t
  (** Revoke token *)
end
