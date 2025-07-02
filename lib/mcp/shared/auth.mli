(** OAuth token as defined in RFC 6749 Section 5.1 *)
type oauth_token = {
  access_token: string;
  token_type: string; (* Always "Bearer" *)
  expires_in: int option;
  scope: string option;
  refresh_token: string option;
}

(** Invalid scope error *)
exception Invalid_scope_error of string

(** Invalid redirect URI error *)
exception Invalid_redirect_uri_error of string

(** OAuth client metadata as defined in RFC 7591 Section 2 *)
type oauth_client_metadata = {
  redirect_uris: string list; (* List of valid redirect URIs *)
  token_endpoint_auth_method: [`None | `Client_secret_post];
  grant_types: [`Authorization_code | `Refresh_token] list;
  response_types: [`Code] list;
  scope: string option;
  client_name: string option;
  client_uri: string option;
  logo_uri: string option;
  contacts: string list option;
  tos_uri: string option;
  policy_uri: string option;
  jwks_uri: string option;
  jwks: Yojson.Safe.t option;
  software_id: string option;
  software_version: string option;
}

(** OAuth client information (metadata plus client credentials) *)
type oauth_client_information = {
  client_id: string;
  client_secret: string option;
  client_id_issued_at: int option;
  client_secret_expires_at: int option;
} [@@deriving fields]

(** Full OAuth client information combining metadata and credentials *)
type oauth_client_information_full = {
  metadata: oauth_client_metadata;
  info: oauth_client_information;
}

(** OAuth authorization server metadata as defined in RFC 8414 Section 2 *)
type oauth_metadata = {
  issuer: string;
  authorization_endpoint: string;
  token_endpoint: string;
  registration_endpoint: string option;
  scopes_supported: string list option;
  response_types_supported: string list;
  response_modes_supported: [`Query | `Fragment] list option;
  grant_types_supported: string list option;
  token_endpoint_auth_methods_supported: string list option;
  token_endpoint_auth_signing_alg_values_supported: unit option;
  service_documentation: string option;
  ui_locales_supported: string list option;
  op_policy_uri: string option;
  op_tos_uri: string option;
  revocation_endpoint: string option;
  revocation_endpoint_auth_methods_supported: string list option;
  revocation_endpoint_auth_signing_alg_values_supported: unit option;
  introspection_endpoint: string option;
  introspection_endpoint_auth_methods_supported: string list option;
  introspection_endpoint_auth_signing_alg_values_supported: unit option;
  code_challenge_methods_supported: string list option;
}

(** Protected resource metadata as defined in RFC 9728 Section 2 *)
type protected_resource_metadata = {
  resource: string;
  authorization_servers: string list;
  scopes_supported: string list option;
  bearer_methods_supported: string list;
  resource_documentation: string option;
}

(** Validate a scope against allowed scopes
    @param metadata The client metadata containing allowed scopes
    @param requested_scope The scope to validate
    @return The list of validated scopes
    @raise Invalid_scope_error if any requested scope is not allowed
*)
val validate_scope : oauth_client_metadata -> string option -> string list option

(** Validate a redirect URI against registered URIs
    @param metadata The client metadata containing registered redirect URIs
    @param redirect_uri The URI to validate
    @return The validated redirect URI
    @raise Invalid_redirect_uri_error if the URI is invalid or not registered
*)
val validate_redirect_uri : oauth_client_metadata -> string option -> string 