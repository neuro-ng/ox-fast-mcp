(** Authentication Providers for OxFastMCP.

    This module provides authentication provider implementations including:
    - TokenVerifier: Base class for token verification
    - RemoteAuthProvider: RFC 9728 protected resource metadata
    - OAuthProvider: Full OAuth Authorization Server *)

open Mcp_server_auth.Provider
open Settings

module type TOKEN_VERIFIER = TOKEN_VERIFIER
(** Token verifier module type *)

type route = {
  path : string;
  methods : string list;
  handler : Cohttp.Request.t -> (Cohttp.Response.t * Cohttp.Body.t) Lwt.t;
}
(** Route type for HTTP endpoints *)

(** Authentication provider base module type *)
module type AUTH_PROVIDER = sig
  val verify_token : string -> access_token option Lwt.t
  val base_url : string option
  val required_scopes : string list

  val get_routes : mcp_path:string option -> route list
  (** Get routes for this authentication provider.

      @param mcp_path The path where the MCP endpoint is mounted (e.g., "/mcp")
      @return List of routes for this provider *)

  val get_well_known_routes : mcp_path:string option -> route list
  (** Get well-known discovery routes (RFC 8414, RFC 9728).

      These should be mounted at root level of the application.

      @param mcp_path The path where the MCP endpoint is mounted
      @return List of well-known routes *)
end

val get_resource_url : base_url:string -> path:string option -> string
(** Get resource URL by combining base_url and path *)

val create_protected_resource_routes :
  resource_url:string ->
  authorization_servers:string list ->
  ?scopes_supported:string list ->
  ?resource_name:string ->
  ?resource_documentation:string ->
  unit ->
  route list
(** Create RFC 9728 protected resource metadata routes.

    Creates a well-known endpoint that advertises the protected resource.

    @param resource_url The URL of the protected resource
    @param authorization_servers List of authorization server URLs
    @param scopes_supported Optional list of supported scopes
    @param resource_name Optional name for the resource
    @param resource_documentation Optional documentation URL
    @return List of routes for protected resource metadata *)

(** Remote Auth Provider.

    Authentication provider for resource servers that verify tokens from known
    authorization servers. Creates RFC 9728 protected resource metadata
    endpoints. *)
module Remote_auth_provider (T : TOKEN_VERIFIER) : sig
  include AUTH_PROVIDER

  val create :
    base_url:string ->
    authorization_servers:string list ->
    ?required_scopes:string list ->
    ?resource_name:string ->
    ?resource_documentation:string ->
    unit ->
    (module AUTH_PROVIDER)
end

(** OAuth provider module type *)
module type OAUTH_PROVIDER = sig
  include OAUTH_AUTHORIZATION_SERVER_PROVIDER
  include AUTH_PROVIDER

  val create :
    issuer_url:string ->
    ?service_documentation_url:string ->
    ?client_registration_options:client_registration_options ->
    ?revocation_options:revocation_options ->
    ?required_scopes:string list ->
    unit ->
    (module OAUTH_AUTHORIZATION_SERVER_PROVIDER)
end

(** OAuth provider functor *)
module Make_oauth_provider (P : OAUTH_AUTHORIZATION_SERVER_PROVIDER) :
  OAUTH_PROVIDER
