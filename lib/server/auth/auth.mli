open Server.Auth.Provider
open Settings

(** OAuth provider module type *)
module type OAUTH_PROVIDER = sig
  include OAUTH_AUTHORIZATION_SERVER_PROVIDER

  val create :
    issuer_url:string ->
    ?service_documentation_url:string ->
    ?client_registration_options:client_registration_options ->
    ?revocation_options:revocation_options ->
    ?required_scopes:string list ->
    unit ->
    (module OAUTH_AUTHORIZATION_SERVER_PROVIDER)
  (** Create a new OAuth provider instance
      @param issuer_url The URL of the OAuth issuer
      @param service_documentation_url The URL of the service documentation
      @param client_registration_options The client registration options
      @param revocation_options The revocation options
      @param required_scopes Scopes that are required for all requests
      @return A new OAUTH_AUTHORIZATION_SERVER_PROVIDER module *)

  val verify_token : string -> access_token option Lwt.t
  (** Verify a bearer token and return access info if valid
      @param token The token string to validate
      @return AccessToken object if valid, None if invalid or expired *)
end
