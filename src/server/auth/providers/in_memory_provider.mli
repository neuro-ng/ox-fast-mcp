open Mcp_server_auth.Provider

(** In-memory OAuth provider module *)
module In_memory_provider : sig
  include OAUTH_AUTHORIZATION_SERVER_PROVIDER

  val create :
    ?issuer_url:string ->
    ?service_documentation_url:string ->
    ?required_scopes:string list ->
    unit ->
    (module OAUTH_AUTHORIZATION_SERVER_PROVIDER)
  (** Create a new in-memory OAuth provider instance
      @param issuer_url
        The issuer URL for the provider (default: http://fastmcp.example.com)
      @param service_documentation_url Optional URL for service documentation
      @param required_scopes Optional list of required scopes
      @return A new OAUTH_AUTHORIZATION_SERVER_PROVIDER module *)
end
