open Mcp_server_auth.Provider
open Settings

module type TOKEN_VERIFIER = TOKEN_VERIFIER
(** Token verifier module type *)

(** Authentication provider base module type *)
module type AUTH_PROVIDER = sig
  val verify_token : string -> access_token option Lwt.t
  val base_url : string option
  val get_routes : unit -> unit list
end

(** Remote Auth Provider *)
module Remote_auth_provider (T : TOKEN_VERIFIER) : sig
  include AUTH_PROVIDER

  val create :
    base_url:string ->
    authorization_servers:string list ->
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
