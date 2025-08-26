open Mcp_server_auth.Provider
open Settings

(** OAuth provider base module *)
module Base_oauth_provider = struct
  type t = {
    issuer_url : string;
    service_documentation_url : string option;
    client_registration_options : client_registration_options option;
    revocation_options : revocation_options option;
    required_scopes : string list option;
  }

  let create ~issuer_url ?service_documentation_url ?client_registration_options
      ?revocation_options ?required_scopes () =
    {
      issuer_url;
      service_documentation_url;
      client_registration_options;
      revocation_options;
      required_scopes;
    }
end

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

  val verify_token : string -> access_token option Lwt.t
end

(** OAuth provider functor *)
module Make_oauth_provider (P : OAUTH_AUTHORIZATION_SERVER_PROVIDER) :
  OAUTH_PROVIDER = struct
  include P

  let create ~issuer_url ?service_documentation_url ?client_registration_options
      ?revocation_options ?required_scopes () =
    let _state =
      Base_oauth_provider.create ~issuer_url ?service_documentation_url
        ?client_registration_options ?revocation_options ?required_scopes ()
    in
    (module struct
      include P
    end : OAUTH_AUTHORIZATION_SERVER_PROVIDER)

  let verify_token token = load_access_token token
end
