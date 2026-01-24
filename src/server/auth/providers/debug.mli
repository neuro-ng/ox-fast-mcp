open Mcp_server_auth.Provider

module Debug_provider : sig
  val create :
    ?validate:(string -> bool Lwt.t) ->
    ?client_id:string ->
    ?scopes:string list ->
    ?required_scopes:string list ->
    unit ->
    (module OAUTH_AUTHORIZATION_SERVER_PROVIDER)
end
