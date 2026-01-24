(** HeadlessOAuth - OAuth Client for Automated Testing

    Provides OAuth client that bypasses browser interaction for testing. *)

type t
(** Headless OAuth configuration *)

val create :
  server_url:string -> mock_auth_code:string -> ?mock_state:string -> unit -> t
(** Create headless OAuth configuration.

    @param server_url OAuth server URL
    @param mock_auth_code Authorization code to return (simulates user auth)
    @param mock_state State token to return (default: test_state_12345) *)

val create_oauth_client :
  t ->
  client_metadata:Mcp_shared.Auth.oauth_client_metadata ->
  Mcp_client_auth.Oauth2.t
(** Create OAuth2 client from headless configuration *)

val create_with_defaults :
  server_url:string -> mock_auth_code:string -> unit -> Mcp_client_auth.Oauth2.t
(** Create OAuth2 client with default test metadata *)

val default_client_metadata :
  ?scopes:string list ->
  ?client_name:string ->
  unit ->
  Mcp_shared.Auth.oauth_client_metadata
(** Create default client metadata for testing *)

(** In-memory token storage for tests *)
module Test_storage : sig
  type t

  val create : unit -> t

  include Mcp_client_auth.Oauth2.Token_storage with type t := t
end
