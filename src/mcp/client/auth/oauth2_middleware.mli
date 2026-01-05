(** OAuth2 Middleware for HTTP Client Integration *)

open Async

type oauth_middleware_state
(** Middleware state for OAuth flow coordination *)

val create_middleware : Oauth2.t -> oauth_middleware_state
(** Create middleware state from OAuth2 client *)

val apply_oauth_auth :
  Oauth2.t -> (string * string) list -> (string * string) list
(** Apply OAuth2 authentication to request headers *)

val oauth_middleware :
  oauth_middleware_state ->
  ('request -> 'response Deferred.t) ->
  'request ->
  'response Deferred.t
(** OAuth middleware for HTTP requests

    Wraps HTTP request handler to provide automatic OAuth2 authentication:
    - Adds auth header if token valid
    - Handles 401 Unauthorized (triggers OAuth flow)
    - Handles 403 Forbidden with insufficient_scope

    Usage:
    {[
      let oauth_client = Oauth2.create ~server_url ~client_metadata ~storage () in
      let middleware_state = create_middleware oauth_client in
      let authenticated_response = oauth_middleware middleware_state http_client request
    ]}

    Note: Full OAuth flow coordination is still in development *)

val wrap_client_with_oauth :
  Oauth2.t ->
  ('request -> 'response Deferred.t) ->
  ('request -> 'response Deferred.t)
(** Convenience wrapper to create OAuth-authenticated HTTP client *)
