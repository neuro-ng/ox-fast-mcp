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
  (Cohttp.Request.t -> (Cohttp.Response.t * Cohttp_async.Body.t) Deferred.t) ->
  Cohttp.Request.t ->
  (Cohttp.Response.t * Cohttp_async.Body.t) Deferred.t
(** OAuth middleware for HTTP requests

    Wraps HTTP request handler to provide automatic OAuth2 authentication:
    - Checks and refreshes expired tokens automatically
    - Adds Authorization header if token is valid
    - Handles 401 Unauthorized (triggers full OAuth flow)
    - Handles 403 Forbidden with insufficient_scope (re-authorizes)

    Usage:
    {[
      let oauth_client = Oauth2.create ~server_url ~client_metadata ~storage () in
      let middleware_state = create_middleware oauth_client in
      let authenticated_client = oauth_middleware middleware_state http_client
    ]}

    The middleware will automatically:
    1. Discover OAuth metadata when receiving 401
    2. Register client (DCR or CIMD)
    3. Perform authorization code flow with PKCE
    4. Exchange code for tokens
    5. Retry original request with token
    6. Handle scope updates on 403 with insufficient_scope *)

val wrap_client_with_oauth :
  Oauth2.t ->
  (Cohttp.Request.t -> (Cohttp.Response.t * Cohttp_async.Body.t) Deferred.t) ->
  (Cohttp.Request.t -> (Cohttp.Response.t * Cohttp_async.Body.t) Deferred.t)
(** Convenience wrapper to create OAuth-authenticated HTTP client *)
