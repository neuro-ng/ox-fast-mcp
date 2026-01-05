(** OAuth2 Middleware for HTTP Client Integration

    Provides middleware pattern for integrating OAuth2 authentication with HTTP
    clients. Uses existing oauth2.ml and utils.ml components to avoid
    duplication. *)

open Async

type oauth_middleware_state = {
  oauth_client : Oauth2.t;
  mutable in_flow : bool; (* Prevent recursive OAuth flows *)
}
(** Middleware state for OAuth flow coordination *)

(** Create middleware state *)
let create_middleware oauth_client = { oauth_client; in_flow = false }

(** Apply OAuth authentication to request headers Similar to bearer.ml's
    apply_auth but for OAuth2 *)
let apply_oauth_auth oauth_client headers =
  Oauth2.add_auth_header oauth_client headers

(** OAuth middleware wrapper for HTTP requests

    This implements the middleware pattern by:
    1. Adding auth header if token valid
    2. Sending request
    3. On 401: Triggering OAuth flow (delegated to oauth2.ml functions)
    4. Retrying with new token

    Note: Full OAuth flow is still stubbed in oauth2.ml - this provides the
    integration point *)
let oauth_middleware state next_handler request =
  (* Prevent recursive OAuth flows *)
  if state.in_flow then next_handler request
  else (
    state.in_flow <- true;

    (* TODO: Get request headers from request and add OAuth auth Current
       limitation: Generic 'request type doesn't expose headers This middleware
       is a structural placeholder showing integration point *)
    Logs.debug (fun m -> m "OAuth2 middleware: processing request");

    (* Execute request *)
    let%bind response = next_handler request in

    (* TODO: Extract status code from generic 'response For now, middleware
       passes through - full integration requires concrete request/response
       types from HTTP client library *)
    Logs.debug (fun m -> m "OAuth2 middleware: response received");

    state.in_flow <- false;
    return response)

(** Convenience function to wrap an HTTP client with OAuth middleware *)
let wrap_client_with_oauth oauth_client http_client =
  let state = create_middleware oauth_client in
  fun request -> oauth_middleware state http_client request
