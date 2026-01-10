(** OAuth2 Middleware for HTTP Client Integration

    Provides middleware pattern for integrating OAuth2 authentication with HTTP
    clients. Implements automatic 401/403 handling and token refresh. *)

open Core
open Async

type oauth_middleware_state = {
  oauth_client : Oauth2.t;
  mutable in_flow : bool; (* Prevent recursive OAuth flows *)
}
(** Middleware state for OAuth flow coordination *)

(** Create middleware state *)
let create_middleware oauth_client = { oauth_client; in_flow = false }

(** Apply OAuth authentication to request headers *)
let apply_oauth_auth oauth_client headers =
  Oauth2.add_auth_header oauth_client headers

(** Handle 401 Unauthorized - Delegates to Oauth2 module *)
let handle_401_unauthorized oauth_client response =
  let%bind result = Oauth2.handle_unauthorized_response oauth_client response in
  match result with
  | Ok () ->
    Logs.info (fun m -> m "OAuth flow completed successfully");
    return (Ok `Retry_with_auth)
  | Error err -> return (Error err)

(** Handle 403 Forbidden with insufficient_scope - Delegates to Oauth2 module *)
let handle_403_forbidden oauth_client response =
  let%bind result =
    Oauth2.handle_insufficient_scope_response oauth_client response
  in
  match result with
  | Ok () ->
    Logs.info (fun m -> m "Re-authorization completed successfully");
    return (Ok `Retry_with_auth)
  | Error err -> return (Error err)

(** OAuth middleware for HTTP requests

    Wraps HTTP request handler to provide automatic OAuth2 authentication:
    - Checks and refreshes expired tokens
    - Adds auth header if token valid
    - Handles 401 Unauthorized (triggers full OAuth flow)
    - Handles 403 Forbidden with insufficient_scope *)
let oauth_middleware state next_handler request =
  (* Prevent recursive OAuth flows *)
  if state.in_flow then next_handler request
  else (
    state.in_flow <- true;

    Monitor.try_with (fun () ->
        (* 1. Check if token refresh needed *)
        let context = Oauth2.get_context state.oauth_client in
        let%bind () =
          if
            Oauth2.can_refresh_token context
            && not (Oauth2.is_token_valid context)
          then (
            Logs.info (fun m -> m "Refreshing expired OAuth token");
            let%bind _refresh_result = Oauth2.refresh_access_token context in
            (* Token is already updated in context by refresh_access_token *)
            return ())
          else return ()
        in

        (* 2. Add auth header if we have tokens *)
        let request_headers = Cohttp.Request.headers request in
        let headers_list = Cohttp.Header.to_list request_headers in
        let headers_with_auth =
          apply_oauth_auth state.oauth_client headers_list
        in
        let new_headers =
          List.fold headers_with_auth ~init:(Cohttp.Header.init ())
            ~f:(fun h (k, v) -> Cohttp.Header.add h k v)
        in
        let request_with_auth =
          { request with Cohttp.Request.headers = new_headers }
        in

        (* 3. Send request *)
        let%bind response, body = next_handler request_with_auth in

        (* 4. Handle auth responses *)
        let status = Cohttp.Response.status response in
        match status with
        | `Unauthorized -> (
          Logs.info (fun m ->
              m "Received 401 Unauthorized, starting OAuth flow");
          (* 401: Full OAuth flow *)
          let%bind retry_result =
            handle_401_unauthorized state.oauth_client response
          in
          match retry_result with
          | Ok `Retry_with_auth ->
            (* Retry with new token *)
            let headers_list = Cohttp.Header.to_list request_headers in
            let headers_with_new_auth =
              apply_oauth_auth state.oauth_client headers_list
            in
            let new_headers =
              List.fold headers_with_new_auth ~init:(Cohttp.Header.init ())
                ~f:(fun h (k, v) -> Cohttp.Header.add h k v)
            in
            let request_with_new_auth =
              { request with Cohttp.Request.headers = new_headers }
            in
            Logs.info (fun m -> m "Retrying request with new OAuth token");
            next_handler request_with_new_auth
          | Error err ->
            Logs.warn (fun m -> m "OAuth flow failed: %s" err);
            (* Return original 401 response *)
            return (response, body))
        | `Forbidden ->
          (* Check for insufficient_scope *)
          let has_scope_error =
            Option.is_some (Utils.extract_scope_from_www_auth response)
          in
          if has_scope_error then (
            Logs.info (fun m ->
                m "Received 403 Forbidden with insufficient scope");
            let%bind retry_result =
              handle_403_forbidden state.oauth_client response
            in
            match retry_result with
            | Ok `Retry_with_auth ->
              let headers_list = Cohttp.Header.to_list request_headers in
              let headers_with_new_auth =
                apply_oauth_auth state.oauth_client headers_list
              in
              let new_headers =
                List.fold headers_with_new_auth ~init:(Cohttp.Header.init ())
                  ~f:(fun h (k, v) -> Cohttp.Header.add h k v)
              in
              let request_with_new_auth =
                { request with Cohttp.Request.headers = new_headers }
              in
              Logs.info (fun m -> m "Retrying request with updated scope");
              next_handler request_with_new_auth
            | Error err ->
              Logs.warn (fun m -> m "Scope re-authorization failed: %s" err);
              return (response, body))
          else return (response, body)
        | _ -> return (response, body))
    >>= function
    | Ok result ->
      state.in_flow <- false;
      return result
    | Error exn ->
      state.in_flow <- false;
      raise exn)

(** Convenience function to wrap an HTTP client with OAuth middleware *)
let wrap_client_with_oauth oauth_client http_client =
  let state = create_middleware oauth_client in
  fun request -> oauth_middleware state http_client request
