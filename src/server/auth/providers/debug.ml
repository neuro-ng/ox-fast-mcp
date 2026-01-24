open Core
open Lwt.Syntax
open Mcp_server_auth.Provider
open! Logging

let logger = Logger.get_logger "ox-fast-mcp.server.auth.providers.debug"

module Debug_provider = struct
  let create ?(validate = fun _ -> Lwt.return true)
      ?(client_id = "debug-client") ?(scopes = []) ?(required_scopes = []) () =
    (module struct
      type authorization_code_t = authorization_code
      type refresh_token_t = refresh_token
      type access_token_t = access_token

      let get_client _ = Lwt.return None
      let register_client _ = Lwt.return_unit

      let authorize _ _ =
        Lwt.fail (Failure "Debug provider does not support authorization flow")

      let load_authorization_code _ _ = Lwt.return None

      let exchange_authorization_code _ _ =
        Lwt.fail (Failure "Debug provider does not support code exchange")

      let load_refresh_token _ _ = Lwt.return None

      let exchange_refresh_token _ _ _ =
        Lwt.fail (Failure "Debug provider does not support refresh tokens")

      let revoke_token _ = Lwt.return_unit

      let load_access_token token =
        (* Reject empty tokens *)
        if String.is_empty (String.strip token) then
          let* () = Lwt.return (Logger.debug logger "Rejecting empty token") in
          Lwt.return None
        else
          Lwt.catch
            (fun () ->
              let* is_valid = validate token in
              if not is_valid then
                let* () =
                  Lwt.return
                    (Logger.debug logger
                       "Token validation failed: callable returned False")
                in
                Lwt.return None
              else
                (* Validate required scopes if any *)
                (* Note: Python implementation passes required_scopes to super().__init__ but 
                    doesn't explicitly check them in verify_token before returning AccessToken.
                    However, AccessToken object usually carries scopes. 
                    The Python implementation assigns self.scopes to the token. *)
                (* Create AccessToken *)
                (* We map 'scopes' param to the token's scopes. *)
                let effective_scopes =
                  scopes @ required_scopes
                  |> List.dedup_and_sort ~compare:String.compare
                in

                Lwt.return
                  (Some
                     {
                       token;
                       client_id;
                       scopes = effective_scopes;
                       expires_at = None;
                       resource = None;
                       (* claims = ... not easily representable in current
                          access_token? The OCaml access_token type doesn't seem
                          to have a generic claims map based on bearer.ml usage.
                          checking bearer.ml, it returns access_token record. *)
                     }))
            (fun exn ->
              let* () =
                Lwt.return
                  (Logger.debug logger
                     (sprintf "Token validation error: %s" (Exn.to_string exn)))
              in
              Lwt.return None)
    end : OAUTH_AUTHORIZATION_SERVER_PROVIDER)
end
