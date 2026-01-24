open! Core

(** OAuth Flow Exceptions

    This module defines exception types for OAuth2 authentication flows. These
    exceptions are raised during various stages of the OAuth process:
    - Client registration
    - Token exchange
    - Token refresh
    - Authorization flow *)

exception Oauth_flow_error of string
(** Base exception for OAuth flow errors *)

exception Oauth_token_error of string
(** Exception raised when token operations fail (exchange or refresh) *)

exception Oauth_registration_error of string
(** Exception raised when client registration fails *)

(** Create an OAuth flow error with a message *)
let oauth_flow_error msg = Oauth_flow_error msg

(** Create an OAuth token error with a message *)
let oauth_token_error msg = Oauth_token_error msg

(** Create an OAuth registration error with a message *)
let oauth_registration_error msg = Oauth_registration_error msg
