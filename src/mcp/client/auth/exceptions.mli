open! Core

(** OAuth Flow Exceptions

    Exception types for OAuth2 authentication flows. *)

exception Oauth_flow_error of string
(** Base exception for OAuth flow errors *)

exception Oauth_token_error of string
(** Exception raised when token operations fail (exchange or refresh) *)

exception Oauth_registration_error of string
(** Exception raised when client registration fails *)

val oauth_flow_error : string -> exn
(** Create an OAuth flow error with a message *)

val oauth_token_error : string -> exn
(** Create an OAuth token error with a message *)

val oauth_registration_error : string -> exn
(** Create an OAuth registration error with a message *)
