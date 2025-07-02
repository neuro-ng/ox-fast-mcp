open Core
open Shared.Auth.Provider

(** Settings for the Bearer Auth Provider loaded from environment variables *)
module Bearer_settings : sig
  type t = {
    public_key : string option;  (** RSA public key in PEM format (for static key) *)
    jwks_uri : string option;  (** URI to fetch keys from (for key rotation) *)
    issuer : string option;  (** Expected issuer claim *)
    audience : string list option;  (** Expected audience claim *)
    required_scopes : string list option;  (** List of required scopes for access *)
  }

  (** Load settings from environment variables with prefix FASTMCP_AUTH_BEARER_ *)
  val load : unit -> t
end

(** Environment-based Bearer Auth Provider module *)
module Env_bearer_auth_provider : sig
  include OAUTH_AUTHORIZATION_SERVER_PROVIDER

  (** Create a new Environment Bearer Auth Provider.
      Settings are loaded from environment variables with prefix FASTMCP_AUTH_BEARER_.
      Provided parameters take precedence over environment variables.
      @param public_key RSA public key in PEM format (for static key)
      @param jwks_uri URI to fetch keys from (for key rotation)
      @param issuer Expected issuer claim
      @param audience Expected audience claim
      @param required_scopes List of required scopes for access
      @return A new OAUTH_AUTHORIZATION_SERVER_PROVIDER module
  *)
  val create :
    ?public_key:string ->
    ?jwks_uri:string ->
    ?issuer:string ->
    ?audience:string list ->
    ?required_scopes:string list ->
    unit ->
    (module OAUTH_AUTHORIZATION_SERVER_PROVIDER)
end 