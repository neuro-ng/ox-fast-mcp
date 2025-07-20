open Core
open Shared.Auth.Provider
open Shared.Auth.Settings

type jwk_data = {
  kty : string;  (** Key type (e.g., "RSA") - required *)
  kid : string option;  (** Key ID (optional but recommended) *)
  use : string option;  (** Usage (e.g., "sig") *)
  alg : string option;  (** Algorithm (e.g., "RS256") *)
  n : string option;  (** Modulus (for RSA keys) *)
  e : string option;  (** Exponent (for RSA keys) *)
  x5c : string list option;  (** X.509 certificate chain *)
  x5t : string option;  (** X.509 certificate thumbprint *)
}
(** JSON Web Key data structure *)

type jwks_data = { keys : jwk_data list }
(** JSON Web Key Set data structure *)

(** RSA Key Pair for testing *)
module RSA_key_pair : sig
  type t = private {
    private_key : string;  (** Private key in PEM format *)
    public_key : string;  (** Public key in PEM format *)
  }

  val generate : unit -> t
  (** Generate a new RSA key pair *)

  val create_token :
    t ->
    ?subject:string ->
    ?issuer:string ->
    ?audience:string list ->
    ?scopes:string list ->
    ?expires_in_seconds:int ->
    ?additional_claims:(string * Yojson.Safe.t) list ->
    ?kid:string ->
    unit ->
    string
  (** Create a JWT token for testing
      @param subject Subject claim (usually user ID)
      @param issuer Issuer claim
      @param audience Audience claim - can be a string or list of strings
      @param scopes List of scopes to include
      @param expires_in_seconds Token expiration time in seconds
      @param additional_claims Any additional claims to include
      @param kid Key ID for JWKS lookup
      @return Signed JWT token string *)
end

(** Bearer Auth Provider module *)
module Bearer_auth_provider : sig
  include OAUTH_AUTHORIZATION_SERVER_PROVIDER

  val create :
    ?public_key:string ->
    ?jwks_uri:string ->
    ?issuer:string ->
    ?audience:string list ->
    ?required_scopes:string list ->
    unit ->
    (module OAUTH_AUTHORIZATION_SERVER_PROVIDER)
  (** Create a new Bearer Auth Provider
      @param public_key RSA public key in PEM format (for static key)
      @param jwks_uri URI to fetch keys from (for key rotation)
      @param issuer Expected issuer claim
      @param audience
        Expected audience claim - can be a string or list of strings
      @param required_scopes List of required scopes for access
      @return A new OAUTH_AUTHORIZATION_SERVER_PROVIDER module *)
end
