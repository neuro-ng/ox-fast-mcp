open! Core
open! Async

(** Client Credentials OAuth2 Authentication

    This module implements RFC 7523 JWT Bearer authentication for OAuth2 client
    credentials flow. It provides JWT assertion generation and OAuth provider
    integration.

    Note: This is a partial implementation. Full OAuth client provider
    functionality requires additional infrastructure. See
    client_credentials.todo for details on missing components. *)

(** JWT Parameters for RFC 7523 JWT Bearer authentication *)
module Jwt_parameters : sig
  type t [@@deriving sexp]

  val create :
    ?assertion:string ->
    ?issuer:string ->
    ?subject:string ->
    ?audience:string ->
    ?claims:(string * Yojson.Safe.t) list ->
    ?jwt_signing_algorithm:string ->
    ?jwt_signing_key:string ->
    ?jwt_lifetime_seconds:int ->
    unit ->
    t
  (** Create JWT parameters with optional configuration

      @param assertion Predefined JWT assertion string (if available)
      @param issuer Issuer (iss) claim for generated JWTs
      @param subject Subject (sub) claim for generated JWTs
      @param audience Audience (aud) claim for generated JWTs
      @param claims Additional custom claims as key-value pairs
      @param jwt_signing_algorithm Signing algorithm (default: RS256)
      @param jwt_signing_key Private key for signing (securely stored)
      @param jwt_lifetime_seconds JWT validity period (default: 300)
      @return JWT parameters instance *)

  val to_assertion : t -> with_audience_fallback:string option -> string
  (** Generate JWT assertion from parameters

      If a predefined assertion is configured, it will be returned. Otherwise, a
      new JWT will be generated with the configured claims.

      Note: Current implementation generates claim structure but JWT
      encoding/signing requires integrating a JWT library (jose or ocaml-jwt).

      @param t JWT parameters
      @param with_audience_fallback Fallback audience if not configured
      @return JWT assertion string
      @raise Failure
        if required parameters (issuer, subject, audience, signing_key) are
        missing *)
end

(** RFC 7523 OAuth Client Provider (STUB)

    This is a stub implementation showing the intended API structure. Full
    implementation requires OAuth client infrastructure including:
    - OAuthClientProvider base class
    - OAuth metadata types (OAuthClientMetadata, OAuthMetadata)
    - Token storage interface
    - OAuth context management
    - HTTP client integration

    See client_credentials.todo for complete list of dependencies. *)
module Rfc7523_oauth_client_provider : sig
  type t [@@deriving sexp]

  val create :
    server_url:string -> ?jwt_parameters:Jwt_parameters.t -> unit -> t
  (** Create RFC 7523 OAuth client provider

      @param server_url OAuth server base URL
      @param jwt_parameters Optional JWT parameters for authentication
      @return Provider instance (stub - not fully functional) *)
end
