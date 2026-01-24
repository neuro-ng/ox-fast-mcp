open! Core
open! Async

(** Bearer token authentication for HTTP requests

    This module provides Bearer token authentication analogous to Python's
    httpx.Auth interface, adapted for OCaml's Cohttp library.

    Tokens are stored securely using {!Secret_string}, which prevents accidental
    exposure through logging or sexp serialization. *)

module BearerAuth : sig
  type t [@@deriving sexp, compare]

  val create : string -> t
  (** Create a new BearerAuth instance with the given token

      The token is automatically wrapped in a {!Secret_string} for secure
      handling. The token will be redacted in sexp output.

      @param token The bearer token to use for authentication
      @return A new BearerAuth instance *)

  val apply_auth : t -> Cohttp.Header.t -> Cohttp.Header.t
  (** Apply bearer authentication to HTTP headers

      Adds an "Authorization: Bearer <token>" header to the provided headers.
      This function also logs the authentication application at debug level.

      @param t The BearerAuth instance
      @param headers The existing Cohttp headers
      @return Updated headers with Authorization header *)

  val get_token : t -> string
  (** Get the token value

      Explicitly retrieves the underlying token value. Use with caution as this
      exposes the sensitive token value.

      @param t The BearerAuth instance
      @return The bearer token string *)
end
