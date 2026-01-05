(** Cryptographic Utilities for OAuth2 Flow *)

val generate_state_token : unit -> string
(** Generate a cryptographically secure random state token

    Returns a URL-safe base64-encoded random token (32 bytes = 256 bits)
    suitable for OAuth2 state parameter. *)

val constant_time_compare : string -> string -> bool
(** Constant-time string comparison to prevent timing attacks

    Essential for validating OAuth2 state parameters.

    @param a First string
    @param b Second string
    @return true if strings are equal, false otherwise *)
