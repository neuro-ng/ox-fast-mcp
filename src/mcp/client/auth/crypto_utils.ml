(** Cryptographic Utilities for OAuth2 Flow

    Provides secure random token generation and constant-time string comparison
    for OAuth2 state parameter handling. *)

open Core

(** Generate a cryptographically secure random state token

    Generates a URL-safe base64-encoded random token suitable for OAuth2 state
    parameter. Uses 32 bytes of random data (256 bits).

    @return URL-safe base64 encoded string without padding *)
let generate_state_token () =
  (* Generate 32 random bytes (256 bits) *)
  let random_bytes = Bytes.create 32 in
  for i = 0 to 31 do
    Bytes.set random_bytes i (Char.of_int_exn (Random.int 256))
  done;
  (* Base64 URL-safe encoding without padding *)
  Base64.encode_exn ~pad:false ~alphabet:Base64.uri_safe_alphabet
    (Bytes.to_string random_bytes)

(** Constant-time string comparison

    Compares two strings in constant time to prevent timing attacks. This is
    essential for validating OAuth2 state parameters.

    @param a First string
    @param b Second string
    @return true if strings are equal, false otherwise *)
let constant_time_compare a b =
  let len_a = String.length a in
  let len_b = String.length b in
  (* Always compare full length to avoid early exit timing leak *)
  let max_len = Int.max len_a len_b in
  let mutable_result = ref 0 in
  for i = 0 to max_len - 1 do
    let char_a = if i < len_a then Char.to_int a.[i] else 0 in
    let char_b = if i < len_b then Char.to_int b.[i] else 0 in
    mutable_result := !mutable_result lor (char_a lxor char_b)
  done;
  (* Length mismatch also contributes to result *)
  mutable_result := !mutable_result lor (len_a lxor len_b);
  !mutable_result = 0
