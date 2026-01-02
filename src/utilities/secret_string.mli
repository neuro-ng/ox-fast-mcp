open! Core

(** Secret_string - Secure handling of sensitive strings

    This module provides an opaque type for sensitive strings like tokens,
    passwords, and API keys. It prevents accidental exposure through logging or
    printing by automatically redacting values in sexp serialization.

    This is the OCaml equivalent of Python's pydantic.SecretStr.

    Example usage:
    {[
      let token = Secret_string.create "my-secret-token" in
      (* Sexp serialization shows "<REDACTED>" instead of the actual value *)
      printf !"%{sexp: Secret_string.t}\n" token;

      (* prints: <REDACTED> *)

      (* Explicit access when needed *)
      let actual_value = Secret_string.get_secret_value token in
      use_token actual_value
    ]} *)

type t [@@deriving sexp]
(** Opaque type for secret strings *)

val create : string -> t
(** Create a secret string from a plain string value

    @param value The sensitive string to protect
    @return A secret string that redacts its value in sexp output *)

val get_secret_value : t -> string
(** Explicitly retrieve the underlying secret value

    This function name makes it clear that you are accessing sensitive data. Use
    with caution and avoid logging the result.

    @param t The secret string
    @return The underlying plain string value *)

val equal : t -> t -> bool
(** Check equality between two secret strings

    Compares the underlying values securely.

    @param t1 First secret string
    @param t2 Second secret string
    @return true if the underlying values are equal *)

val compare : t -> t -> int
(** Compare two secret strings for ordering

    Compares the underlying values.

    @param t1 First secret string
    @param t2 Second secret string
    @return Comparison result *)

val to_string : t -> string
(** Convert to string - returns redacted placeholder

    @param t The secret string
    @return The string "<REDACTED>" *)

val is_empty : t -> bool
(** Check if the underlying secret value is empty

    @param t The secret string
    @return true if the underlying string is empty *)

val length : t -> int
(** Get the length of the underlying secret value

    This can be useful for validation without exposing the actual value.

    @param t The secret string
    @return Length of the underlying string *)
