open! Core

(** Secret_string provides secure handling of sensitive strings.

    This module is analogous to Python's pydantic.SecretStr, preventing
    accidental logging or printing of sensitive values like tokens, passwords,
    and API keys. *)

type t = { value : string }
(** Opaque type wrapping a sensitive string value *)

let create value = { value }
let get_secret_value t = t.value

(** Custom sexp serialization that redacts the value for safety *)
let sexp_of_t _t = Sexp.Atom "<REDACTED>"

(** Parse from sexp - accepts any string value *)
let t_of_sexp sexp =
  match sexp with
  | Sexp.Atom value -> { value }
  | Sexp.List _ -> failwith "Secret_string.t_of_sexp: expected Atom, got List"

(** Equality comparison - compares underlying values *)
let equal t1 t2 = String.equal t1.value t2.value

(** Ordering comparison - compares underlying values *)
let compare t1 t2 = String.compare t1.value t2.value

(** String representation for debugging - redacts the value *)
let to_string _t = "<REDACTED>"

(** Check if the secret string is empty *)
let is_empty t = String.is_empty t.value

(** Get the length of the underlying secret value *)
let length t = String.length t.value
