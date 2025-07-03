open Core

module Field_error : sig
  type t =
    | Invalid_type of { field : string; expected : string; got : string }
    | Missing_required of string
    | Invalid_value of { field : string; message : string }
    | Nested_error of { field : string; error : t }
  [@@deriving sexp]

  val to_string : t -> string
end

module Field_metadata : sig
  type validator = string -> (unit, string) Result.t

  type t = {
    name : string;
    description : string option;
    required : bool;
    validators : validator list;
  }

  val create :
    ?description:string ->
    ?required:bool ->
    ?validators:validator list ->
    string ->
    t

  val validate : t -> string -> (unit, string) Result.t
end

module Field_value : sig
  type t =
    | String of string
    | Int of int
    | Float of float
    | Bool of bool
    | List of t list
    | Dict of (string * t) list
    | Null
  [@@deriving sexp, yojson_of, yojson]

  val type_name : t -> string
  val to_string : t -> string

  val of_string : string -> t
  val of_int : int -> t
  val of_float : float -> t
  val of_bool : bool -> t
  val of_list : t list -> t
  val of_dict : (string * t) list -> t

  val get_string : t -> (string, string) Result.t
  val get_int : t -> (int, string) Result.t
  val get_float : t -> (float, string) Result.t
  val get_bool : t -> (bool, string) Result.t
  val get_list : t -> (t list, string) Result.t
  val get_dict : t -> ((string * t) list, string) Result.t
end

module Nested_settings : sig
  type t [@@deriving sexp]

  val create : (string * Field_metadata.t) list -> t
  val add_field : t -> string -> Field_metadata.t -> t
  val set_value : t -> string -> Field_value.t -> t
  val get_value : t -> string -> Field_value.t option
  val get_field_metadata : t -> string -> Field_metadata.t option
  val validate : t -> (unit, Field_error.t) Result.t
  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) Result.t
end

(** Field validation functions *)

val validate_min_length : int -> Field_metadata.validator
val validate_max_length : int -> Field_metadata.validator
val validate_pattern : string -> Field_metadata.validator
val validate_min : float -> Field_metadata.validator
val validate_max : float -> Field_metadata.validator
val validate_enum : string list -> Field_metadata.validator 