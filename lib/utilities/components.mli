open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

(** Component validation errors *)
module Error : sig
  type t =
    | Invalid_name of string
    | Invalid_tags of string list
    | Invalid_json of string
    | Invalid_version of string
    | Invalid_extra of string
    | Invalid_type of string
    | Validation_error of string
  [@@deriving sexp, compare, equal, hash]

  exception Component_error of t [@@deriving sexp]

  val to_string : t -> string
  val to_error : t -> Error.t
end

(** Base type for FastMCP components *)
type 'a t = {
  name : string; [@key "name"] (** The name of the component *)
  description : string option; [@key "description"] (** The description of the component *)
  tags : string list; [@key "tags"] (** Tags for the component *)
  enabled : bool; [@key "enabled"] [@default true] (** Whether the component is enabled *)
  key : string option; [@key "_key"] [@default None] (** Internal key for bookkeeping *)
  extra : 'a; [@key "extra"] (** Extra data specific to each component type *)
  version : int option; [@key "version"] [@default None] (** Component version for migrations *)
} [@@deriving fields, sexp, compare]

(** Convert a list to a set, defaulting to an empty list if None *)
val convert_set_default_none : 'a list option -> 'a list

(** Convert Result to Or_error *)
val to_or_error : ('a, Error.t) Result.t -> 'a Or_error.t

(** Create a new FastMCP component with validation *)
val create :
  ?description:string option ->
  ?tags:string list ->
  ?enabled:bool ->
  ?key:string option ->
  ?version:int option ->
  name:string ->
  extra:'a ->
  unit ->
  ('a t, Error.t) Result.t

(** Get the component's key, falling back to name if not set *)
val get_key : 'a t -> string

(** Create a copy of the component with a new key *)
val with_key : 'a t -> string -> 'a t

(** Enable the component *)
val enable : 'a t -> 'a t

(** Disable the component *)
val disable : 'a t -> 'a t

(** Compare two components for equality *)
val equal : ?compare_extra:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

(** Convert component to string representation *)
val to_string : 'a t -> string

(** Create a copy of the component with updated fields *)
val copy :
  ?name:string ->
  ?description:string option ->
  ?tags:string list ->
  ?enabled:bool ->
  ?key:string option ->
  ?extra:'a ->
  ?version:int option ->
  'a t ->
  ('a t, Error.t) Result.t

(** JSON serialization with validation *)
val to_yojson : ('a -> Yojson.Safe.t) -> 'a t -> (Yojson.Safe.t, Error.t) Result.t

(** JSON deserialization with validation *)
val of_yojson : (Yojson.Safe.t -> ('a, string) Result.t) -> Yojson.Safe.t -> ('a t, string) Result.t

(** Version-related functions *)
val get_version : 'a t -> int option
val set_version : 'a t -> int -> ('a t, Error.t) Result.t
val clear_version : 'a t -> ('a t, Error.t) Result.t

(** Comparison functions *)
val compare_by_name : 'a t -> 'a t -> int
val compare_by_version : 'a t -> 'a t -> int
val compare_by_key : 'a t -> 'a t -> int

(** Hashing functions *)
val hash : ?hash_extra:('a -> int) -> 'a t -> int

(** Type conversion functions *)
val to_list : 'a t list -> 'a t list
val of_list : 'a t list -> 'a t list

(** Component validation rules *)
module Validation : sig
  val validate_name : string -> (unit, Error.t) Result.t
  val validate_tags : string list -> (unit, Error.t) Result.t
  val validate_version : int option -> (unit, Error.t) Result.t
  val validate_extra : 'a option -> (unit, Error.t) Result.t
  val validate_all : 'a t -> (unit, Error.t) Result.t
end 