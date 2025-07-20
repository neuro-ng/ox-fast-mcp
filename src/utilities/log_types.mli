open! Core

module type Handler = sig
  type t
end

module Level : sig
  type t = Debug | Info | Warning | Error | Critical
  [@@deriving sexp, compare, yojson]

  val of_string : string -> t
  val to_string : t -> string
  val to_level : t -> [> `Debug | `Info | `Warning | `Error | `Critical ]

  include Comparable.S with type t := t

  val compare_level : t -> t -> int
  val level_ge : t -> t -> bool
  val level_le : t -> t -> bool
  val level_gt : t -> t -> bool
  val level_lt : t -> t -> bool
  val level_eq : t -> t -> bool
end
