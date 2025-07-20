open! Core

module String_set : sig
  type t = String.Set.t [@@deriving compare, sexp, yojson]
end

type t = {
  name : string;
  description : string option;
  tags : String_set.t;
  enabled : bool;
  key : string option; [@default None] [@yojson_drop_if Option.is_none]
}
[@@deriving compare, sexp, yojson]
(** Base class for FastMCP tools, prompts, resources, and resource templates. *)

val create :
  ?key:string ->
  ?description:string ->
  ?tags:String.Set.t ->
  ?enabled:bool ->
  name:string ->
  unit ->
  t
(** Create a new component *)

val key : t -> string
(** Get the key of the component. This is used for internal bookkeeping and may
    reflect e.g. prefixes or other identifiers. You should not depend on keys
    having a certain value, as the same tool loaded from different hierarchies
    of servers may have different keys. *)

val with_key : t -> string -> t
(** Create a new component with a different key *)

val enable : t -> t
(** Enable the component *)

val disable : t -> t
(** Disable the component *)
