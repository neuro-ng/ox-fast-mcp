(** Base component types for OxFastMCP

    Implements the polymorphic component pattern for tools, prompts, resources.
    See: PYTHON_TO_OCAML_TYPE_MAP.md Section 2 (lines 85-164) See:
    COMPLIANCE_ACTION_PLAN.md Task 2.1 *)

open! Core

(** Helper module for String.Set.t JSON conversion *)
module String_set : sig
  type t = String.Set.t [@@deriving compare, sexp, yojson]
end

(** Custom meta type for component metadata *)
module Meta : sig
  type t = (string * Yojson.Safe.t) list

  val yojson_of_t : t -> Yojson.Safe.t
  val t_of_yojson : Yojson.Safe.t -> t
  val sexp_of_t : t -> Sexp.t
  val t_of_sexp : Sexp.t -> t
end

(** Base component record type - polymorphic over component-specific data *)
type 'a component = {
  name : string;
  title : string option; [@default None] [@yojson_drop_if Option.is_none]
  description : string option; [@default None] [@yojson_drop_if Option.is_none]
  tags : String_set.t;
  meta : Meta.t option; [@default None] [@yojson_drop_if Option.is_none]
  enabled : bool;
  key : string option; [@default None] [@yojson_drop_if Option.is_none]
  data : 'a;  (** Polymorphic data field for specific component types *)
}
[@@deriving sexp, yojson]
(** Base type for all OxFastMCP components (tools, prompts, resources).

    The 'a type parameter holds component-specific data:
    - For tools: tool_data
    - For resources: resource_data
    - For prompts: prompt_data

    Note: compare function is provided separately as compare_component *)

val compare_component : ('a -> 'a -> int) -> 'a component -> 'a component -> int
(** Custom compare function for components that takes a comparison function for
    the data field *)

type fastmcp_meta = { tags : string list } [@@deriving sexp, yojson]
(** Metadata type for FastMCP components *)

(** Mirrored component flag *)
type mirrored_flag = Local | Mirrored [@@deriving sexp, compare, yojson]

type 'a mirrored_component = {
  component : 'a component;
  mirrored : mirrored_flag;
}
[@@deriving sexp, yojson]
(** Component with mirrored status *)

val create :
  ?key:string ->
  ?title:string ->
  ?description:string ->
  ?tags:String.Set.t ->
  ?meta:Meta.t ->
  ?enabled:bool ->
  name:string ->
  data:'a ->
  unit ->
  'a component
(** Create a new component with component-specific data *)

val key : 'a component -> string
(** Get the key of the component. This is used for internal bookkeeping and may
    reflect e.g. prefixes or other identifiers. You should not depend on keys
    having a certain value, as the same tool loaded from different hierarchies
    of servers may have different keys. *)

val with_key : 'a component -> string -> 'a component
(** Create a new component with a different key *)

val enable : 'a component -> 'a component
(** Enable the component *)

val disable : 'a component -> 'a component
(** Disable the component *)

val get_meta :
  include_fastmcp_meta:bool option ->
  'a component ->
  (string * Yojson.Safe.t) list option
(** Get metadata for the component, optionally including FastMCP-specific
    metadata *)
