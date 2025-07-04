open! Core
open! Async

(** Property type for JSON schema *)
type property = {
  name : string;
  value : Yojson.Safe.t;
} [@@deriving yojson]

(** JSON schema type *)
type json_schema = {
  properties : property list option [@yojson.option];
  required : string list option [@yojson.option];
  defs : property list option [@yojson.option] [@key "$defs"];
  additional_properties : bool option [@yojson.option];
  title : string option [@yojson.option];
} [@@deriving yojson]

(** Prune a parameter from a schema *)
val prune_param : Yojson.Safe.t -> string -> Yojson.Safe.t

(** Walk schema and track def references *)
val find_referenced_defs : Yojson.Safe.t -> string list

(** Prune unused definitions from schema *)
val prune_unused_defs : Yojson.Safe.t -> Yojson.Safe.t

(** Walk schema and optionally prune titles and additionalProperties *)
val walk_and_prune :
  ?prune_titles:bool ->
  ?prune_additional_properties:bool ->
  Yojson.Safe.t ->
  Yojson.Safe.t

(** Compress schema by pruning specified elements *)
val compress_schema :
  ?prune_params:string list ->
  ?prune_defs:bool ->
  ?prune_additional_properties:bool ->
  ?prune_titles:bool ->
  Yojson.Safe.t ->
  Yojson.Safe.t 