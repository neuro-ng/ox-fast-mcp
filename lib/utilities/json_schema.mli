(** JSON Schema manipulation utilities *)

open Yojson.Safe

(** Remove a parameter from a JSON schema
    @param schema The schema to modify
    @param param The parameter name to remove
    @return A new schema with the parameter removed *)
val prune_param : t -> string -> t

(** Remove unused definitions from a JSON schema
    @param schema The schema to modify
    @return A new schema with unused definitions removed *)
val prune_unused_defs : t -> t

(** Walk through a schema and optionally remove titles and additionalProperties
    @param prune_titles Whether to remove title fields
    @param prune_additional_properties Whether to remove additionalProperties: false
    @param schema The schema to modify
    @return A new schema with requested fields removed *)
val walk_and_prune : ?prune_titles:bool -> ?prune_additional_properties:bool -> t -> t

(** Remove additionalProperties: false from a schema
    @param schema The schema to modify
    @return A new schema with additionalProperties: false removed *)
val prune_additional_properties : t -> t

(** Compress a JSON schema by removing specified elements
    @param prune_params List of parameter names to remove
    @param prune_defs Whether to remove unused definitions
    @param prune_additional_properties Whether to remove additionalProperties: false
    @param prune_titles Whether to remove title fields
    @param schema The schema to modify
    @return A new compressed schema *)
val compress_schema :
  ?prune_params:string list ->
  ?prune_defs:bool ->
  ?prune_additional_properties:bool ->
  ?prune_titles:bool ->
  t -> t 