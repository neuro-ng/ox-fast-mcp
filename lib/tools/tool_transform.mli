open Core
open Async

(** Context variable to store current transformed tool *)
val current_tool : Tool.transformed option ref

(** Module for configuring argument transformations *)
module Arg_transform : sig
  type t = {
    name : string option;
    description : string option;
    default : Yojson.Safe.t option;
    default_factory : (unit -> Yojson.Safe.t) option;
    type_schema : Yojson.Safe.t option;
    hide : bool;
    required : bool option;
    examples : Yojson.Safe.t option;
  }
  [@@deriving sexp]

  (** Create a new argument transformation with validation *)
  val create :
    ?name:string ->
    ?description:string ->
    ?default:Yojson.Safe.t ->
    ?default_factory:(unit -> Yojson.Safe.t) ->
    ?type_schema:Yojson.Safe.t ->
    ?hide:bool ->
    ?required:bool ->
    ?examples:Yojson.Safe.t ->
    unit ->
    t
end

(** Forward arguments to parent tool with transformation *)
val forward : Yojson.Safe.t -> Content_block.t list Deferred.t

(** Forward raw arguments to parent tool without transformation *)
val forward_raw : Yojson.Safe.t -> Content_block.t list Deferred.t

(** Check if a function accepts kwargs *)
val function_has_kwargs : Template.t -> bool

(** Apply transformation to a single parameter *)
val apply_single_transform :
  old_name:string ->
  old_schema:Yojson.Safe.t Map.M(String).t ->
  transform:Arg_transform.t ->
  is_required:bool ->
  (string * Yojson.Safe.t Map.M(String).t * bool) option

(** Create schema and forwarding function for transformed tool *)
val create_forwarding_transform :
  parent_tool:Tool.function_tool ->
  transform_args:(string, Arg_transform.t) Map.t ->
  Schema.t * (Yojson.Safe.t -> Content_block.t list Deferred.t)

(** Merge two JSON schemas with precedence rules *)
val merge_schema_with_precedence :
  base_schema:Schema.t ->
  override_schema:Schema.t ->
  fn:Template.t ->
  Schema.t

(** Create a transformed tool from a parent tool *)
val from_tool :
  ?name:string ->
  ?description:string ->
  ?tags:string list ->
  ?transform_fn:Template.t ->
  ?transform_args:(string, Arg_transform.t) Map.t ->
  ?annotations:(string * Yojson.Safe.t) list ->
  ?serializer:(Yojson.Safe.t -> string) ->
  ?enabled:bool ->
  Tool.function_tool ->
  Tool.transformed 