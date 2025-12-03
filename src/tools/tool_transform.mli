open Core
open Fmcp_types
open Tool_types
open Async

val forward : execution_context -> json -> content_type list Deferred.t
(** Forward arguments to parent tool with transformation *)

val forward_raw : execution_context -> json -> content_type list Deferred.t
(** Forward raw arguments to parent tool without transformation *)

val create_from_tool :
  ?name:string ->
  ?description:string ->
  ?tags:string list ->
  ?transform_fn:tool_handler ->
  ?transform_args:Arg_transform.t String.Map.t ->
  ?_annotations:'a ->
  ?_serializer:'b ->
  ?enabled:bool ->
  Tool_types.tool_function ->
  Tool_types.t
(** Create a transformed tool from a parent tool *)
