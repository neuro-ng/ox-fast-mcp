open Core
open Utilities.Types
open Tool_types

(** Forward arguments to parent tool with transformation *)
val forward : execution_context -> json -> content_type list Lwt.t

(** Forward raw arguments to parent tool without transformation *)
val forward_raw : execution_context -> json -> content_type list Lwt.t

(** Create a transformed tool from a parent tool *)
val create_from_tool :
  ?name:string ->
  ?description:string ->
  ?tags:string list ->
  ?transform_fn:tool_handler ->
  ?transform_args:Arg_transform.t String.Map.t ->
  ?_annotations:'a ->
  ?_serializer:'b ->
  ?enabled:bool ->
  function_tool ->
  transformed 