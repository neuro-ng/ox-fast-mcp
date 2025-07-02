open Utilities.Types

(** Tool handler signature *)
type tool_handler = execution_context -> json -> content_type list Lwt.t

(** Function tool definition *)
type function_tool = {
  name : string;
  description : string;
  parameters : json;
  handler : tool_handler;
  enabled : bool;
  tags : string list;
  annotations : (string * json) list option;
}

(** Transformed tool type *)
type transformed = {
  parent_tool : function_tool;
  fn : tool_handler;
  forwarding_fn : tool_handler;
  parameters : json;
  transform_args : (string, Tool_transform.Arg_transform.t) Map.t;
}

(** Create a new function tool *)
val create_tool :
  name:string ->
  description:string ->
  ?parameters:json ->
  ?enabled:bool ->
  ?tags:string list ->
  ?annotations:(string * json) list option ->
  tool_handler ->
  function_tool

(** Create a transformed tool *)
val create_transformed :
  ?name:string ->
  ?description:string ->
  ?tags:string list ->
  ?annotations:(string * json) list option ->
  ?serializer:(json -> string) ->
  ?enabled:bool ->
  parent_tool:function_tool ->
  fn:tool_handler ->
  forwarding_fn:tool_handler ->
  parameters:json ->
  transform_args:(string, Tool_transform.Arg_transform.t) Map.t ->
  unit ->
  transformed

(** Run transformed tool's forwarding function *)
val run_transformed_forwarding : transformed -> json -> content_type list Lwt.t

(** Get tool parameters *)
val parameters : function_tool -> json

(** Run a tool *)
val run : function_tool -> json -> content_type list Lwt.t 