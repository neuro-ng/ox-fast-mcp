open Core
open Async

(** URI template matching for dynamic resources
    
    This module provides URI template matching following RFC 6570, supporting:
    - Path parameters: {var}, {var*} (wildcard)
    - Query parameters: {?var1,var2}
*)

val extract_query_params : string -> String.Set.t
(** Extract query parameter names from RFC 6570 [{?param1,param2}] syntax *)

val match_uri_template : string -> string -> string String.Map.t option
(** Match URI against template and extract both path and query parameters.
    Returns None if URI doesn't match template, Some param_map otherwise *)

type template_config = {
  uri_template : string;
  name : string;
  description : string option;
  mime_type : string;
}
[@@deriving sexp_of]
(** Resource template configuration *)

(** Resource template for dynamic resource creation *)
module ResourceTemplate : sig
  type 'a t

  val create :
    uri_template:string ->
    name:string ->
    ?description:string ->
    ?mime_type:string ->
    read_fn:(string String.Map.t -> 'a Deferred.t) ->
    ?list_fn:(string String.Map.t -> Mcp.Types.resource list Deferred.t) ->
    unit ->
    'a t

  val matches : 'a t -> string -> string String.Map.t option
  val read : 'a t -> string -> 'a Deferred.Or_error.t
  val list : 'a t -> string -> Mcp.Types.resource list Deferred.Or_error.t
  val to_mcp : 'a t -> Mcp.Types.resource_template
end

val text_resource_template :
  uri_template:string ->
  name:string ->
  ?description:string ->
  read_fn:(string String.Map.t -> string Deferred.t) ->
  ?list_fn:(string String.Map.t -> Mcp.Types.resource list Deferred.t) ->
  unit ->
  Mcp.Types.text_resource_contents ResourceTemplate.t
(** Create a text resource template *)

val binary_resource_template :
  uri_template:string ->
  name:string ->
  ?description:string ->
  ?mime_type:string ->
  read_fn:(string String.Map.t -> string Deferred.t) ->
  ?list_fn:(string String.Map.t -> Mcp.Types.resource list Deferred.t) ->
  unit ->
  Mcp.Types.blob_resource_contents ResourceTemplate.t
(** Create a binary resource template *)
