(** Resource types for OxFastMCP

    Implements resource type system per PYTHON_TO_OCAML_TYPE_MAP.md Section 4
    Follows the same pattern as tool_types.mli and prompt_types.mli See: Task
    4.1 - Resource Integration with Component Pattern *)

open! Core
open! Async

(** {1 Resource Content Types} *)

(** Resource content type *)
type content = Text of string | Binary of bytes
[@@deriving sexp, compare, yojson]

(** {1 Resource Handler} *)

type reader =
  unit -> (content, Ox_fast_mcp.Exceptions.error_data) Deferred.Result.t
(** Resource reader function - returns Result.t for error handling *)

(** {1 Resource Data} *)

(** Annotations module for handling JSON annotations *)
module Annotations : sig
  type t = (string * Yojson.Safe.t) list

  val yojson_of_t : t -> Yojson.Safe.t
  val t_of_yojson : Yojson.Safe.t -> t
end

type resource_data = {
  uri : string;
  mime_type : string;
  annotations : Annotations.t option;
      [@default None] [@yojson_drop_if Option.is_none]
}
[@@deriving yojson]
(** Resource-specific metadata *)

(** {1 Resource Kinds} *)

(** Resource variants *)
type resource_kind =
  | Function_resource of { fn : reader }
  | Text_resource of { content : string }
  | Binary_resource of { content : bytes }
  | File_resource of { path : string }
(* Note: No sexp derivation due to function field *)

(** {1 Unified Resource Type} *)

type resource_component_data = { data : resource_data; kind : resource_kind }
(** Component-specific data for resources *)
(* Note: No sexp derivation due to function field *)

type t = resource_component_data Components.component
(** Main resource type - uses polymorphic component pattern! *)
(* Note: No sexp derivation due to function field *)

(** {1 Resource Operations} *)

val get_uri : t -> string
(** Get resource URI *)

val get_mime_type : t -> string
(** Get resource MIME type *)

val get_name : t -> string
(** Get resource name *)

val get_description : t -> string option
(** Get resource description *)

val is_enabled : t -> bool
(** Check if resource is enabled *)

val get_tags : t -> string list
(** Get resource tags *)

val get_annotations : t -> Annotations.t option
(** Get resource annotations *)

(** {1 Resource Modification} *)

val enable : t -> t
(** Enable a resource *)

val disable : t -> t
(** Disable a resource *)

val key : t -> string
(** Get or generate resource key *)

val with_key : t -> string -> t
(** Set resource key *)

(** {1 Resource Reading} *)

val read : t -> (content, Ox_fast_mcp.Exceptions.error_data) Deferred.Result.t
(** Read resource content *)

(** {1 Resource Creation} *)

val create_function_resource :
  uri:string ->
  name:string ->
  ?description:string ->
  ?mime_type:string ->
  ?tags:string list ->
  ?key:string ->
  ?annotations:Annotations.t ->
  ?enabled:bool ->
  reader ->
  t
(** Create a function resource *)

val create_text_resource :
  uri:string ->
  name:string ->
  content:string ->
  ?description:string ->
  ?mime_type:string ->
  ?tags:string list ->
  ?key:string ->
  ?annotations:Annotations.t ->
  ?enabled:bool ->
  unit ->
  t
(** Create a text resource *)

val create_binary_resource :
  uri:string ->
  name:string ->
  content:bytes ->
  ?description:string ->
  mime_type:string ->
  ?tags:string list ->
  ?key:string ->
  ?annotations:Annotations.t ->
  ?enabled:bool ->
  unit ->
  t
(** Create a binary resource *)

val create_file_resource :
  uri:string ->
  name:string ->
  path:string ->
  ?description:string ->
  ?mime_type:string ->
  ?tags:string list ->
  ?key:string ->
  ?annotations:Annotations.t ->
  ?enabled:bool ->
  unit ->
  t
(** Create a file resource *)

(** {1 MCP Integration} *)

val to_mcp_resource : ?_include_fastmcp_meta:bool -> t -> Mcp.Types.resource
(** Convert to MCP resource *)

(** {1 Component Integration} *)

val to_component : t -> t
(** Resources are components *)

val from_component : resource_component_data Components.component -> t
(** Create resource from component *)

(** {1 Helper Functions} *)

val validate_mime_type : string -> bool
(** Validate MIME type format *)

val text : string -> content
(** Create text content helper *)

val binary : bytes -> content
(** Create binary content helper *)
