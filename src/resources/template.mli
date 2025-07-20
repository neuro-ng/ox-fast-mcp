(** Resource template functionality *)

open Core

exception Parameter_validation_error of string
(** Parameter validation error *)

exception Template_function_signature_error of string
(** Function signature error *)

exception Lambda_function_error of string
(** Lambda function error *)

(** Function parameter type *)
type parameter_type =
  | Required of string (* Required named parameter *)
  | Optional of string (* Optional named parameter *)
  | Context (* Context parameter *)
  | VarArgs (* Variable arguments *)
  | KwArgs (* Keyword arguments *)
[@@deriving sexp, compare]

type template_function_signature = {
  parameters : parameter_type list;
  docstring : string option;
  is_async : bool;
}
[@@deriving sexp]
(** Function signature *)

(** Function type *)
type ('ctx, 'params, 'result) function_type =
  | Normal of
      (?ctx:'ctx -> 'params -> 'result Lwt.t) * template_function_signature
  | Static of
      (?ctx:'ctx -> 'params -> 'result Lwt.t) * template_function_signature
  | Method of
      (?ctx:'ctx -> 'params -> 'result Lwt.t) * template_function_signature
  | Class_method of
      (?ctx:'ctx -> 'params -> 'result Lwt.t) * template_function_signature

type function_metadata = {
  name : string;
  description : string option;
  is_static : bool;
  is_method : bool;
  is_class_method : bool;
  signature : template_function_signature;
}
[@@deriving sexp]
(** Function metadata *)

type t = {
  uri_template : string;
  name : string;
  mime_type : string;
  description : string option;
  tags : string list;
  enabled : bool;
  parameters : Yojson.Safe.t; (* JSON schema for parameters *)
}
[@@deriving sexp, yojson]
(** Resource template type *)

type ('ctx, 'a) function_template = {
  base : t;
  fn : ?ctx:'ctx -> (string * string) list -> 'a Lwt.t;
  required_params : String.Set.t;
  optional_params : String.Set.t;
  metadata : function_metadata;
}
[@@deriving sexp]
(** Function template type *)

val get_function_metadata :
  ('ctx, 'params, 'result) function_type -> function_metadata
(** Get function metadata *)

val extract_parameters :
  template_function_signature -> String.Set.t * String.Set.t
(** Extract required and optional parameters from function signature *)

val extract_uri_parameters : string -> String.Set.t
(** Extract parameter names from URI template *)

val validate_function_parameters :
  uri_template:string ->
  required_params:String.Set.t ->
  optional_params:String.Set.t ->
  signature:template_function_signature ->
  unit
(** Validate function parameters against URI template *)

val match_uri_template : string -> string -> (string * string) list option
(** Match URI against template and extract parameters *)

val to_mcp_template :
  ?overrides:(string * Yojson.Safe.t) list -> t -> Mcp.Types.Resource_template.t
(** Convert template to MCP template *)

val of_mcp_template : Mcp.Types.Resource_template.t -> t
(** Create template from MCP template *)

val key : t -> string
(** Get template key *)

val enable : t -> t Lwt.t
(** Enable template *)

val disable : t -> t Lwt.t
(** Disable template *)

val create_function_template :
  ?name:string ->
  ?description:string ->
  ?mime_type:string ->
  ?tags:string list ->
  ?enabled:bool ->
  uri_template:string ->
  ('ctx, (string * string) list, 'a) function_type ->
  ('ctx, 'a) function_template
(** Create function template *)

val read_function_template :
  ?ctx:'ctx ->
  ('ctx, 'a) function_template ->
  (string * string) list ->
  'a Lwt.t
(** Read function template *)

val create_resource :
  ?ctx:'ctx ->
  ('ctx, 'a) function_template ->
  string ->
  (string * string) list ->
  Resource.t Lwt.t
(** Create resource from template *)
