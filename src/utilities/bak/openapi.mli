(** OpenAPI module for handling OpenAPI specifications *)

(** {2 Types} *)

type http_method =
  [ `GET | `POST | `PUT | `DELETE | `PATCH | `OPTIONS | `HEAD | `TRACE ]
(** HTTP method type *)

type parameter_location = [ `Path | `Query | `Header | `Cookie ]
(** Parameter location type *)

type json_schema = Yojson.Safe.t
(** JSON schema type *)

type parameter_info = {
  name : string;
  location : parameter_location;
  required : bool;
  schema : json_schema;
  description : string option;
}
(** Parameter information type *)

type request_body_info = {
  required : bool;
  content_schema : (string * json_schema) list; (* media_type * schema *)
  description : string option;
}
(** Request body information type *)

type response_info = {
  description : string option;
  content_schema : (string * json_schema) list; (* media_type * schema *)
}
(** Response information type *)

type http_route = {
  path : string;
  http_method : http_method;
  operation_id : string option;
  summary : string option;
  description : string option;
  tags : string list;
  parameters : parameter_info list;
  request_body : request_body_info option;
  responses : (string * response_info) list; (* status_code * response_info *)
  schema_definitions : (string * json_schema) list;
}
(** HTTP route type *)

(** {2 Helper Modules} *)

module HttpRoute : sig
  val get_method : http_route -> string
  val get_path : http_route -> string
  val get_operation_id : http_route -> string option
  val get_parameters : http_route -> parameter_info list
  val get_request_body : http_route -> request_body_info option
  val get_tags : http_route -> string list
end

module Parameter : sig
  val get_name : parameter_info -> string
  val get_location : parameter_info -> string
  val get_required : parameter_info -> bool
  val get_schema : parameter_info -> json_schema
end

module RequestBody : sig
  val get_required_field : request_body_info -> bool
  val get_content_schema : request_body_info -> (string * json_schema) list
end

(** {2 Conversion Functions} *)

val string_of_http_method : http_method -> string
(** Convert HTTP method to string *)

val http_method_of_string : string -> http_method option
(** Convert string to HTTP method *)

val string_of_parameter_location : parameter_location -> string
(** Convert parameter location to string *)

val parameter_location_of_string : string -> parameter_location option
(** Convert string to parameter location *)

(** {2 JSON Conversion Functions} *)

val parameter_info_of_json : Yojson.Safe.t -> parameter_info option
(** Convert JSON to parameter info *)

val request_body_info_of_json : Yojson.Safe.t -> request_body_info option
(** Convert JSON to request body info *)

val response_info_of_json : Yojson.Safe.t -> response_info option
(** Convert JSON to response info *)

val http_route_of_json : Yojson.Safe.t -> http_route option
(** Convert HTTP route to JSON *)

val json_of_parameter_info : parameter_info -> Yojson.Safe.t
(** Convert parameter info to JSON *)

val json_of_request_body_info : request_body_info -> Yojson.Safe.t
(** Convert request body info to JSON *)

val json_of_response_info : response_info -> Yojson.Safe.t
(** Convert response info to JSON *)

val json_of_http_route : http_route -> Yojson.Safe.t
(** Convert HTTP route to JSON *)

(** {2 Schema Validation Functions} *)

val validate_schema : json_schema -> (string, string) result
(** Validate an OpenAPI schema
    @param schema The schema to validate
    @return
      Ok version if valid (returns the OpenAPI version), Error msg if invalid *)

(** {2 Schema Manipulation Functions} *)

val resolve_ref : json_schema -> string -> json_schema
(** Resolve a JSON Schema reference to its target definition.
    @param schema The root schema containing all definitions
    @param ref_path The reference path (e.g. "#/components/schemas/MyType")
    @return The resolved schema or `Null if resolution fails *)

val replace_ref_with_defs : json_schema -> json_schema -> json_schema
(** Replace OpenAPI $ref with JSON Schema $defs. Also handles external
    references by replacing them with a placeholder.
    @param root_schema The root schema containing all definitions
    @param schema The schema to process
    @return The processed schema with references replaced *)

val combine_schemas : http_route -> json_schema
(** Combine all schemas in a route into a single schema. This includes
    parameters, request body, responses, and schema definitions. References are
    resolved and replaced with $defs.
    @param route The HTTP route containing all schemas
    @return A combined schema with all references resolved *)

val parse_openapi_to_http_routes : Yojson.Safe.t -> http_route list
(** Parse OpenAPI specification into HTTP routes. Supports OpenAPI 3.0.x and
    3.1.x versions.
    @param schema The OpenAPI schema to parse
    @return A list of HTTP routes *)

(** {2 Schema Display and Formatting Functions} *)

val clean_schema_for_display : json_schema -> json_schema
(** Clean up a schema for display purposes *)

val generate_example_from_schema : json_schema -> json_schema
(** Generate an example from a schema *)

val format_json_for_description : Yojson.Safe.t -> string
(** Format JSON for a description *)

val format_description_with_responses :
  ?parameters:parameter_info list ->
  ?request_body:request_body_info ->
  responses:(string * response_info) list ->
  string ->
  string
(** Format a description with responses *)

(** {2 JSON Manipulation Functions} *)

val deep_copy_json : Yojson.Safe.t -> Yojson.Safe.t
(** Create a deep copy of a JSON value *)
