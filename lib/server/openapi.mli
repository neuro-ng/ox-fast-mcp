(** OpenAPI server implementation for FastMCP *)

open Core

(** {2 Types} *)

module MCPType : sig
  type t =
    | Tool
    | Resource
    | ResourceTemplate
    | Exclude
  [@@deriving show, eq]

  val to_string : t -> string
  val of_string : string -> t option
end

(** Deprecated RouteType module for backward compatibility.
    Use MCPType instead. *)
module RouteType : sig
  type t =
    | Tool
    | Resource
    | ResourceTemplate
    | Ignore  (** Deprecated, use MCPType.Exclude instead *)
  [@@deriving show, eq]

  val to_string : t -> string
  val of_string : string -> t option
  val to_mcp_type : t -> MCPType.t
end

type route_map = {
  methods: string list;  (** "*" for all methods *)
  pattern: string;  (** regex pattern *)
  mcp_type: MCPType.t;
  tags: string list;
  mcp_tags: string list;
} [@@deriving fields]

type component_counts = {
  tools: int String.Map.t;
  resources: int String.Map.t;
  templates: int String.Map.t;
} [@@deriving sexp]

(** {2 Default Values} *)

val default_route_mappings : route_map list
(** Default route mappings that convert all routes to tools *)

(** {2 Helper Functions} *)

val slugify : string -> string
(** Convert text to a URL-friendly slug format *)

(** {2 OpenAPI Components} *)

module OpenAPITool : sig
  type t = private {
    client: Cohttp_lwt_unix.Client.t;
    route: Utilities.Openapi.http_route;
    name: string;
    description: string;
    parameters: Yojson.Safe.t;
    tags: string list;
    timeout: float option;
  }

  val create :
    client:Cohttp_lwt_unix.Client.t ->
    route:Utilities.Openapi.http_route ->
    name:string ->
    description:string ->
    parameters:Yojson.Safe.t ->
    ?tags:string list ->
    ?timeout:float ->
    unit ->
    t

  val run : t -> Yojson.Safe.t -> Mcp.Types.ContentBlock.t list Lwt.t
end

module OpenAPIResource : sig
  type t = private {
    client: Cohttp_lwt_unix.Client.t;
    route: Utilities.Openapi.http_route;
    uri: Uri.t;
    name: string;
    description: string;
    mime_type: string;
    tags: string list;
    timeout: float option;
  }

  val create :
    client:Cohttp_lwt_unix.Client.t ->
    route:Utilities.Openapi.http_route ->
    uri:Uri.t ->
    name:string ->
    description:string ->
    ?mime_type:string ->
    ?tags:string list ->
    ?timeout:float ->
    unit ->
    t

  val read : t -> string Lwt.t
end

module OpenAPIResourceTemplate : sig
  type t = private {
    client: Cohttp_lwt_unix.Client.t;
    route: Utilities.Openapi.http_route;
    uri_template: string;
    name: string;
    description: string;
    parameters: Yojson.Safe.t;
    tags: string list;
    timeout: float option;
  }

  val create :
    client:Cohttp_lwt_unix.Client.t ->
    route:Utilities.Openapi.http_route ->
    uri_template:string ->
    name:string ->
    description:string ->
    parameters:Yojson.Safe.t ->
    ?tags:string list ->
    ?timeout:float ->
    unit ->
    t

  val create_resource :
    t ->
    uri:string ->
    params:Yojson.Safe.t ->
    OpenAPIResource.t Lwt.t
end

class fast_mcp_openapi :
  openapi_spec:Yojson.Safe.t ->
  client:Cohttp_lwt_unix.Client.t ->
  ?name:string ->
  ?route_maps:route_map list ->
  ?route_map_fn:(Utilities.Openapi.http_route -> MCPType.t -> MCPType.t option) ->
  ?mcp_component_fn:(Utilities.Openapi.http_route -> < .. > -> unit) ->
  ?mcp_names:string String.Map.t ->
  ?tags:String.Set.t ->
  ?timeout:float ->
  unit ->
  object
    inherit Mcp.Server.t

    val mutable component_counts : component_counts
  end

(** {2 Schema Formatting and Display} *)

val format_json_for_description : Yojson.Safe.t -> string
(** Format a JSON value for display in a description *)

val format_description_with_responses :
  ?parameters:Utilities.Openapi.parameter_info list ->
  ?request_body:Utilities.Openapi.request_body_info ->
  responses:(string * Utilities.Openapi.response_info) list ->
  string ->
  string
(** Format a description with parameter, request body, and response information *)

val handle_array_parameter :
  Utilities.Openapi.parameter_info ->
  string list ->
  string list
(** Handle array parameters in query/path parameters *)

val clean_schema_for_display : Yojson.Safe.t -> Yojson.Safe.t
(** Clean up a schema for display purposes *)

val validate_schema_format : Yojson.Safe.t -> bool
(** Validate that a schema format is a valid OpenAPI format *)

val validate_schema_enum : Yojson.Safe.t -> bool
(** Validate that a schema enum contains only valid values *)

val validate_schema : Yojson.Safe.t -> (string, string) result
(** Validate an OpenAPI schema *)

val validate_parameter_value :
  Utilities.Openapi.parameter_info ->
  string ->
  (string, string) result
(** Validate a parameter value against its schema *)

(** {2 Route Mapping} *)

val determine_route_type : Utilities.Openapi.http_route -> route_map list -> route_map
(** Determine the FastMCP component type for a route based on mappings *)

val generate_default_name : Utilities.Openapi.http_route -> string String.Map.t -> string
(** Generate a default name for a component from a route *)

(** {2 Schema Generation and Resolution} *)

val generate_example_from_schema : Yojson.Safe.t -> Yojson.Safe.t
(** Generate example values from a JSON schema.
    Handles common types and formats:
    - object: generates example for each property
    - array: generates single example item
    - string: handles formats like date-time, email, uri, uuid
    - number/integer: uses example or default values
    - boolean: uses example or default true
    @param schema The schema to generate examples from
    @return A JSON value containing example data *)

val replace_ref_with_defs : Yojson.Safe.t -> Yojson.Safe.t -> Yojson.Safe.t
(** Replace $ref references in a schema with their definitions.
    Handles both internal (#/...) and external references.
    External references are replaced with a warning message.
    @param root_schema The root schema containing all definitions
    @param schema The schema to process
    @return The processed schema with references replaced *)

(** {2 Schema Functions} *)

val combine_schemas : Utilities.Openapi.http_route -> Yojson.Safe.t
(** Combine all schemas in a route into a single schema.
    This includes parameters, request body, responses, and schema definitions.
    References are resolved and replaced with $defs.
    @param route The HTTP route containing all schemas
    @return A combined schema with all references resolved *) 