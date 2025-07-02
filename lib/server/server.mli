open Core
open Mcp.Types
open Mcp.Shared
open Lwt.Syntax

(** Cache management *)
module Cache : sig
  type item = {
    value: Yojson.Safe.t;
    expires_at: float option;
  }

  type t = {
    mutable items: (string, item) Hashtbl.t;
    default_ttl: float option;
  }

  val create : ?default_ttl:float -> unit -> t
  val set : ?ttl:float -> t -> key:string -> value:Yojson.Safe.t -> unit
  val get : t -> string -> Yojson.Safe.t option
  val remove : t -> string -> unit
  val clear : t -> unit
end

(** Transport types for the server *)
type transport = 
  | Stdio
  | Http
  | Sse
  | StreamableHttp

(** Duplicate behavior when adding components *)
type duplicate_behavior = 
  | Warn 
  | Error 
  | Replace 
  | Ignore

(** Resource prefix format *)
type resource_prefix_format = 
  | Protocol
  | Path

(** Tool handler function type *)
type tool_handler = request_context -> Yojson.Safe.t -> content list Lwt.t

(** Resource handler function type *)
type resource_handler = request_context -> string list -> string Lwt.t

(** Prompt handler function type *)
type prompt_handler = request_context -> (string * string) list -> prompt_message list Lwt.t

(** Lifespan context manager type *)
type 'a lifespan_fn = 'a t -> 'a Lwt.t

(** Middleware context for request processing *)
and middleware_context = {
  message : Yojson.Safe.t;
  source : [ `Client | `Server ];
  type_ : [ `Request | `Notification ];
  method_ : string option;
  fastmcp_context : request_context option;
}

(** Middleware function type *)
and middleware_fn = middleware_context -> (middleware_context -> 'a Lwt.t) -> 'a Lwt.t

(** Custom HTTP route type *)
type custom_route = {
  path : string;
  methods : string list;
  handler : Starlette.Request.t -> Starlette.Response.t Lwt.t;
  name : string option;
  include_in_schema : bool;
}

(** Main FastMCP server type *)
and 'a t = {
  name : string;
  version : string option;
  instructions : string option;
  mutable tools : (string, tool_info) Hashtbl.t;
  mutable resources : (string, resource_info) Hashtbl.t;
  mutable resource_templates : (string, resource_template_info) Hashtbl.t;
  mutable prompts : (string, prompt_info) Hashtbl.t;
  middleware : middleware_fn list;
  lifespan : 'a lifespan_fn option;
  duplicate_behavior : duplicate_behavior;
  resource_prefix_format : resource_prefix_format;
  mask_error_details : bool;
  include_tags : string Set.t option;
  exclude_tags : string Set.t option;
  dependencies : string list;
  low_level_server : 'a t option;
  mutable _additional_http_routes : custom_route list;
}

(** Tool information *)
and tool_info = {
  name : string;
  description : string;
  handler : tool_handler;
  input_schema : Yojson.Safe.t;
  tags : string Set.t;
  enabled : bool;
}

(** Resource information *)
and resource_info = {
  uri : string;
  name : string option;
  description : string option;
  mime_type : string option;
  handler : resource_handler;
  tags : string Set.t;
  enabled : bool;
}

(** Resource template information *)
and resource_template_info = {
  uri_template : string;
  name : string option;
  description : string option;
  mime_type : string option;
  handler : resource_handler;
  tags : string Set.t;
  enabled : bool;
}

(** Prompt information *)
and prompt_info = {
  name : string;
  description : string;
  handler : prompt_handler;
  arguments : prompt_argument list option;
  tags : string Set.t;
  enabled : bool;
}

(** Authentication provider *)
type auth_provider = {
  validate : Yojson.Safe.t -> bool;
  name : string;
  description : string option;
}

val env_bearer_auth_provider : ?env_var:string -> ?description:string -> unit -> auth_provider

(** Notification system *)
type notification_options = {
  tools_changed : bool;
  resources_changed : bool;
  prompts_changed : bool;
}

type notification = {
  method_ : string;
  params : Yojson.Safe.t option;
}

val create_notification : string -> ?params:Yojson.Safe.t -> unit -> notification
val notify_tools_changed : 'a t -> unit
val notify_resources_changed : 'a t -> unit
val notify_prompts_changed : 'a t -> unit

(** HTTP/SSE Server Integration *)
val create_http_app : 
  'a t -> 
  ?path:string -> 
  ?middleware:Starlette.Middleware.t list -> 
  ?auth:auth_provider ->
  ?json_response:bool ->
  ?stateless_http:bool ->
  unit -> Starlette.App.t

val create_sse_app :
  'a t ->
  ?path:string ->
  ?message_path:string ->
  ?auth:auth_provider ->
  ?middleware:Starlette.Middleware.t list ->
  unit -> Starlette.App.t

(** OpenAPI Integration *)
type openapi_component = {
  name : string;
  schema : Yojson.Safe.t;
  examples : (string * Yojson.Safe.t) list;
}

val create_openapi_spec : 'a t -> Yojson.Safe.t

(** Route mapping for OpenAPI/FastAPI integration *)
type route_map = {
  path : string;
  method_ : string;
  operation_id : string;
  tool_name : string;
  arguments : (string * Yojson.Safe.t) list;
}

type route_map_fn = string -> string -> Yojson.Safe.t -> route_map option

val default_route_map_fn : route_map_fn

(** Component mounting system *)
type mounted_component = {
  prefix : string option;
  component : [ `Tool of tool_info | `Resource of resource_info | `Prompt of prompt_info ];
  enabled : bool;
}

val mount_component : 'a t -> mounted_component -> unit

(** Event store for HTTP/SSE *)
module Event_store : sig
  type event = {
    id : string;
    event_type : string;
    data : string;
    timestamp : float;
  }

  type t = {
    mutable events : event list;
    max_size : int;
  }

  val create : ?max_size:int -> unit -> t
  val add : t -> event_type:string -> data:string -> event
  val get_events : t -> ?since:float -> ?until:float -> unit -> event list
end

(** Dependency injection system *)
module Dependencies : sig
  type context = {
    server : 'a t;
    mutable notifications : notification list;
    mutable cache : (string * Yojson.Safe.t) list;
    event_store : Event_store.t option;
  }

  val create_context : 'a t -> ?event_store:Event_store.t -> unit -> context
  val get_context : unit -> context
  val with_context : context -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  val get_notifications : context -> notification list
  val add_notification : context -> notification -> unit
  val clear_notifications : context -> unit
  val get_cache_item : context -> string -> Yojson.Safe.t option
  val set_cache_item : context -> string -> Yojson.Safe.t -> unit
  val clear_cache : context -> unit
end

(** FastAPI/OpenAPI integration *)
val create_fastapi_app :
  'a t ->
  ?route_maps:route_map list ->
  ?route_map_fn:route_map_fn ->
  ?openapi_spec:Yojson.Safe.t ->
  ?auth:auth_provider ->
  ?middleware:Starlette.Middleware.t list ->
  unit -> Starlette.App.t

(** Create a new FastMCP server *)
val create :
  ?name:string ->
  ?version:string ->
  ?instructions:string ->
  ?middleware:middleware_fn list ->
  ?lifespan:'a lifespan_fn ->
  ?cache_expiration_seconds:float ->
  ?on_duplicate_tools:duplicate_behavior ->
  ?on_duplicate_resources:duplicate_behavior ->
  ?on_duplicate_prompts:duplicate_behavior ->
  ?resource_prefix_format:resource_prefix_format ->
  ?mask_error_details:bool ->
  ?dependencies:string list ->
  ?include_tags:string list ->
  ?exclude_tags:string list ->
  unit -> 'a t

(** Tool management *)
val add_tool : 'a t -> tool_info -> unit
val remove_tool : 'a t -> string -> unit
val get_tool : 'a t -> string -> tool_info option Lwt.t
val get_tools : 'a t -> (string, tool_info) Hashtbl.t Lwt.t

(** Resource management *)
module Resource : sig
  type mime_type = string

  type metadata = {
    name: string option;
    description: string option;
    tags: string list;
    custom: (string * string) list;
  }

  type content =
    | Text of string
    | Binary of bytes
    | Json of Yojson.Safe.t
    | Stream of string Lwt_stream.t

  type resource = {
    uri: string;
    mime_type: mime_type;
    metadata: metadata;
    content: content;
  }

  type template_param = {
    name: string;
    param_type: [ `String | `Int | `Float | `Bool ];
    description: string option;
    required: bool;
    default: string option;
  }

  type template = {
    uri_template: string;
    mime_type: mime_type;
    metadata: metadata;
    parameters: template_param list;
    generator: (string * string) list -> resource Lwt.t;
  }

  type t

  val create : unit -> t
  val add_resource : t -> resource -> unit
  val add_template : t -> template -> unit
  val get_resource : t -> string -> resource option
  val get_template : t -> string -> template option
  val remove_resource : t -> string -> unit
  val remove_template : t -> string -> unit
  val list_resources : t -> resource list
  val list_templates : t -> template list
  val get_by_tag : t -> string -> resource list
end

(** Prompt management *)
val add_prompt : 'a t -> prompt_info -> unit
val get_prompt : 'a t -> string -> prompt_info option Lwt.t
val get_prompts : 'a t -> (string, prompt_info) Hashtbl.t Lwt.t

(** Middleware *)
val add_middleware : 'a t -> middleware_fn -> unit
val error_handling_middleware : ?mask_error_details:bool -> middleware_fn
val logging_middleware : ?log_level:[`Debug | `Info | `Warning | `Error] -> middleware_fn
val auth_middleware : ?auth_provider:auth_provider -> middleware_fn

(** Tool decorator *)
val tool :
  ?name:string ->
  ?description:string ->
  ?tags:string list ->
  ?enabled:bool ->
  'a t -> 
  tool_handler ->
  tool_info

(** Resource decorator *)
val resource :
  string ->
  ?name:string option ->
  ?description:string option ->
  ?mime_type:string option ->
  ?tags:string list ->
  ?enabled:bool ->
  'a t ->
  resource_handler ->
  resource_info

(** Prompt decorator *)
val prompt :
  ?name:string ->
  ?description:string ->
  ?tags:string list ->
  ?enabled:bool ->
  'a t ->
  prompt_handler ->
  prompt_info

(** Server mounting and importing *)
val mount : 'a t -> 'b t -> ?prefix:string -> unit -> unit Lwt.t
val import_server : 'a t -> 'b t -> ?prefix:string -> unit -> unit Lwt.t

(** Running the server *)
val run_async : 'a t -> ?transport:transport -> unit -> unit Lwt.t
val run : 'a t -> ?transport:transport -> unit -> unit
val run_stdio_async : 'a t -> unit Lwt.t
val run_http_async : 
  'a t -> 
  ?transport:transport ->
  ?host:string -> 
  ?port:int -> 
  ?log_level:string -> 
  ?path:string -> 
  unit -> unit Lwt.t

(** HTTP application creation *)
val http_app : 
  'a t -> 
  ?path:string -> 
  ?middleware:Starlette.Middleware.t list -> 
  unit -> Starlette.App.t

(** Custom HTTP routes *)
val custom_route :
  string -> 
  string list -> 
  ?name:string -> 
  ?include_in_schema:bool -> 
  'a t ->
  (Starlette.Request.t -> Starlette.Response.t Lwt.t) ->
  unit

(** Utility functions *)
val should_enable_component : 'a t -> string Set.t -> bool -> bool
val add_resource_prefix : string -> string -> resource_prefix_format -> string
val remove_resource_prefix : string -> string -> resource_prefix_format -> string
val has_resource_prefix : string -> string -> resource_prefix_format -> bool

(** JSON serialization helpers *)
val tool_to_json : tool_info -> Yojson.Safe.t
val resource_to_json : resource_info -> Yojson.Safe.t
val prompt_to_json : prompt_info -> Yojson.Safe.t
val content_to_json : content -> Yojson.Safe.t
val prompt_result_to_json : prompt_result -> Yojson.Safe.t

(** Lifespan management *)
val default_lifespan : 'a t -> unit Lwt.t
val with_lifespan : 'a t -> (unit -> 'b Lwt.t) -> 'b Lwt.t

(** Resource template system *)
module Uri_template : sig
  type param = {
    name : string;
    default : string option;
    description : string option;
  }

  type t = {
    template : string;
    params : param list;
  }

  val create : string -> t
  val match_uri : string -> string -> (string * string) list option
  val substitute : string -> (string * string) list -> string
end

(** Proxy server support *)
module Proxy : sig
  type transport = {
    send : Yojson.Safe.t -> Yojson.Safe.t Lwt.t;
    close : unit -> unit Lwt.t;
  }

  type client = {
    transport : transport;
    mutable closed : bool;
  }

  val create_client : transport -> client
  val close : client -> unit Lwt.t
  val send : client -> Yojson.Safe.t -> Yojson.Safe.t Lwt.t
  val proxy_request : t -> client -> Yojson.Safe.t -> Yojson.Safe.t Lwt.t
end

(** Client transport integration *)
module Transport : sig
  type t = {
    connect : unit -> Proxy.transport Lwt.t;
    name : string;
    description : string option;
  }

  val create : connect:(unit -> Proxy.transport Lwt.t) -> ?description:string -> string -> t
  val stdio_transport : t
  val http_transport : ?base_url:string -> unit -> t
end

(** Stateless HTTP support *)
val create_stateless_http_app : 
  t ->
  ?path:string ->
  ?middleware:middleware list ->
  ?auth:auth_provider ->
  unit ->
  Starlette.App.t

(** Client creation *)
val create_client : Transport.t -> Proxy.client Lwt.t

(** Server from client *)
val server_from_client :
  Proxy.client ->
  ?name:string ->
  ?version:string ->
  ?instructions:string ->
  ?middleware:middleware list ->
  unit ->
  t

(** Notification system *)
type notification = {
  method_ : string;
  params : Yojson.Safe.t option;
}

val create_notification : string -> ?params:Yojson.Safe.t -> unit -> notification
val notify_tools_changed : 'a t -> unit
val notify_resources_changed : 'a t -> unit
val notify_prompts_changed : 'a t -> unit

(** Notification system *)
type notification_options = {
  tools_changed : bool;
  resources_changed : bool;
  prompts_changed : bool;
}

type notification = {
  method_ : string;
  params : Yojson.Safe.t option;
}

val create_notification : string -> ?params:Yojson.Safe.t -> unit -> notification
val notify_tools_changed : 'a t -> unit
val notify_resources_changed : 'a t -> unit
val notify_prompts_changed : 'a t -> unit

(** Component management system *)
module Component : sig
  type tag = string
  type component_id = string

  type 'a t = {
    id: component_id;
    name: string option;
    description: string option;
    tags: tag list;
    enabled: bool;
    data: 'a;
  }

  val create : ?name:string -> ?description:string -> ?tags:tag list -> ?enabled:bool -> 'a -> 'a t
  val is_enabled : 'a t -> bool
  val has_tag : 'a t -> tag -> bool
  val matches_tags : 'a t -> include_tags:tag list option -> exclude_tags:tag list option -> bool
end

(** OpenAPI integration *)
module OpenAPI : sig
  type schema = {
    type_: string;
    format: string option;
    description: string option;
    required: bool;
    enum: string list option;
    items: schema option;
    properties: (string * schema) list option;
  }
  (** Type representing an OpenAPI schema *)

  type parameter = {
    name: string;
    location: [`Path | `Query | `Header | `Cookie];
    description: string option;
    required: bool;
    schema: schema;
  }
  (** Type representing an OpenAPI parameter *)

  type response = {
    code: int;
    description: string;
    content: (string * schema) list;
  }
  (** Type representing an OpenAPI response *)

  type operation = {
    operation_id: string;
    summary: string option;
    description: string option;
    parameters: parameter list;
    request_body: schema option;
    responses: response list;
    tags: string list;
  }
  (** Type representing an OpenAPI operation *)

  type path = {
    path: string;
    get: operation option;
    post: operation option;
    put: operation option;
    delete: operation option;
    parameters: parameter list;
  }
  (** Type representing an OpenAPI path *)

  type spec = {
    openapi: string;
    info: Yojson.Safe.t;
    paths: path list;
    components: Yojson.Safe.t;
  }
  (** Type representing an OpenAPI specification *)

  val create_schema :
    ?format:string ->
    ?description:string ->
    ?required:bool ->
    ?enum:string list ->
    ?items:schema ->
    ?properties:(string * schema) list ->
    string ->
    schema
  (** Create an OpenAPI schema *)

  val create_parameter :
    ?description:string ->
    ?required:bool ->
    name:string ->
    location:[`Path | `Query | `Header | `Cookie] ->
    schema:schema ->
    unit ->
    parameter
  (** Create an OpenAPI parameter *)

  val create_response :
    code:int ->
    description:string ->
    content:(string * schema) list ->
    response
  (** Create an OpenAPI response *)

  val create_operation :
    ?summary:string ->
    ?description:string ->
    ?parameters:parameter list ->
    ?request_body:schema ->
    ?responses:response list ->
    ?tags:string list ->
    operation_id:string ->
    unit ->
    operation
  (** Create an OpenAPI operation *)

  val create_path :
    ?get:operation ->
    ?post:operation ->
    ?put:operation ->
    ?delete:operation ->
    ?parameters:parameter list ->
    path:string ->
    unit ->
    path
  (** Create an OpenAPI path *)

  val create_spec :
    openapi:string ->
    info:Yojson.Safe.t ->
    paths:path list ->
    components:Yojson.Safe.t ->
    spec
  (** Create an OpenAPI specification *)

  val spec_to_json : spec -> Yojson.Safe.t
  (** Convert an OpenAPI specification to JSON *)
end

(** Server type with component management *)
type component_store = {
  mutable tools: (string, Tool.t Component.t) Hashtbl.t;
  mutable resources: (string, Resource.t Component.t) Hashtbl.t;
  mutable prompts: (string, Prompt.t Component.t) Hashtbl.t;
}

type t = {
  name: string;
  version: string option;
  instructions: string option;
  components: component_store;
  event_store: Event_store.t option;
  openapi: OpenAPI.t;
  middleware: middleware list;
  mutable lifespan: (t -> unit Lwt.t) option;
}

val create : ?name:string -> ?version:string -> ?instructions:string -> ?middleware:middleware list -> ?event_store:Event_store.t -> unit -> t 

(** Authentication System *)
module Auth : sig
  type credentials = {
    username: string option;
    password: string option;
    token: string option;
    api_key: string option;
  }

  type permission = [
    | `Read
    | `Write
    | `Execute
    | `Admin
  ]

  type role = private {
    name: string;
    permissions: permission list;
    metadata: (string * string) list;
  }

  type session = private {
    id: string;
    user: string;
    roles: role list;
    created_at: float;
    expires_at: float option;
    metadata: (string * string) list;
  }

  type authenticator = credentials -> session option Lwt.t
  type t

  val create : ?default_authenticator:string -> unit -> t
  val register_authenticator : t -> string -> authenticator -> unit
  val register_role : t -> role -> unit
  val authenticate : t -> ?authenticator:string -> credentials -> session option Lwt.t
  val create_session : t -> string -> role list -> session
  val get_session : t -> string -> session option
  val remove_session : t -> string -> unit
  val has_permission : t -> session -> permission -> bool
  val cleanup_expired_sessions : t -> unit
end

(** Middleware pipeline *)
module Middleware : sig
  type context = {
    request: Yojson.Safe.t;
    mutable response: Yojson.Safe.t option;
    server: t;
    auth: Auth.credentials option;
  }

  type t = context -> (context -> unit Lwt.t) -> unit Lwt.t

  val compose : t list -> t
  val auth_middleware : Auth.provider -> t
  val logging_middleware : t
  val error_middleware : t
end

(** Component mounting system *)
module Mount : sig
  type prefix = string option

  type mounted_server = {
    prefix: prefix;
    server: t;
    resource_prefix_format: [`Protocol | `Path];
  }

  val add_prefix : prefix -> string -> string
  val add_resource_prefix : prefix -> string -> [`Protocol | `Path] -> string
end

(** Server type with new fields *)
type t = {
  name: string;
  version: string option;
  instructions: string option;
  components: component_store;
  event_store: Event_store.t option;
  openapi: OpenAPI.t;
  middleware: Middleware.t list;
  mutable lifespan: (t -> unit Lwt.t) option;
  auth: Auth.provider option;
  cache: Cache.t;
  mounted_servers: Mount.mounted_server list;
}

val create :
  ?name:string ->
  ?version:string ->
  ?instructions:string ->
  ?middleware:Middleware.t list ->
  ?event_store:Event_store.t ->
  ?auth:Auth.provider ->
  ?cache_ttl:float ->
  unit ->
  t 

(** WebSocket support *)
module WebSocket : sig
  type message = [
    | `Text of string
    | `Binary of string
    | `Close of int * string
    | `Ping of string
    | `Pong of string
  ]

  type connection = {
    id: string;
    send: message -> unit Lwt.t;
    close: int -> string -> unit Lwt.t;
    is_closed: unit -> bool;
    last_ping: float ref;
  }

  type handler = connection -> message -> unit Lwt.t

  type config = {
    ping_interval: float;
    ping_timeout: float;
    max_message_size: int;
  }

  val default_config : config

  val create_connection :
    id:string ->
    send:(message -> unit Lwt.t) ->
    close:(int -> string -> unit Lwt.t) ->
    connection

  val handle_connection : config -> handler -> connection -> unit Lwt.t
end

(** Session management *)
module Session : sig
  type data = {
    id: string;
    created_at: float;
    last_accessed: float ref;
    data: (string, Yojson.Safe.t) Hashtbl.t;
  }

  type config = {
    timeout: float;
    cleanup_interval: float;
  }

  type t = {
    sessions: (string, data) Hashtbl.t;
    config: config;
  }

  val default_config : config

  val create : ?config:config -> unit -> t

  val create_session : t -> data

  val get_session : t -> string -> data option

  val delete_session : t -> string -> unit

  val set_data : data -> string -> Yojson.Safe.t -> unit

  val get_data : data -> string -> Yojson.Safe.t option

  val remove_data : data -> string -> unit
end

(** Plugin system *)
module Plugin : sig
  type hook = [
    | `Init
    | `Start
    | `Stop
    | `Request
    | `Response
    | `Error
  ]

  type context = {
    server: t;
    hook: hook;
    data: Yojson.Safe.t;
    timestamp: float;
  }

  type handler = context -> unit Lwt.t
  type plugin = private {
    name: string;
    version: string;
    description: string option;
    handlers: (hook * handler) list;
    enabled: bool;
  }
  type t

  val create : unit -> t
  val register : t -> plugin -> unit
  val unregister : t -> string -> unit
  val trigger : t -> hook -> Yojson.Safe.t -> unit Lwt.t
  val get_plugin : t -> string -> plugin option
  val list_plugins : t -> plugin list
  val enable : t -> string -> unit
  val disable : t -> string -> unit
end

(** Metrics collection *)
module Metrics : sig
  type counter = {
    name: string;
    description: string option;
    value: int ref;
  }

  type gauge = {
    name: string;
    description: string option;
    value: float ref;
  }

  type histogram = {
    name: string;
    description: string option;
    buckets: float array;
    counts: int array ref;
    sum: float ref;
  }

  type t = {
    counters: (string, counter) Hashtbl.t;
    gauges: (string, gauge) Hashtbl.t;
    histograms: (string, histogram) Hashtbl.t;
  }

  val create : unit -> t

  val create_counter : t -> name:string -> ?description:string -> unit -> counter

  val create_gauge : t -> name:string -> ?description:string -> unit -> gauge

  val create_histogram :
    t ->
    name:string ->
    ?description:string ->
    buckets:float list ->
    unit -> histogram

  val inc_counter : counter -> unit

  val add_counter : counter -> int -> unit

  val set_gauge : gauge -> float -> unit

  val inc_gauge : gauge -> unit

  val dec_gauge : gauge -> unit

  val observe_histogram : histogram -> float -> unit

  val get_counter : counter -> int

  val get_gauge : gauge -> float

  val get_histogram_counts : histogram -> int array

  val get_histogram_sum : histogram -> float
end

(** Event System *)
module Event : sig
  type level = [
    | `Debug
    | `Info
    | `Warning
    | `Error
    | `Critical
  ]

  type event = {
    id: string;
    timestamp: float;
    level: level;
    category: string;
    name: string;
    data: Yojson.Safe.t option;
    metadata: (string * string) list;
  }

  type buffer = {
    max_size: int;
    events: event Queue.t;
    mutable dropped_count: int;
  }

  type handler = event -> unit Lwt.t
  type t

  val create : ?default_level:level -> unit -> t
  val create_buffer : ?max_size:int -> unit -> buffer
  val add_handler : t -> handler -> t
  val create_event : t -> category:string -> name:string -> ?level:level -> ?data:Yojson.Safe.t -> ?metadata:(string * string) list -> unit -> event
  val emit : t -> event -> unit Lwt.t
  val buffer_event : t -> string -> event -> unit
  val get_buffer : t -> string -> buffer option
  val clear_buffer : t -> string -> unit
  val remove_buffer : t -> string -> unit
end

(** Logging System *)
module Log : sig
  type level = [
    | `Debug
    | `Info
    | `Warning
    | `Error
    | `Critical
  ]

  type message = {
    timestamp: float;
    level: level;
    source: string;
    message: string;
    metadata: (string * string) list;
  }

  type handler = message -> unit Lwt.t

  type logger = {
    name: string;
    level: level;
    handlers: handler list;
    metadata: (string * string) list;
  }

  type t

  val create : ?default_level:level -> ?default_handlers:handler list -> unit -> t

  val create_logger :
    t ->
    string ->
    ?level:level ->
    ?handlers:handler list ->
    ?metadata:(string * string) list ->
    unit -> logger

  val log : logger -> level -> string -> unit Lwt.t

  val debug : logger -> string -> unit Lwt.t
  val info : logger -> string -> unit Lwt.t
  val warning : logger -> string -> unit Lwt.t
  val error : logger -> string -> unit Lwt.t
  val critical : logger -> string -> unit Lwt.t
end

(** Configuration Management *)
module Config : sig
  type validation_error = {
    path: string list;
    message: string;
  }

  type validator = Yojson.Safe.t -> (unit, validation_error) result

  type value =
    | String of string
    | Int of int
    | Float of float
    | Bool of bool
    | Json of Yojson.Safe.t

  type config_source =
    | Environment
    | File of string
    | Memory

  type t

  val create : ?required:string list -> unit -> t
  val set : t -> string -> value -> config_source -> unit
  val get : t -> string -> value option
  val remove : t -> string -> unit
  val add_validator : t -> string -> validator -> unit
  val validate : t -> (unit, validation_error) result
  val load_env : t -> string -> unit
  val load_file : t -> string -> (unit, string) result
end

(** Task Management *)
module Task : sig
  type status = [
    | `Pending
    | `Running
    | `Completed
    | `Failed
    | `Cancelled
  ]

  type progress = {
    current: int;
    total: int option;
    message: string option;
  }

  type error = {
    code: string;
    message: string;
    details: Yojson.Safe.t option;
  }

  type result = {
    data: Yojson.Safe.t option;
    metadata: (string * string) list;
  }

  type task = {
    id: string;
    name: string;
    status: status;
    created_at: float;
    started_at: float option;
    completed_at: float option;
    progress: progress option;
    error: error option;
    result: result option;
    metadata: (string * string) list;
  }

  type t

  val create : unit -> t
  val create_task : t -> name:string -> ?metadata:(string * string) list -> unit -> task
  val update_status : t -> task -> status -> unit Lwt.t
  val update_progress : t -> task -> ?current:int -> ?total:int -> ?message:string -> unit -> unit Lwt.t
  val complete : t -> task -> ?data:Yojson.Safe.t -> ?metadata:(string * string) list -> unit -> unit Lwt.t
  val fail : t -> task -> code:string -> message:string -> ?details:Yojson.Safe.t -> unit -> unit Lwt.t
  val cancel : t -> task -> unit Lwt.t
  val get_task : t -> string -> task option
  val list_tasks : t -> task list
  val add_status_handler : t -> (status -> task -> unit Lwt.t) -> t
  val add_progress_handler : t -> (progress -> task -> unit Lwt.t) -> t
end

(** Cache System *)
module Cache : sig
  type entry = {
    value: Yojson.Safe.t;
    expires: float option;
    tags: string list;
  }

  type t

  val create : ?cleanup_interval:float -> unit -> t
  val start_cleanup : t -> unit
  val stop_cleanup : t -> unit
  val set : ?expires:float -> ?tags:string list -> t -> string -> Yojson.Safe.t -> unit
  val get : t -> string -> Yojson.Safe.t option
  val remove : t -> string -> unit
  val clear : t -> unit
  val get_by_tag : t -> string -> (string * Yojson.Safe.t) list
end

(** Validation System *)
module Validation : sig
  type error = {
    path: string list;
    code: string;
    message: string;
    details: Yojson.Safe.t option;
  }

  type result = (unit, error list) result
  type rule = Yojson.Safe.t -> (unit, error) result

  type schema = {
    rules: (string list * rule) list;
    required: string list;
    optional: string list;
  }

  type t

  val create : unit -> t
  val add_schema : t -> string -> schema -> unit
  val remove_schema : t -> string -> unit
  val get_schema : t -> string -> schema option
  val validate : t -> string -> Yojson.Safe.t -> result
end

(** Error Handling System *)
module Error : sig
  type severity = [
    | `Debug
    | `Info
    | `Warning
    | `Error
    | `Critical
  ]

  type error = {
    code: string;
    message: string;
    details: Yojson.Safe.t option;
    severity: severity;
    source: string option;
    timestamp: float;
    trace: string list;
  }

  type handler = error -> unit Lwt.t
  type t

  val create : ?max_errors:int -> unit -> t
  val add_handler : t -> handler -> t
  val create_error : ?details:Yojson.Safe.t option -> ?severity:severity -> ?source:string -> ?trace:string list -> string -> string -> error
  val handle : t -> error -> unit Lwt.t
  val get_errors : t -> error list
  val clear_errors : t -> unit
end

(** Notification System *)
module Notification : sig
  type level = [
    | `Info
    | `Warning
    | `Error
  ]

  type notification = {
    id: string;
    level: level;
    title: string;
    message: string;
    data: Yojson.Safe.t option;
    timestamp: float;
    expires: float option;
    actions: (string * string) list;
  }

  type handler = notification -> unit Lwt.t
  type t

  val create : ?default_expiry:float option -> unit -> t
  val add_handler : t -> handler -> t
  val notify : t -> level:level -> title:string -> message:string -> ?data:Yojson.Safe.t -> ?expires:float -> ?actions:(string * string) list -> unit -> unit Lwt.t
  val dismiss : t -> string -> unit
  val get_notification : t -> string -> notification option
  val list_notifications : t -> notification list
  val cleanup : t -> unit
end

(** Rate Limiting *)
module RateLimit : sig
  type window = [
    | `Second
    | `Minute
    | `Hour
    | `Day
  ]

  type limit = {
    max_requests: int;
    window: window;
  }

  type t

  val create : ?cleanup_interval:float -> unit -> t

  val add_limit :
    t ->
    key:string ->
    max_requests:int ->
    window:window ->
    unit

  val remove_limit : t -> string -> unit

  val check : t -> key:string -> client_id:string -> bool

  val get_remaining : t -> key:string -> client_id:string -> int option
end

(** Metrics Collection *)
module Metrics : sig
  type metric_type = [
    | `Counter
    | `Gauge
    | `Histogram
  ]

  type value =
    | Int of int
    | Float of float

  type metric = {
    name: string;
    metric_type: metric_type;
    description: string option;
    labels: (string * string) list;
    mutable value: value;
    mutable samples: value list;
  }

  type t

  val create : unit -> t
  val register_collector : t -> (unit -> unit Lwt.t) -> t
  val create_counter : t -> name:string -> ?description:string -> ?labels:(string * string) list -> ?initial:int -> unit -> metric
  val create_gauge : t -> name:string -> ?description:string -> ?labels:(string * string) list -> ?initial:float -> unit -> metric
  val create_histogram : t -> name:string -> ?description:string -> ?labels:(string * string) list -> ?buckets:float list -> unit -> metric
  val inc_counter : metric -> unit
  val dec_counter : metric -> unit
  val set_gauge : metric -> float -> unit
  val inc_gauge : metric -> float -> unit
  val dec_gauge : metric -> float -> unit
  val observe_histogram : metric -> float -> unit
  val collect : t -> unit Lwt.t
end

(** Dependency Injection *)
module DI : sig
  type injectable = ..
  type injectable +=
    | Cache of Cache.t
    | Task of Task.t
    | Notification of Notification.t
    | RateLimit of RateLimit.t
    | Metrics of Metrics.t
    | Error of Error.t
    | Validation of Validation.t

  type t

  val create : unit -> t

  val register : t -> name:string -> factory:(unit -> injectable) -> unit

  val register_instance : t -> name:string -> instance:injectable -> unit

  val get : t -> string -> injectable option

  val remove : t -> string -> unit

  val clear : t -> unit

  val get_cache : t -> Cache.t option

  val get_task : t -> Task.t option

  val get_notification : t -> Notification.t option

  val get_rate_limit : t -> RateLimit.t option

  val get_metrics : t -> Metrics.t option

  val get_error : t -> Error.t option

  val get_validation : t -> Validation.t option
end

(** FastMCP Server implementation in OCaml *)

(** Security System *)
module Security : sig
  type permission = [
    | `Read
    | `Write
    | `Execute
    | `Admin
  ]

  type role = {
    name: string;
    permissions: permission list;
    metadata: (string * string) list;
  }

  type principal = {
    id: string;
    roles: role list;
    attributes: (string * string) list;
  }

  type policy = {
    name: string;
    effect: [`Allow | `Deny];
    principals: string list;
    resources: string list;
    actions: permission list;
    conditions: (principal -> bool) list;
  }

  type t

  val create : unit -> t
  val add_role : t -> name:string -> permissions:permission list -> ?metadata:(string * string) list -> unit -> unit
  val add_principal : t -> id:string -> roles:role list -> ?attributes:(string * string) list -> unit -> unit
  val add_policy : t -> name:string -> effect:[`Allow | `Deny] -> principals:string list -> resources:string list -> actions:permission list -> ?conditions:(principal -> bool) list -> unit -> unit
  val get_role : t -> string -> role option
  val get_principal : t -> string -> principal option
  val get_policy : t -> string -> policy option
  val check_permission : t -> string -> string -> permission -> bool
end

(** Middleware Pipeline *)
module Middleware : sig
  type context = {
    request_id: string;
    timestamp: float;
    method_: string;
    path: string;
    headers: (string * string) list;
    body: string option;
    principal: Security.principal option;
    metadata: (string * string) list;
  }

  type handler = context -> (context -> unit Lwt.t) -> unit Lwt.t
  type t

  val create : ?handlers:handler list -> ?error_handlers:(exn -> unit Lwt.t) list -> unit -> t
  val add_handler : t -> handler -> t
  val add_error_handler : t -> (exn -> unit Lwt.t) -> t
  val create_context :
    ?headers:(string * string) list ->
    ?body:string option ->
    ?principal:Security.principal option ->
    ?metadata:(string * string) list ->
    method_:string ->
    path:string ->
    unit ->
    context
  val execute : t -> context -> (context -> unit Lwt.t) -> unit Lwt.t
end

(** Transport Layer *)
module Transport : sig
  type message = {
    id: string;
    method_: string;
    params: Yojson.Safe.t;
    timestamp: float;
  }

  type response = {
    id: string;
    result: (Yojson.Safe.t, string) result;
    timestamp: float;
  }

  type handler = message -> response Lwt.t

  type subscription = {
    id: string;
    pattern: string;
    callback: message -> unit Lwt.t;
  }

  type t

  val create : unit -> t
  val add_handler : t -> method_:string -> handler -> unit
  val remove_handler : t -> string -> unit
  val subscribe : t -> pattern:string -> (message -> unit Lwt.t) -> string
  val unsubscribe : t -> string -> unit
  val create_message : method_:string -> params:Yojson.Safe.t -> unit -> message
  val create_response : message -> (Yojson.Safe.t, string) result -> response
  val handle_message : t -> message -> response Lwt.t
  val notify_subscribers : t -> message -> unit Lwt.t
  val process_message : t -> message -> response Lwt.t
  val send_message : t -> message -> response Lwt.t
  val add_before_send : t -> (message -> message Lwt.t) -> t
  val add_after_receive : t -> (message -> message Lwt.t) -> t
end

(** State Management System *)
module State : sig
  type change = {
    key: string;
    old_value: Yojson.Safe.t option;
    new_value: Yojson.Safe.t option;
    timestamp: float;
    source: string option;
  }

  type observer = change -> unit Lwt.t
  type t

  val create : ?max_history:int -> unit -> t
  val set : t -> string -> Yojson.Safe.t -> ?source:string -> unit -> unit Lwt.t
  val get : t -> string -> Yojson.Safe.t option
  val remove : t -> string -> ?source:string -> unit -> unit Lwt.t
  val observe : t -> string -> observer -> unit
  val unobserve : t -> string -> observer -> unit
  val get_history : t -> change list
end

(** Lifecycle Management *)
module Lifecycle : sig
  type phase = [
    | `Initializing
    | `Starting
    | `Running
    | `Stopping
    | `Stopped
  ]

  type handler = phase -> unit Lwt.t
  type t

  val create : unit -> t
  val add_handler : t -> handler -> t
  val add_component : t -> string -> (phase -> unit Lwt.t) -> t
  val get_phase : t -> phase
  val initialize : t -> unit Lwt.t
  val start : t -> unit Lwt.t
  val stop : t -> unit Lwt.t
end

(** Protocol Adapters *)
module Protocol : sig
  type request = {
    id: string;
    method_: string;
    params: Yojson.Safe.t;
    metadata: (string * string) list;
  }

  type response = {
    id: string;
    result: (Yojson.Safe.t, string) result;
    metadata: (string * string) list;
  }

  type handler = request -> response Lwt.t
  type middleware = request -> (request -> response Lwt.t) -> response Lwt.t
  type adapter = private {
    name: string;
    version: string;
    description: string option;
    handler: handler;
    middleware: middleware list;
  }
  type t

  val create : ?default_adapter:string -> unit -> t
  val register : t -> adapter -> unit
  val unregister : t -> string -> unit
  val get_adapter : t -> string -> adapter option
  val list_adapters : t -> adapter list
  val create_request : method_:string -> params:Yojson.Safe.t -> ?metadata:(string * string) list -> unit -> request
  val create_response : request -> (Yojson.Safe.t, string) result -> ?metadata:(string * string) list -> unit -> response
  val handle : t -> request -> response Lwt.t
end

(** Main server creation and management functions *)

(** Create a new FastMCP server with all managers *)
val create_server :
  ?name:string ->
  ?version:string ->
  ?instructions:string ->
  ?middleware:middleware_fn list ->
  ?lifespan:'a lifespan_fn ->
  ?cache_expiration_seconds:float ->
  ?on_duplicate_tools:duplicate_behavior ->
  ?on_duplicate_resources:duplicate_behavior ->
  ?on_duplicate_prompts:duplicate_behavior ->
  ?resource_prefix_format:resource_prefix_format ->
  ?mask_error_details:bool ->
  ?tools:tool_handler list ->
  ?dependencies:string list ->
  ?include_tags:string list ->
  ?exclude_tags:string list ->
  ?auth:auth_provider ->
  ?tool_serializer:(Yojson.Safe.t -> string) ->
  unit -> 'a server

(** Server management functions *)
val add_tool_v2 : 'a server -> tool_info -> unit
val remove_tool_v2 : 'a server -> string -> unit
val add_resource_v2 : 'a server -> resource_info -> unit
val add_template_v2 : 'a server -> resource_template_info -> unit
val add_prompt_v2 : 'a server -> prompt_info -> unit

(** Server mounting and importing *)
val mount_server_v2 : 'a server -> 'b server -> ?prefix:string -> ?as_proxy:bool -> unit -> unit Lwt.t
val import_server_v2 : 'a server -> 'b server -> ?prefix:string -> unit -> unit Lwt.t

(** Protocol handlers *)
val handle_list_tools_v2 : 'a server -> tool_info list Lwt.t
val handle_call_tool_v2 : 'a server -> string -> Yojson.Safe.t -> content list Lwt.t
val handle_list_resources_v2 : 'a server -> resource_info list Lwt.t
val handle_read_resource_v2 : 'a server -> string -> content list Lwt.t
val handle_list_prompts_v2 : 'a server -> prompt_info list Lwt.t
val handle_get_prompt_v2 : 'a server -> string -> (string * string) list -> prompt_result Lwt.t

(** Server execution *)
val run_async_v2 : 'a server -> ?transport:transport -> unit -> unit Lwt.t
val run_v2 : 'a server -> ?transport:transport -> unit -> unit
val run_stdio_async_v2 : 'a server -> unit Lwt.t

(** HTTP application creation *)
val create_stateless_http_app : 'a server -> ?path:string -> ?middleware:middleware list -> ?auth:auth_provider -> unit -> Http.t

(** Proxy and integration support *)
val as_proxy_v2 : [`Client of Proxy.client | `URL of string | `Config of Yojson.Safe.t] -> 'a server
val from_openapi_v2 : Yojson.Safe.t -> ?route_maps:FastAPI.route_map list -> ?route_map_fn:FastAPI.route_map_fn -> ?auth:auth_provider -> unit -> 'a server Lwt.t
val from_fastapi_v2 : Yojson.Safe.t -> ?name:string -> ?route_maps:FastAPI.route_map list -> ?route_map_fn:FastAPI.route_map_fn -> ?auth:auth_provider -> unit -> 'a server Lwt.t

(** Client and transport support *)
val create_client : Transport.t -> Proxy.client Lwt.t
val server_from_client : Proxy.client -> ?name:string -> ?version:string -> ?instructions:string -> ?middleware:middleware_fn list -> unit -> 'a server

(** OpenAPI and FastAPI modules *)
module FastAPI : sig
  type route_map = {
    path: string;
    method_: string;
    operation_id: string;
    tool_name: string;
    arguments: (string * Yojson.Safe.t) list;
  }

  type route_map_fn = string -> string -> Yojson.Safe.t -> route_map option

  val default_route_map_fn : route_map_fn
  val from_openapi_spec : Yojson.Safe.t -> ?route_maps:route_map list -> ?route_map_fn:route_map_fn -> 'a server -> 'a server Lwt.t
  val from_fastapi_app : Yojson.Safe.t -> ?name:string -> ?route_maps:route_map list -> ?route_map_fn:route_map_fn -> 'a server -> 'a server Lwt.t
end

(** Proxy module *)
module Proxy : sig
  type config = {
    base_url: string;
    timeout: float option;
    headers: (string * string) list;
  }

  type client = {
    config: config;
    mutable closed: bool;
  }

  type transport = {
    send: Yojson.Safe.t -> Yojson.Safe.t Lwt.t;
    close: unit -> unit Lwt.t;
  }

  val create_transport : ?timeout:float option -> ?headers:(string * string) list -> base_url:string -> unit -> transport
  val create_client : transport -> client
  val close : client -> unit Lwt.t
  val send : client -> Yojson.Safe.t -> Yojson.Safe.t Lwt.t
  val proxy_request : 'a server -> client -> Yojson.Safe.t -> Yojson.Safe.t Lwt.t
end

(** Utility functions *)
module Util : sig
  val generate_uuid : unit -> string
  val current_timestamp : unit -> float
  val format_timestamp : float -> string
  val parse_uri : string -> string * string
  val join_uri : string -> string -> string
  val normalize_path : string -> string
  val merge_json : Yojson.Safe.t -> Yojson.Safe.t -> Yojson.Safe.t
end 