(** HTTP Application Module for OxFastMCP

    Provides HTTP application types and utilities for creating SSE and
    Streamable HTTP server applications. This module handles request context
    management, authentication middleware integration, and ASGI-style
    application creation. *)

open! Core
open! Async

(** {1 Types} *)

(** HTTP methods *)
module Http_method : sig
  type t = GET | POST | PUT | DELETE | PATCH | OPTIONS | HEAD
  [@@deriving sexp, compare, equal, enumerate]

  val to_string : t -> string
  val of_string : string -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val t_of_yojson : Yojson.Safe.t -> t
end

(** HTTP request representation *)
module Request : sig
  type t = {
    method_ : Http_method.t;
    path : string;
    headers : (string * string) list;
    query_params : (string * string) list;
    body : string option;
    scope : Yojson.Safe.t option;
  }

  val create :
    method_:Http_method.t ->
    path:string ->
    ?headers:(string * string) list ->
    ?query_params:(string * string) list ->
    ?body:string ->
    ?scope:Yojson.Safe.t ->
    unit ->
    t

  val get_header : t -> name:string -> string option
  val get_query_param : t -> name:string -> string option
end

(** HTTP response representation *)
module Response : sig
  type t = {
    status : int;
    headers : (string * string) list;
    body : string option;
  }
  [@@deriving sexp]

  val create :
    ?status:int -> ?headers:(string * string) list -> ?body:string -> unit -> t

  val ok : ?body:string -> unit -> t
  val created : ?body:string -> unit -> t
  val no_content : unit -> t
  val bad_request : ?body:string -> unit -> t
  val unauthorized : ?body:string -> unit -> t
  val forbidden : ?body:string -> unit -> t
  val not_found : ?body:string -> unit -> t
  val internal_server_error : ?body:string -> unit -> t
  val json : ?status:int -> Yojson.Safe.t -> t
end

(** Route definition *)
module Route : sig
  type handler = Request.t -> Response.t Deferred.t

  type t = {
    path : string;
    methods : Http_method.t list;
    handler : handler;
    name : string option;
  }

  val create :
    path:string -> ?methods:Http_method.t list -> ?name:string -> handler -> t
end

(** Middleware definition *)
module Middleware : sig
  type next = Request.t -> Response.t Deferred.t
  type t = next -> next

  val identity : t
  val compose : t list -> t
end

(** Application state *)
module App_state : sig
  type t

  val create : unit -> t
  val set_fastmcp_server : t -> Yojson.Safe.t -> unit
  val get_fastmcp_server : t -> Yojson.Safe.t option
  val set_path : t -> string -> unit
  val get_path : t -> string option
  val set_data : t -> key:string -> data:Yojson.Safe.t -> unit
  val get_data : t -> key:string -> Yojson.Safe.t option
end

(** HTTP Application *)
module App : sig
  type lifespan = unit -> unit Deferred.t

  type t = {
    routes : Route.t list;
    middleware : Middleware.t list;
    debug : bool;
    lifespan : lifespan option;
    state : App_state.t;
  }

  val create :
    ?routes:Route.t list ->
    ?middleware:Middleware.t list ->
    ?debug:bool ->
    ?lifespan:lifespan ->
    unit ->
    t

  val with_state : t -> f:(App_state.t -> unit) -> t
  val get_lifespan : t -> lifespan option
end

(** {1 Request Context Management} *)

val get_current_request : unit -> Request.t option
(** Get the current HTTP request from context *)

val with_http_request : Request.t -> f:(unit -> 'a Deferred.t) -> 'a Deferred.t
(** Set HTTP request in context for the duration of a function call *)

(** {1 Middleware} *)

val request_context_middleware : Middleware.t
(** Middleware that stores each request in a context variable *)

(** {1 Streamable HTTP ASGI App Wrapper} *)

module Streamable_http_asgi_app : sig
  type session_manager = { handle_request : Request.t -> Response.t Deferred.t }
  type t

  val create : session_manager:session_manager -> t
  val handle_request : t -> Request.t -> Response.t Deferred.t
end

(** {1 SSE Transport}
    **)

module Sse_transport : sig
  type connection
  type t

  val create : message_path:string -> t

  val connect_sse :
    t -> Request.t -> (int * (unit -> unit Deferred.t)) Deferred.t
  (** Establish SSE connection, returns connection ID and cleanup function **)

  val broadcast_message : t -> Yojson.Safe.t -> unit
  (** Broadcast a message to all active SSE connections **)

  val handle_post_message : t -> Request.t -> Response.t Deferred.t
  (** Handle POST messages and broadcast to SSE connections **)
end

(** {1 Session Manager Placeholder} *)

module Session_manager : sig
  type t

  val create : ?json_response:bool -> ?stateless:bool -> unit -> t
  val run : t -> (unit -> unit Deferred.t) Deferred.t
end

(** {1 Authentication Configuration} *)

module Auth_config : sig
  type t = {
    required_scopes : string list;
    get_middleware : unit -> Middleware.t list;
    get_routes : mcp_path:string -> Route.t list;
    get_resource_url : string -> string option;
  }

  val get_middleware : t -> Middleware.t list
  val get_routes : t -> mcp_path:string -> Route.t list
  val get_resource_url : t -> string -> string option
end

(** {1 Application Creation} *)

val build_resource_metadata_url : string -> string
(** Build RFC 9728-compliant resource metadata URL *)

val require_auth_middleware :
  handler:Route.handler ->
  required_scopes:string list ->
  resource_metadata_url:string option ->
  Route.handler
(** Require authentication middleware wrapper *)

val create_base_app :
  routes:Route.t list ->
  middleware:Middleware.t list ->
  ?debug:bool ->
  ?lifespan:App.lifespan ->
  unit ->
  App.t
(** Create a base application with common middleware and routes *)

val create_sse_app :
  server:Yojson.Safe.t ->
  message_path:string ->
  sse_path:string ->
  ?auth:Auth_config.t ->
  ?debug:bool ->
  ?routes:Route.t list ->
  ?middleware:Middleware.t list ->
  unit ->
  App.t
(** Create an SSE (Server-Sent Events) application *)

val create_streamable_http_app :
  server:Yojson.Safe.t ->
  streamable_http_path:string ->
  ?_event_store:Yojson.Safe.t ->
  ?auth:Auth_config.t ->
  ?json_response:bool ->
  ?stateless_http:bool ->
  ?debug:bool ->
  ?routes:Route.t list ->
  ?middleware:Middleware.t list ->
  unit ->
  App.t
(** Create a Streamable HTTP application *)
