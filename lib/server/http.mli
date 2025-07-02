open Core
open Cohttp
open Lwt

(** Current HTTP request context *)
val current_http_request : Request.t option Lwt.key

(** Set the current HTTP request in context *)
val with_http_request : Request.t -> (unit -> 'a Lwt.t) -> 'a Lwt.t

(** Setup auth middleware and routes *)
val setup_auth_middleware_and_routes :
  auth:Mcp.Server.Auth.Provider.oauth_provider ->
  middleware_list * route_list * string list

(** Create a base application with common middleware and routes *)
val create_base_app :
  routes:route_list ->
  middleware:middleware_list ->
  ?debug:bool ->
  ?lifespan:(unit -> unit Lwt.t) ->
  app

(** Create an SSE application *)
val create_sse_app :
  server:Mcp.Server.server ->
  message_path:string ->
  sse_path:string ->
  ?auth:Mcp.Server.Auth.Provider.oauth_provider ->
  ?debug:bool ->
  ?routes:route_list ->
  ?middleware:middleware_list ->
  unit ->
  app

(** Create a streamable HTTP application *)
val create_streamable_http_app :
  server:Mcp.Server.server ->
  streamable_http_path:string ->
  ?event_store:Mcp.Server.event_store ->
  ?auth:Mcp.Server.Auth.Provider.oauth_provider ->
  ?json_response:bool ->
  ?stateless_http:bool ->
  ?debug:bool ->
  ?routes:route_list ->
  ?middleware:middleware_list ->
  unit ->
  app

(** Application type *)
and app = {
  routes: route_list;
  middleware: middleware_list;
  debug: bool;
  lifespan: (unit -> unit Lwt.t) option;
}

(** Route type *)
and route = {
  path: string;
  methods: string list;
  handler: (Request.t -> (Response.t * Body.t) Lwt.t);
}

and route_list = route list

(** Middleware type *)
and middleware = {
  name: string;
  handler: (Request.t -> (Response.t * Body.t) Lwt.t) -> (Request.t -> (Response.t * Body.t) Lwt.t);
}

and middleware_list = middleware list 