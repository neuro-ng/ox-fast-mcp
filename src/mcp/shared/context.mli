(** Context Module

    This module provides the request context type used throughout the MCP
    framework. It carries session, request, and lifespan context information. *)

open Mcp.Types

type ('session, 'lifespan_context, 'request) t = {
  request_id : request_id;
  meta : Mcp.Types.meta option;
  session : 'session;
  lifespan_context : 'lifespan_context;
  request : 'request option;
}
[@@deriving sexp, yojson]
(** A generic request context carrying session and lifespan information.

    Type parameters:
    - 'session: The type of the session (must be a subtype of Base_session)
    - 'lifespan_context: The type of the lifespan context
    - 'request: The type of the request *)

val create :
  request_id:request_id ->
  ?meta:Mcp.Types.meta ->
  session:'session ->
  lifespan_context:'lifespan_context ->
  ?request:'request ->
  unit ->
  ('session, 'lifespan_context, 'request) t
(** Create a new request context *)
