(** Context Module

    This module provides the request context type used throughout the MCP
    framework. It carries session, request, and lifespan context information. *)

open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives
module Types = Mcp.Types

type ('session, 'lifespan_context, 'request) t = {
  request_id : Mcp.Types.request_id;
  meta : Mcp.Types.meta option;
  session : 'session;
  lifespan_context : 'lifespan_context;
  request : 'request option;
}
[@@deriving yojson]
(** A generic request context carrying session and lifespan information *)

(** Create a new request context *)
let create ~request_id ?meta ~session ~lifespan_context ?request () =
  { request_id; meta; session; lifespan_context; request }
