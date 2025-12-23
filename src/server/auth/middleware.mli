(** Enhanced authentication middleware with better error messages.

    This module provides enhanced versions of MCP SDK authentication middleware
    that return more helpful error messages for developers troubleshooting
    authentication issues. *)

open Cohttp

(** Configuration for require auth middleware *)
type require_auth_config = {
  required_scopes : string list;
  resource_metadata_url : string option;
}
[@@deriving compare, sexp]

(** Require Auth Middleware with enhanced error messages.

    Extends the SDK's RequireAuthMiddleware to provide more actionable error
    messages when authentication fails. This helps developers understand what
    went wrong and how to fix it. *)
module Require_auth_middleware : sig
  val create : require_auth_config -> require_auth_config
  (** Create a new require auth middleware configuration *)

  val send_auth_error :
    status:Code.status_code ->
    error:string ->
    description:string ->
    resource_metadata_url:string option ->
    (Response.t * Body.t) Lwt.t
  (** Send an authentication error response with enhanced error messages.

      Overrides the SDK's _send_auth_error to provide more detailed error
      descriptions that help developers troubleshoot authentication issues.

      @param status HTTP status code (401 or 403)
      @param error OAuth error code
      @param description Base error description
      @param resource_metadata_url Optional resource metadata URL *)

  val handle :
    require_auth_config ->
    Mcp_server_auth_middleware.Bearer_auth.auth_result option ->
    Request.t ->
    (Response.t * Body.t) Lwt.t ->
    (Response.t * Body.t) Lwt.t
  (** Handle a request with authentication requirements.

      @param config The middleware configuration
      @param auth_result_opt The authentication result from upstream middleware
      @param req The incoming HTTP request
      @param next The next handler in the chain
      @return HTTP response and body *)
end
