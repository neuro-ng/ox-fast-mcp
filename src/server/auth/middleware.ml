(** Enhanced authentication middleware with better error messages.

    This module provides enhanced versions of MCP SDK authentication middleware
    that return more helpful error messages for developers troubleshooting
    authentication issues. *)

open! Core
open Cohttp
open! Logging

(** Initialize logger for Auth Middleware *)
let logger = Logger.get_logger "AuthMiddleware"

(** Enhanced error descriptions for specific error types *)
let enhance_description ~status_code ~error ~description =
  match (error, status_code) with
  | "invalid_token", 401 ->
    (* This is the "Authentication required" error *)
    "Authentication failed. The provided bearer token is invalid, expired, or \
     no longer recognized by the server. To resolve: clear authentication \
     tokens in your MCP client and reconnect. Your client should \
     automatically re-register and obtain new tokens."
  | "insufficient_scope", _ ->
    (* Scope error - already has good detail from SDK *)
    description
  | _ -> description

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
module Require_auth_middleware = struct
  (** Create a new require auth middleware configuration *)
  let create config = config

  (** Send an authentication error response with enhanced error messages.

      Overrides the SDK's _send_auth_error to provide more detailed error
      descriptions that help developers troubleshoot authentication issues.

      @param status HTTP status code (401 or 403)
      @param error OAuth error code
      @param description Base error description
      @param resource_metadata_url Optional resource metadata URL *)
  let send_auth_error ~status ~error ~description ~resource_metadata_url =
    let status_code = Cohttp.Code.code_of_status status in
    let enhanced_description =
      enhance_description ~status_code ~error ~description
    in
    (* Build WWW-Authenticate header value *)
    let www_auth_parts =
      [
        Printf.sprintf "error=\"%s\"" error;
        Printf.sprintf "error_description=\"%s\"" enhanced_description;
      ]
    in
    let www_auth_parts =
      match resource_metadata_url with
      | None -> www_auth_parts
      | Some url ->
        www_auth_parts @ [ Printf.sprintf "resource_metadata=\"%s\"" url ]
    in
    let www_authenticate =
      Printf.sprintf "Bearer %s" (String.concat ~sep:", " www_auth_parts)
    in
    (* Build response body *)
    let body =
      `Assoc
        [
          ("error", `String error);
          ("error_description", `String enhanced_description);
        ]
    in
    let body_string = Yojson.Safe.to_string body in
    (* Log the auth error *)
    let () =
      Logger.info logger
        (Printf.sprintf "Auth error returned: %s (status=%d)" error status_code)
    in
    (* Build and return response *)
    let headers =
      Header.of_list
        [
          ("Content-Type", "application/json");
          ("Content-Length", string_of_int (String.length body_string));
          ("WWW-Authenticate", www_authenticate);
        ]
    in
    let response = Response.make ~status ~headers () in
    Lwt.return (response, Body.of_string body_string)

  (** Handle a request with authentication requirements.

      @param config The middleware configuration
      @param auth_result_opt The authentication result from upstream middleware
      @param req The incoming HTTP request
      @param next The next handler in the chain
      @return HTTP response and body *)
  let handle config
      (auth_result_opt :
        Mcp_server_auth_middleware.Bearer_auth.auth_result option)
      (_req : Request.t) (next : (Response.t * Body.t) Lwt.t) =
    match auth_result_opt with
    | None | Some None ->
      send_auth_error ~status:`Unauthorized ~error:"invalid_token"
        ~description:"Authentication required"
        ~resource_metadata_url:config.resource_metadata_url
    | Some (Some (auth_creds, _auth_user)) -> (
      let missing_scope =
        List.find config.required_scopes ~f:(fun required_scope ->
            not
              (List.mem auth_creds.scopes required_scope ~equal:String.equal))
      in
      match missing_scope with
      | Some scope ->
        send_auth_error ~status:`Forbidden ~error:"insufficient_scope"
          ~description:(Printf.sprintf "Required scope: %s" scope)
          ~resource_metadata_url:config.resource_metadata_url
      | None -> next)
end
