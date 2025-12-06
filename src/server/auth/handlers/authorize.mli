(** Enhanced authorization handler with improved error responses.

    This module provides an enhanced authorization handler that wraps the MCP
    SDK's AuthorizationHandler to provide better error messages when clients
    attempt to authorize with unregistered client IDs.

    The enhancement adds:
    - Content negotiation: HTML for browsers, JSON for API clients
    - Enhanced JSON responses with registration endpoint hints
    - Styled HTML error pages with registration links/forms
    - Link headers pointing to registration endpoints *)

open! Core

type authorization_error_response = {
  error : string;
  error_description : string option;
  state : string option;
  registration_endpoint : string option;
  authorization_server_metadata : string option;
}
[@@deriving yojson, sexp, compare]
(** Authorization error response type *)

type config = {
  base_url : string;
  server_name : string option;
  server_icon_url : string option;
}
(** Configuration for the authorization handler *)

val create_error_response :
  error:string ->
  ?error_description:string ->
  ?state:string ->
  ?registration_endpoint:string ->
  ?authorization_server_metadata:string ->
  unit ->
  authorization_error_response
(** Create an authorization error response *)

val create_unregistered_client_html :
  client_id:string ->
  registration_endpoint:string ->
  discovery_endpoint:string ->
  ?server_name:string ->
  ?server_icon_url:string ->
  ?title:string ->
  unit ->
  string
(** Create styled HTML error page for unregistered client attempts.

    @param client_id The unregistered client ID that was provided
    @param registration_endpoint URL of the registration endpoint
    @param discovery_endpoint URL of the OAuth metadata discovery endpoint
    @param server_name Optional server name for branding
    @param server_icon_url Optional server icon URL
    @param title Page title
    @return HTML string for the error page *)

(** Authorization handler module type *)
module type AUTHORIZATION_HANDLER = sig
  type request
  type response

  val handle : request -> response Lwt.t
  (** Handle authorization request with enhanced error responses *)
end

(** Create the enhanced authorization handler

    @param config Handler configuration with base_url and optional branding
    @return An authorization handler module *)
module Make_authorization_handler
    (Provider : Mcp_server_auth.Provider.OAUTH_AUTHORIZATION_SERVER_PROVIDER) : sig
  val is_client_registered : string -> bool Lwt.t
  (** Check if a client is registered *)

  val create_enhanced_error_response :
    client_id:string ->
    accept_header:string ->
    state:string option ->
    config:config ->
    int * string * string * (string * string) list
  (** Create an enhanced error response for unregistered client

      @param client_id The unregistered client ID
      @param accept_header The Accept header from the request
      @param state The state parameter from the request
      @param config Handler configuration
      @return Tuple of (status_code, content_type, body, headers) *)
end
