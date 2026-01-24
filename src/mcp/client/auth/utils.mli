(** OAuth Client Utility Functions *)

open Async

(** {1 Constants} *)

val mcp_protocol_version_header : string
(** Header name for MCP protocol version: "MCP-Protocol-Version" *)

(** {1 WWW-Authenticate Header Parsing} *)

val extract_field_from_www_auth : Cohttp.Response.t -> string -> string option
(** Extract a named field from the WWW-Authenticate header using regex.

    [extract_field_from_www_auth response field_name] searches for the field in
    the WWW-Authenticate header and returns its value if found.

    Supports both quoted and unquoted field values:
    - [field="value"]
    - [field=value]

    @param response HTTP response containing WWW-Authenticate header
    @param field_name Name of the field to extract
    @return Field value if found, None otherwise *)

val extract_scope_from_www_auth : Cohttp.Response.t -> string option
(** Extract scope parameter from WWW-Authenticate header per RFC6750.

    @param response HTTP response with WWW-Authenticate header
    @return Scope string if found, None otherwise *)

val extract_resource_metadata_from_www_auth : Cohttp.Response.t -> string option
(** Extract protected resource metadata URL from WWW-Authenticate header per
    RFC9728.

    Only extracts if response status is 401.

    @param response HTTP response (must be 401)
    @return Resource metadata URL if found, None otherwise *)

(** {1 URL Building for Discovery} *)

val build_protected_resource_metadata_discovery_urls :
  string option -> string -> string list
(** Build ordered list of URLs for protected resource metadata discovery
    (SEP-985).

    Discovery priority:
    1. WWW-Authenticate header [resource_metadata] URL (if present)
    2. Path-based well-known: [/.well-known/oauth-protected-resource{path}]
    3. Root-based well-known: [/.well-known/oauth-protected-resource]

    @param www_auth_url Optional resource_metadata URL from WWW-Authenticate
    @param server_url Server URL
    @return Ordered list of URLs to try for discovery *)

val build_oauth_authorization_server_metadata_discovery_urls :
  string option -> string -> string list
(** Build ordered list of URLs for OAuth authorization server metadata
    discovery.

    Discovery paths:
    - Legacy (no auth_server_url): [/.well-known/oauth-authorization-server]
      only
    - With path: OAuth path-aware, OIDC path-aware, OIDC suffix paths
    - Root: OAuth root, OIDC root

    @param auth_server_url Optional OAuth authorization server URL
    @param server_url MCP server URL (used as fallback)
    @return Ordered list of URLs to try for discovery *)

(** {1 Scope Selection} *)

val get_client_metadata_scopes :
  www_authenticate_scope:string option ->
  protected_resource_metadata:Mcp_shared.Auth.protected_resource_metadata option ->
  ?authorization_server_metadata:Mcp_shared.Auth.oauth_metadata option ->
  unit ->
  string option
(** Select scopes per MCP spec priority order.

    Priority:
    1. WWW-Authenticate header scope (highest)
    2. PRM scopes_supported (join with space)
    3. OASM scopes_supported (join with space)
    4. None (omit parameter)

    @param www_authenticate_scope Scope from WWW-Authenticate header
    @param protected_resource_metadata Protected resource metadata
    @param authorization_server_metadata Optional OAuth server metadata
    @return Selected scope string or None *)

(** {1 CIMD (Client ID Metadata Document) Support} *)

val is_valid_client_metadata_url : string option -> bool
(** Validate that a URL is suitable for use as client_id (CIMD).

    Requirements:
    - URL must exist (not None)
    - Scheme must be "https"
    - Path must be non-empty and not just "/"

    @param url URL to validate
    @return true if valid for CIMD, false otherwise *)

val should_use_client_metadata_url :
  oauth_metadata:Mcp_shared.Auth.oauth_metadata option ->
  client_metadata_url:string option ->
  bool
(** Determine if CIMD should be used instead of DCR.

    Conditions:
    - Both parameters must be Some
    - Server must advertise [client_id_metadata_document_supported = true]

    @param oauth_metadata OAuth authorization server metadata
    @param client_metadata_url URL-based client ID
    @return true if CIMD should be used, false otherwise *)

val create_client_info_from_metadata_url :
  client_metadata_url:string ->
  ?redirect_uris:Uri.t list ->
  unit ->
  Mcp_shared.Auth.oauth_client_information_full
(** Create client information using URL-based client ID (CIMD).

    When using URL-based client IDs:
    - URL becomes the client_id
    - No client_secret is used ([token_endpoint_auth_method="none"])

    @param client_metadata_url URL to use as client_id
    @param redirect_uris Optional redirect URIs
    @return Full OAuth client information *)

(** {1 Async Response Handlers} *)

val handle_protected_resource_response :
  Cohttp.Response.t ->
  Cohttp_async.Body.t ->
  Mcp_shared.Auth.protected_resource_metadata option Deferred.t
(** Handle protected resource metadata discovery response.

    Parses and validates PRM discovery response per SEP-985.

    @param response HTTP response
    @param body Response body
    @return Some metadata on success, None on failure *)

val handle_auth_metadata_response :
  Cohttp.Response.t ->
  Cohttp_async.Body.t ->
  (bool * Mcp_shared.Auth.oauth_metadata option) Deferred.t
(** Handle OAuth authorization server metadata discovery response.

    Return values:
    - [(true, Some metadata)] - Success
    - [(true, None)] - Invalid JSON, try next URL
    - [(false, None)] - Server error (non-4XX), stop trying

    @param response HTTP response
    @param body Response body
    @return (should_continue, optional_metadata) *)

val handle_registration_response :
  Cohttp.Response.t ->
  Cohttp_async.Body.t ->
  Mcp_shared.Auth.oauth_client_information_full Deferred.t
(** Handle client registration response.

    @param response HTTP response (must be 200 or 201)
    @param body Response body
    @return Client information
    @raise Exceptions.Oauth_registration_error
      if status not 200/201 or invalid JSON *)

val handle_token_response_scopes :
  Cohttp.Response.t ->
  Cohttp_async.Body.t ->
  Mcp_shared.Auth.oauth_token Deferred.t
(** Parse and validate token response.

    Note: Caller should check status_code before calling this function.

    @param response HTTP response
    @param body Response body
    @return OAuth token
    @raise Exceptions.Oauth_token_error if JSON is invalid *)

(** {1 Request Builders} *)

val create_oauth_metadata_request : string -> Cohttp.Request.t
(** Build GET request for metadata discovery.

    Adds MCP-Protocol-Version header.

    @param url Discovery URL
    @return HTTP GET request *)

val create_client_registration_request :
  auth_server_metadata:Mcp_shared.Auth.oauth_metadata option ->
  client_metadata:Mcp_shared.Auth.oauth_client_metadata ->
  auth_base_url:string ->
  Cohttp.Request.t
(** Build POST request for client registration.

    Uses [registration_endpoint] from metadata if available, otherwise falls
    back to [/register] on auth_base_url.

    @param auth_server_metadata Optional OAuth server metadata
    @param client_metadata Client metadata to register
    @param auth_base_url Base URL for auth server
    @return HTTP POST request with JSON body *)
