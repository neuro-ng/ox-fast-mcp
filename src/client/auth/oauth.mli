(** OAuth Client for OxFastMCP *)

open Async

(** {1 Exceptions} *)

exception Client_not_found_error of string
(** Raised when OAuth client credentials are not found on server *)

(** {1 Helper Functions} *)

val check_if_auth_required :
  ?httpx_kwargs:(string * string) list -> string -> bool Deferred.t
(** Check if the MCP endpoint requires authentication by making a test request.

    Returns true if auth appears to be required (401/403 status or
    WWW-Authenticate header), false otherwise. Returns true on connection
    errors.

    @param httpx_kwargs Optional HTTP headers as key-value pairs
    @param mcp_url MCP endpoint URL to test *)

val find_available_port : unit -> int
(** Find an available port by binding to port 0 and getting OS-assigned port.
    Returns 8080 as fallback if port detection fails. *)

val open_browser : string -> unit
(** Open default browser with given URL using platform-specific command. Logs
    the URL being opened.

    @param url URL to open in browser *)

(** {1 Token Storage Adapter} *)

module Token_storage_adapter : sig
  type t
  (** Token storage with in-memory cache and TTL support *)

  val create : server_url:string -> t
  (** Create new token storage adapter for given server URL *)

  val get_token_cache_key : t -> string
  (** Get cache key for OAuth tokens *)

  val get_client_info_cache_key : t -> string
  (** Get cache key for client information *)

  val clear : t -> unit Deferred.t
  (** Clear all cached tokens and client info *)

  val get_tokens : t -> Mcp_shared.Auth.oauth_token option Deferred.t
  (** Retrieve cached OAuth tokens, checking expiry *)

  val set_tokens : t -> Mcp_shared.Auth.oauth_token -> unit Deferred.t
  (** Store OAuth tokens with TTL based on expires_in field *)

  val get_client_info :
    t -> Mcp_shared.Auth.oauth_client_information_full option Deferred.t
  (** Retrieve cached client information *)

  val set_client_info :
    t -> Mcp_shared.Auth.oauth_client_information_full -> unit Deferred.t
  (** Store client information with TTL based on secret expiry *)
end

(** {1 OAuth Client} *)

type t
(** OAuth client for MCP server authentication *)

val create :
  mcp_url:string ->
  ?scopes:string list ->
  ?client_name:string ->
  ?callback_port:int option ->
  ?additional_client_metadata:(string * Yojson.Safe.t) list ->
  unit ->
  t
(** Create new OAuth client for MCP server.

    @param mcp_url Full URL to MCP endpoint (e.g. "http://host/mcp/sse/")
    @param scopes OAuth scopes to request as list of strings
    @param client_name
      Name for this client during registration (default: "OxFastMCP Client")
    @param callback_port
      Fixed port for OAuth callback (default: random available port)
    @param additional_client_metadata Extra fields for OAuth client metadata *)

val initialize : t -> unit Deferred.t
(** Load stored tokens and client info from cache. Safe to call multiple times -
    only initializes once. *)

val redirect_handler : t -> string -> unit Deferred.t
(** Handle OAuth redirect by opening browser with authorization URL.

    Performs pre-flight check to detect invalid client_id before opening
    browser. Raises Client_not_found_error if client credentials are
    stale/invalid.

    @param authorization_url Full OAuth authorization URL *)

val callback_handler : t -> (string * string option) Deferred.t
(** Handle OAuth callback and return (auth_code, state).

    NOTE: This is currently stubbed - full implementation requires HTTP server.
    See oauth.todo for implementation requirements.

    @raise Failure Always raises - not implemented yet *)

val clear_cache : t -> unit Deferred.t
(** Clear all cached OAuth credentials for this client *)
