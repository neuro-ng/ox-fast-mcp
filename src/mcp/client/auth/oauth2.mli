(** OAuth2 Authentication for MCP Client *)

open Async

(** {1 PKCE Parameters} *)

module Pkce_parameters : sig
  type t = { code_verifier : string; code_challenge : string }

  val generate : unit -> t
  (** Generate new PKCE parameters with 128-character verifier and SHA256
      challenge *)
end

(** {1 Token Storage} *)

module type Token_storage = sig
  type t

  val get_tokens : t -> Mcp_shared.Auth.oauth_token option Deferred.t
  val set_tokens : t -> Mcp_shared.Auth.oauth_token -> unit Deferred.t

  val get_client_info :
    t -> Mcp_shared.Auth.oauth_client_information_full option Deferred.t

  val set_client_info :
    t -> Mcp_shared.Auth.oauth_client_information_full -> unit Deferred.t
end

(** {1 OAuth Context} *)

type storage_wrapper =
  | Storage : (module Token_storage with type t = 'a) * 'a -> storage_wrapper

type oauth_context = private {
  server_url : string;
  client_metadata : Mcp_shared.Auth.oauth_client_metadata;
  storage : storage_wrapper;
  redirect_handler : (string -> unit Deferred.t) option;
  callback_handler : (unit -> (string * string option) Deferred.t) option;
  timeout : float;
  client_metadata_url : string option;
  mutable protected_resource_metadata :
    Mcp_shared.Auth.protected_resource_metadata option;
  mutable oauth_metadata : Mcp_shared.Auth.oauth_metadata option;
  mutable auth_server_url : string option;
  mutable protocol_version : string option;
  mutable client_info : Mcp_shared.Auth.oauth_client_information_full option;
  mutable current_tokens : Mcp_shared.Auth.oauth_token option;
  mutable token_expiry_time : float option;
  lock : unit Ivar.t;
}

val get_authorization_base_url : string -> string
val calculate_token_expiry : int option -> float option
val update_token_expiry : oauth_context -> Mcp_shared.Auth.oauth_token -> unit
val is_token_valid : oauth_context -> bool
val can_refresh_token : oauth_context -> bool
val clear_tokens : oauth_context -> unit
val get_resource_url : oauth_context -> string
val should_include_resource_param : oauth_context -> string option -> bool

val prepare_token_auth :
  oauth_context ->
  (string * string) list ->
  (string * string) list option ->
  (string * string) list * (string * string) list

(** {1 OAuth Client Provider} *)

type t

val create :
  server_url:string ->
  client_metadata:Mcp_shared.Auth.oauth_client_metadata ->
  storage:storage_wrapper ->
  ?redirect_handler:(string -> unit Deferred.t) option ->
  ?callback_handler:(unit -> (string * string option) Deferred.t) option ->
  ?timeout:float ->
  ?client_metadata_url:string option ->
  unit ->
  t

val initialize : t -> unit Deferred.t
val add_auth_header : t -> (string * string) list -> (string * string) list
val get_token_endpoint : oauth_context -> string

val get_context : t -> oauth_context
(** Get the OAuth context from the provider *)

(** {1 OAuth Flow Helper Functions} *)

val discover_protected_resource_metadata :
  string option ->
  string ->
  Mcp_shared.Auth.protected_resource_metadata option Deferred.t
(** Discover protected resource metadata with fallback URLs *)

val discover_oauth_authorization_server_metadata :
  string option -> string -> Mcp_shared.Auth.oauth_metadata option Deferred.t
(** Discover OAuth authorization server metadata with fallback URLs *)

val register_client :
  oauth_context ->
  Mcp_shared.Auth.oauth_client_information_full option Deferred.t
(** Register client with authorization server or use CIMD *)

val perform_authorization_code_grant :
  oauth_context -> (string * string, string) Result.t Deferred.t
(** Perform authorization code grant with PKCE, returns (auth_code,
    code_verifier) *)

val exchange_authorization_code :
  oauth_context ->
  string ->
  string ->
  (Mcp_shared.Auth.oauth_token, string) Result.t Deferred.t
(** Exchange authorization code for access token *)

val refresh_access_token :
  oauth_context -> (Mcp_shared.Auth.oauth_token, string) Result.t Deferred.t
(** Refresh access token using refresh token *)

val stub_async_auth_flow : t -> 'request -> 'response
(** NOTE: Full async auth flow is stubbed - see oauth2.todo *)
