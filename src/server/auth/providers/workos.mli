(** WorkOS authentication providers for OxFastMCP.

    This module provides two WorkOS authentication strategies:
    1. WorkOSProvider - OAuth proxy for WorkOS Connect applications
    2. AuthKitProvider - DCR-compliant provider for WorkOS AuthKit *)

(** Settings for WorkOS OAuth provider. *)
module Workos_settings : sig
  type t = {
    client_id : string option;
    client_secret : string option;
    authkit_domain : string option;
    base_url : string option;
    issuer_url : string option;
    redirect_path : string option;
    required_scopes : string list option;
    timeout_seconds : int option;
    allowed_client_redirect_uris : string list option;
    jwt_signing_key : string option;
  }
  [@@deriving sexp, yojson, compare]

  val create :
    ?client_id:string ->
    ?client_secret:string ->
    ?authkit_domain:string ->
    ?base_url:string ->
    ?issuer_url:string ->
    ?redirect_path:string ->
    ?required_scopes:string list ->
    ?timeout_seconds:int ->
    ?allowed_client_redirect_uris:string list ->
    ?jwt_signing_key:string ->
    unit ->
    t

  val load_from_env : unit -> t
  val merge : t -> t -> t
  val validate : t -> (unit, Core.Error.t) result
end

(** Token verifier for WorkOS OAuth tokens. *)
module Workos_token_verifier : sig
  type t [@@deriving sexp, compare]

  val create :
    authkit_domain:string ->
    ?required_scopes:string list ->
    ?timeout_seconds:int ->
    unit ->
    t

  val authkit_domain : t -> string
  val required_scopes : t -> string list
  val timeout_seconds : t -> int
  val userinfo_url : t -> string
end

(** Complete WorkOS OAuth provider for OxFastMCP. *)
module Workos_provider : sig
  type t [@@deriving sexp, compare]

  val create :
    ?client_id:string ->
    ?client_secret:string ->
    ?authkit_domain:string ->
    ?base_url:string ->
    ?issuer_url:string ->
    ?redirect_path:string ->
    ?required_scopes:string list ->
    ?timeout_seconds:int ->
    ?allowed_client_redirect_uris:string list ->
    ?jwt_signing_key:string ->
    unit ->
    (t, Core.Error.t) result

  val client_id : t -> string
  val client_secret : t -> string
  val authkit_domain : t -> string
  val base_url : t -> string option
  val issuer_url : t -> string option
  val redirect_path : t -> string
  val required_scopes : t -> string list
  val timeout_seconds : t -> int
  val upstream_authorization_endpoint : t -> string
  val upstream_token_endpoint : t -> string
  val token_verifier : t -> Workos_token_verifier.t
end

(** Settings for AuthKit provider. *)
module Authkit_settings : sig
  type t = {
    authkit_domain : string option;
    base_url : string option;
    required_scopes : string list option;
  }
  [@@deriving sexp, yojson, compare]

  val create :
    ?authkit_domain:string ->
    ?base_url:string ->
    ?required_scopes:string list ->
    unit ->
    t

  val load_from_env : unit -> t
  val merge : t -> t -> t
  val validate : t -> (unit, Core.Error.t) result
end

(** AuthKit metadata provider for DCR (Dynamic Client Registration). *)
module Authkit_provider : sig
  type t [@@deriving sexp, compare]

  val create :
    ?authkit_domain:string ->
    ?base_url:string ->
    ?required_scopes:string list ->
    unit ->
    (t, Core.Error.t) result

  val authkit_domain : t -> string
  val base_url : t -> string
  val required_scopes : t -> string list
  val jwks_uri : t -> string
  val issuer : t -> string
  val authorization_servers : t -> string list
  val authorization_server : t -> string
  val metadata_url : t -> string
end
