(** OCI OIDC provider for OxFastMCP.

    This module provides OIDC Implementation to integrate MCP servers with OCI
    (Oracle Cloud Infrastructure). *)

(** Provider settings configuration. *)
module Settings : sig
  type t = {
    config_url : string option;
    client_id : string option;
    client_secret : string option;
    audience : string option;
    base_url : string option;
    issuer_url : string option;
    redirect_path : string option;
    required_scopes : string list option;
    allowed_client_redirect_uris : string list option;
    jwt_signing_key : string option;
  }
  [@@deriving sexp, yojson, compare]

  val create :
    ?config_url:string ->
    ?client_id:string ->
    ?client_secret:string ->
    ?audience:string ->
    ?base_url:string ->
    ?issuer_url:string ->
    ?redirect_path:string ->
    ?required_scopes:string list ->
    ?allowed_client_redirect_uris:string list ->
    ?jwt_signing_key:string ->
    unit ->
    t

  val load_from_env : unit -> t
  val merge : t -> t -> t
  val validate : t -> (unit, Core.Error.t) result
end

(** An OCI IAM Domain provider implementation for OxFastMCP. *)
module Oci_provider : sig
  type t [@@deriving sexp, compare]

  val create :
    ?config_url:string ->
    ?client_id:string ->
    ?client_secret:string ->
    ?audience:string ->
    ?base_url:string ->
    ?issuer_url:string ->
    ?redirect_path:string ->
    ?required_scopes:string list ->
    ?allowed_client_redirect_uris:string list ->
    ?jwt_signing_key:string ->
    ?_require_authorization_consent:bool ->
    unit ->
    (t, Core.Error.t) result
  (** Create a new OCI provider.

      @param config_url OCI OIDC Discovery URL
      @param client_id OCI IAM Domain Integrated Application client id
      @param client_secret OCI Integrated Application client secret
      @param audience OCI API audience (optional)
      @param base_url
        Public URL where OIDC endpoints will be accessible (includes any mount
        path)
      @param issuer_url Issuer URL for OCI IAM Domain metadata (optional)
      @param redirect_path Redirect path configured in OCI IAM Domain
      @param required_scopes Required OCI scopes (defaults to ["openid"])
      @param allowed_client_redirect_uris
        List of allowed redirect URI patterns for MCP clients
      @param jwt_signing_key JWT signing key (optional)
      @param _require_authorization_consent Whether to require consent *)

  val config_url : t -> string
  val client_id : t -> string
  val client_secret : t -> string
  val audience : t -> string option
  val base_url : t -> string
  val issuer_url : t -> string option
  val redirect_path : t -> string option
  val required_scopes : t -> string list
  val allowed_client_redirect_uris : t -> string list option
  val jwt_signing_key : t -> string option
end
