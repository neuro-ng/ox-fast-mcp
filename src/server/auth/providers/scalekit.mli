(** Scalekit authentication provider for OxFastMCP.

    This module provides ScalekitProvider - a complete authentication solution
    that integrates with Scalekit's OAuth 2.1 and OpenID Connect services. *)

(** Provider settings configuration. *)
module Settings : sig
  type t = {
    environment_url : string option;
    resource_id : string option;
    base_url : string option;
    mcp_url : string option;
    required_scopes : string list option;
  }
  [@@deriving sexp, yojson, compare]

  val create :
    ?environment_url:string ->
    ?resource_id:string ->
    ?base_url:string ->
    ?mcp_url:string ->
    ?required_scopes:string list ->
    unit ->
    t

  val load_from_env : unit -> t
  val merge : t -> t -> t
  val validate : t -> (unit, Core.Error.t) result
  val resolve_base_url : t -> (string, Core.Error.t) result
end

(** Scalekit resource server provider for OAuth 2.1 authentication. *)
module Scalekit_provider : sig
  type t [@@deriving sexp, compare]

  val create :
    ?environment_url:string ->
    ?resource_id:string ->
    ?base_url:string ->
    ?mcp_url:string ->
    ?required_scopes:string list ->
    ?_client_id:string ->
    unit ->
    (t, Core.Error.t) result
  (** Create a new Scalekit provider.

      @param environment_url Your Scalekit environment URL
      @param resource_id Your Scalekit resource ID
      @param base_url Public URL of this OxFastMCP server
      @param mcp_url Deprecated, use base_url instead
      @param required_scopes
        Optional list of scopes that must be present in tokens
      @param _client_id Deprecated, ignored for backward compatibility *)

  val environment_url : t -> string
  val resource_id : t -> string
  val base_url : t -> string
  val required_scopes : t -> string list
  val jwks_uri : t -> string
  val issuer : t -> string
  val authorization_servers : t -> string list
  val authorization_server : t -> string
  val metadata_url : t -> string
end
