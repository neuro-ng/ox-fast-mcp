(** Descope authentication provider for OxFastMCP.

    This module provides integration with Descope's OAuth 2.1 and OpenID Connect
    services, supporting Dynamic Client Registration (DCR) for seamless MCP
    client authentication. *)

(** Provider settings configuration. *)
module Settings : sig
  type t = {
    config_url : string option;
    project_id : string option;
    descope_base_url : string option;
    base_url : string option;
    required_scopes : string list option;
  }
  [@@deriving sexp, yojson, compare]

  val create :
    ?config_url:string ->
    ?project_id:string ->
    ?descope_base_url:string ->
    ?base_url:string ->
    ?required_scopes:string list ->
    unit ->
    t

  val load_from_env : unit -> t
  val merge : t -> t -> t
  val validate : t -> (unit, Core.Error.t) result
  val validate_base_url : t -> (unit, Core.Error.t) result
end

(** Descope OAuth provider for DCR (Dynamic Client Registration). *)
module Descope_provider : sig
  type t [@@deriving sexp, compare]

  val create :
    ?config_url:string ->
    ?project_id:string ->
    ?descope_base_url:string ->
    ?base_url:string ->
    ?required_scopes:string list ->
    ?_token_verifier:unit ->
    unit ->
    (t, Core.Error.t) result
  (** Create a new Descope provider.

      @param config_url
        Your Descope Well-Known URL (new API). If provided, project_id and
        descope_base_url are extracted automatically.
      @param project_id Your Descope Project ID (old API, backwards compat)
      @param descope_base_url Your Descope base URL (old API, backwards compat)
      @param base_url Public URL of this OxFastMCP server
      @param required_scopes
        Optional list of scopes that must be present in validated tokens
      @param _token_verifier Optional custom token verifier (placeholder) *)

  val project_id : t -> string
  (** Get the project ID *)

  val descope_base_url : t -> string
  (** Get the Descope base URL *)

  val base_url : t -> string
  (** Get the base URL *)

  val issuer_url : t -> string
  (** Get the issuer URL *)

  val jwks_uri : t -> string
  (** Get the JWKS URI *)

  val required_scopes : t -> string list
  (** Get required scopes *)

  val oauth_metadata_url : t -> string
  (** Get OAuth authorization server metadata URL *)
end
