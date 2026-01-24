open Core

module Settings : sig
  type t = {
    client_id : string option;
    client_secret : string option;
    tenant_id : string option;
    identifier_uri : string option;
    base_url : string option;
    issuer_url : string option;
    redirect_path : string option;
    required_scopes : string list option;
    additional_authorize_scopes : string list option;
    allowed_client_redirect_uris : string list option;
    jwt_signing_key : string option;
    base_authority : string option;
  }
  [@@deriving sexp, yojson, compare]

  val create :
    ?client_id:string ->
    ?client_secret:string ->
    ?tenant_id:string ->
    ?identifier_uri:string ->
    ?base_url:string ->
    ?issuer_url:string ->
    ?redirect_path:string ->
    ?required_scopes:string list ->
    ?additional_authorize_scopes:string list ->
    ?allowed_client_redirect_uris:string list ->
    ?jwt_signing_key:string ->
    ?base_authority:string ->
    unit ->
    t

  val load_from_env : unit -> t
  val merge : t -> t -> t
  val validate : t -> unit Or_error.t
end

module Azure_provider : sig
  type t = {
    settings : Settings.t;
    identifier_uri : string;
    authorization_endpoint : string;
    token_endpoint : string;
    issuer : string;
    jwks_uri : string;
  }

  val prefix_scopes_for_azure : string -> string list -> string list

  val create :
    ?client_id:string ->
    ?client_secret:string ->
    ?tenant_id:string ->
    ?identifier_uri:string ->
    ?base_url:string ->
    ?issuer_url:string ->
    ?redirect_path:string ->
    ?required_scopes:string list ->
    ?additional_authorize_scopes:string list ->
    ?allowed_client_redirect_uris:string list ->
    ?_client_storage:unit ->
    ?jwt_signing_key:string ->
    ?require_authorization_consent:bool ->
    ?base_authority:string ->
    unit ->
    (t, Error.t) Result.t
end
