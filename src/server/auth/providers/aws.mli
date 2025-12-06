open Core

module Settings : sig
  type t = {
    user_pool_id : string option;
    aws_region : string option;
    client_id : string option;
    client_secret : string option;
    base_url : string option;
    issuer_url : string option;
    redirect_path : string option;
    required_scopes : string list option;
    allowed_client_redirect_uris : string list option;
    jwt_signing_key : string option;
  }
  [@@deriving sexp, yojson, compare]

  val create :
    ?user_pool_id:string ->
    ?aws_region:string ->
    ?client_id:string ->
    ?client_secret:string ->
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
  val validate : t -> unit Or_error.t
end

module Aws_cognito_provider : sig
  type t = { settings : Settings.t; config_url : string }

  val create :
    ?user_pool_id:string ->
    ?aws_region:string ->
    ?client_id:string ->
    ?client_secret:string ->
    ?base_url:string ->
    ?issuer_url:string ->
    ?redirect_path:string ->
    ?required_scopes:string list ->
    ?allowed_client_redirect_uris:string list ->
    ?_client_storage:unit ->
    ?jwt_signing_key:string ->
    ?require_authorization_consent:bool ->
    unit ->
    (t, Error.t) Result.t
end
