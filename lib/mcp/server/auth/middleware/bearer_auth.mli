open Core
open Async
open Cohttp
open Mcp.Server.Auth.Provider

(** User with authentication info *)
type authenticated_user = {
  client_id: string;
  access_token: access_token;
  scopes: string list;
}

(** Authentication credentials *)
type auth_credentials = {
  scopes: string list;
}

(** Authentication result *)
type auth_result = (auth_credentials * authenticated_user) option

(** Bearer auth backend configuration *)
type bearer_auth_config = {
  token_verifier: (module TOKEN_VERIFIER);
}

(** Bearer auth backend *)
module Bearer_auth_backend : sig
  (** Create a new bearer auth backend *)
  val create : bearer_auth_config -> bearer_auth_config

  (** Authenticate a request *)
  val authenticate : bearer_auth_config -> Request.t -> auth_result Lwt.t
end

(** Required auth middleware configuration *)
type require_auth_config = {
  required_scopes: string list;
  resource_metadata_url: string option;
}

(** Required auth middleware *)
module Require_auth_middleware : sig
  (** Create a new require auth middleware *)
  val create : require_auth_config -> require_auth_config

  (** Handle a request *)
  val handle : 
    require_auth_config ->
    auth_result option ->
    Request.t ->
    (Response.t * Body.t) Lwt.t ->
    (Response.t * Body.t) Lwt.t
end 