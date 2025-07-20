open Core
open Async
open Cohttp
open Mcp.Server.Auth.Provider

type authenticated_user = {
  client_id : string;
  access_token : access_token;
  scopes : string list;
}
(** User with authentication info *)

type auth_credentials = { scopes : string list }
(** Authentication credentials *)

type auth_result = (auth_credentials * authenticated_user) option
(** Authentication result *)

type bearer_auth_config = { token_verifier : (module TOKEN_VERIFIER) }
(** Bearer auth backend configuration *)

(** Bearer auth backend *)
module Bearer_auth_backend : sig
  val create : bearer_auth_config -> bearer_auth_config
  (** Create a new bearer auth backend *)

  val authenticate : bearer_auth_config -> Request.t -> auth_result Lwt.t
  (** Authenticate a request *)
end

type require_auth_config = {
  required_scopes : string list;
  resource_metadata_url : string option;
}
(** Required auth middleware configuration *)

(** Required auth middleware *)
module Require_auth_middleware : sig
  val create : require_auth_config -> require_auth_config
  (** Create a new require auth middleware *)

  val handle :
    require_auth_config ->
    auth_result option ->
    Request.t ->
    (Response.t * Body.t) Lwt.t ->
    (Response.t * Body.t) Lwt.t
  (** Handle a request *)
end
