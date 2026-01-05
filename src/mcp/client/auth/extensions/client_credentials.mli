(** OAuth2 Client Credentials Extension - RFC 7523 *)

open Core
open Async

(** {1 JWT Parameters} *)

type jwt_parameters = {
  assertion : string option;
  issuer : string option;
  subject : string option;
  audience : string option;
  claims : (string * Yojson.Safe.t) list option;
  jwt_signing_algorithm : string option;
  jwt_signing_key : string option;
  jwt_lifetime_seconds : int;
}

val default_jwt_parameters : jwt_parameters

val create_jwt_parameters :
  ?assertion:string ->
  ?issuer:string ->
  ?subject:string ->
  ?audience:string ->
  ?claims:(string * Yojson.Safe.t) list ->
  ?jwt_signing_algorithm:string ->
  ?jwt_signing_key:string ->
  ?jwt_lifetime_seconds:int ->
  unit ->
  jwt_parameters

val to_assertion :
  jwt_parameters ->
  with_audience_fallback:string ->
  (string, Error.t) Result.t Deferred.t

(** {1 RFC 7523 OAuth Client Provider} *)

type t

val create :
  server_url:string ->
  client_metadata:Mcp_shared.Auth.oauth_client_metadata ->
  storage:Mcp_client_auth.Oauth2.storage_wrapper ->
  ?redirect_handler:(string -> unit Deferred.t) option ->
  ?callback_handler:(unit -> (string * string option) Deferred.t) option ->
  ?timeout:float ->
  ?jwt_parameters:jwt_parameters ->
  unit ->
  t

val add_client_authentication_jwt :
  t ->
  token_data:(string * string) list ->
  ((string * string) list, Error.t) Result.t Deferred.t

val exchange_token_jwt_bearer : t -> (unit, Error.t) Result.t Deferred.t
val initialize : t -> unit Deferred.t
val add_auth_header : t -> (string * string) list -> (string * string) list
