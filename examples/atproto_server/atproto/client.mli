(** AT Protocol HTTP client interface *)

open! Core
open! Async
open Atproto_types

type session = {
  did : string;  (** Decentralized identifier *)
  access_jwt : string;  (** Access token *)
  refresh_jwt : string;  (** Refresh token *)
  handle : string;  (** User handle *)
}
[@@deriving sexp]
(** AT Protocol session information *)

type t
(** Client state *)

val create : base_url:string -> t
(** Create a new client with the given base URL *)

val login : t -> handle:string -> password:string -> session Deferred.t
(** Authenticate and create a session *)

val switch_account : t -> handle:string -> bool Deferred.t
(** Switch the active account by handle. Returns true if successful. *)

val list_accounts : t -> string list
(** List all authenticated account handles *)

val api_get : t -> endpoint:string -> string Deferred.t
(** Make an authenticated GET request to an AT Protocol endpoint *)

val api_post : t -> endpoint:string -> body:Yojson.Safe.t -> string Deferred.t
(** Make an authenticated POST request to an AT Protocol endpoint *)

val api_post_blob :
  t ->
  endpoint:string ->
  content_type:string ->
  body:string ->
  Yojson.Safe.t Deferred.t
(** Make an authenticated POST request with binary blob data *)

val get_client : Settings.t -> t Deferred.t
(** Get or create the authenticated singleton client instance *)

val get_session : t -> session option
(** Get current session (for testing/debugging) *)

(** Convenience wrappers using singleton client *)

val authenticated_get : endpoint:string -> string Deferred.t

val authenticated_post :
  endpoint:string -> body:Yojson.Safe.t -> string Deferred.t

val authenticated_post_blob :
  endpoint:string ->
  content_type:string ->
  body:string ->
  Yojson.Safe.t Deferred.t
