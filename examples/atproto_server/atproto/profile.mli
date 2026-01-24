(** AT Protocol profile operations interface *)

open! Async
open Atproto_types

val get_profile_info : unit -> Types.profile_info Deferred.t
(** Get current authenticated user's profile information *)

val update_profile :
  Types.profile_update_param -> Types.profile_update_result Deferred.t
(** Update current user's profile (display name, bio, avatar) *)

val get_profile_by_handle : string -> Types.profile_query_result Deferred.t
(** Get detailed profile information by handle *)

val get_profile_by_did : string -> Types.profile_query_result Deferred.t
(** Get detailed profile information by DID *)
