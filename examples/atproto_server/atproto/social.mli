(** AT Protocol social operations interface *)

open! Async
open Atproto_types

val follow_user_by_handle : string -> Types.follow_result Deferred.t
(** Follow a user by their handle *)

val like_post : uri:string -> cid:string -> Types.like_result Deferred.t
(** Like a post by its URI and CID *)

val repost : uri:string -> cid:string -> Types.repost_result Deferred.t
(** Repost a post by its URI and CID *)

val unfollow_user : string -> Types.delete_result Deferred.t
(** Unfollow a user by their DID *)

val unlike_post : uri:string -> cid:string -> Types.delete_result Deferred.t
(** Unlike a post by its URI and CID *)

val unrepost : uri:string -> cid:string -> Types.delete_result Deferred.t
(** Remove a repost by its URI and CID *)
