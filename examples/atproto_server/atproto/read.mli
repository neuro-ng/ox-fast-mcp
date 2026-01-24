(** AT Protocol read operations interface *)

open! Async
open Atproto_types

val fetch_timeline : int -> Types.timeline_result Deferred.t
(** Fetch the authenticated user's timeline *)

val fetch_notifications : int -> Types.notifications_result Deferred.t
(** Fetch the authenticated user's notifications *)

val search_posts : query:string -> limit:int -> Types.search_result Deferred.t
(** Search for posts matching a query *)
