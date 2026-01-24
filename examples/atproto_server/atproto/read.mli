(** AT Protocol read operations interface *)

open! Async
open Atproto_types

val fetch_timeline : int -> Types.timeline_result Deferred.t
(** Fetch the authenticated user's timeline *)

val fetch_notifications : int -> Types.notifications_result Deferred.t
(** Fetch the authenticated user's notifications *)

val search_posts : query:string -> limit:int -> Types.search_result Deferred.t
(** Search for posts matching a query *)

val fetch_author_feed :
  actor:string -> limit:int -> Types.author_feed_result Deferred.t
(** Fetch posts by a specific user *)

val fetch_post_thread : uri:string -> Types.post_thread_result Deferred.t
(** Fetch full conversation thread for a post *)
