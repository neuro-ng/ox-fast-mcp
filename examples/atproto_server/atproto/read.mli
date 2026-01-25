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

val fetch_custom_feed :
  feed_uri:string -> limit:int -> Types.timeline_result Deferred.t
(** Fetch a custom feed by AT URI (app.bsky.feed.getFeed) *)

val fetch_post_thread : uri:string -> Types.post_thread_result Deferred.t
(** Fetch full conversation thread for a post *)

val get_post : uri:string -> Types.post_result Deferred.t
(** Get a single post (wrapper around fetch_post_thread) *)
