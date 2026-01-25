(** AT Protocol post creation operations interface *)

open! Async
open Atproto_types

val create_post :
  text:string ->
  ?images:Types.image_param list ->
  ?video:Types.video_param ->
  ?emojis:Types.emoji_def list ->
  ?links:Types.rich_text_link list ->
  ?mentions:Types.rich_text_mention list ->
  ?reply_to:Types.reply_param ->
  ?quote:Types.quote_param ->
  unit ->
  Types.post_result Deferred.t
(** Create a post with optional rich text features. Supports text with links,
    mentions, images, replies, and quotes. *)

val create_thread : Types.thread_post list -> Types.thread_result Deferred.t
(** Create a thread of posts. Currently creates posts sequentially without
    proper reply chaining. Full reply implementation pending. *)

val detect_hashtag_facets : string -> Yojson.Safe.t list
(** Detect hashtags in text and create facets for them *)
