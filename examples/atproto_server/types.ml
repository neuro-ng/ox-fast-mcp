(** Type definitions for ATProto MCP server *)

open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Sexplib.Std

type profile_info = {
  connected : bool;
  handle : string option;
  display_name : string option;
  did : string option;
  followers : int option;
  following : int option;
  posts : int option;
  error : string option;
}
[@@deriving sexp, yojson]
(** Profile information response *)

let profile_info_to_yojson = yojson_of_profile_info

type post_result = {
  success : bool;
  uri : string option;
  cid : string option;
  text : string option;
  created_at : string option;
  error : string option;
}
[@@deriving sexp, yojson]
(** Post creation result *)

let post_result_to_yojson = yojson_of_post_result

type post = {
  author : string;
  text : string option;
  created_at : string option;
  likes : int;
  reposts : int;
  replies : int;
  uri : string;
  cid : string;
}
[@@deriving sexp, yojson]
(** A single post *)

let post_to_yojson = yojson_of_post

type timeline_result = {
  success : bool;
  count : int;
  posts : post list;
  error : string option;
}
[@@deriving sexp, yojson]
(** Timeline fetch result *)

let timeline_result_to_yojson = yojson_of_timeline_result

type search_result = {
  success : bool;
  query : string;
  count : int;
  posts : post list;
  error : string option;
}
[@@deriving sexp, yojson]
(** Search result *)

let search_result_to_yojson = yojson_of_search_result

type notification = {
  reason : string;
  author : string option;
  is_read : bool;
  indexed_at : string;
  uri : string;
  cid : string;
}
[@@deriving sexp, yojson]
(** A single notification *)

let notification_to_yojson = yojson_of_notification

type notifications_result = {
  success : bool;
  count : int;
  notifications : notification list;
  error : string option;
}
[@@deriving sexp, yojson]
(** Notifications fetch result *)

let notifications_result_to_yojson = yojson_of_notifications_result

type follow_result = {
  success : bool;
  handle : string option;
  did : string option;
  uri : string option;
  error : string option;
}
[@@deriving sexp, yojson]
(** Follow result *)

let follow_result_to_yojson = yojson_of_follow_result

type like_result = {
  success : bool;
  liked_uri : string option;
  like_uri : string option;
  error : string option;
}
[@@deriving sexp, yojson]
(** Like result *)

let like_result_to_yojson = yojson_of_like_result

type repost_result = {
  success : bool;
  reposted_uri : string option;
  repost_uri : string option;
  error : string option;
}
[@@deriving sexp, yojson]
(** Repost result *)

let repost_result_to_yojson = yojson_of_repost_result

type rich_text_link = { text : string; url : string } [@@deriving sexp, yojson]
(** Rich text link *)

type rich_text_mention = { handle : string; display_text : string option }
[@@deriving sexp, yojson]
(** Rich text mention *)

type thread_post = {
  text : string;
  images : string list option; [@yojson.option]
  image_alts : string list option; [@yojson.option]
  links : rich_text_link list option; [@yojson.option]
  mentions : rich_text_mention list option; [@yojson.option]
  quote : string option; [@yojson.option]
}
[@@deriving sexp, yojson]
(** Thread post definition *)

type thread_result = {
  success : bool;
  thread_uri : string option;
  post_uris : string list;
  post_count : int;
  error : string option;
}
[@@deriving sexp, yojson]
(** Thread result *)

let thread_result_to_yojson = yojson_of_thread_result

type blob_ref = {
  ref_type : string; [@key "$type"]
  ref_link : string; [@key "$link"]
  mime_type : string; [@key "mimeType"]
  size : int;
}
[@@deriving sexp, yojson]
(** Blob reference for uploaded content **)

type image_embed = {
  image : blob_ref;
  alt : string;
  aspect_ratio : (int * int) option; [@yojson.option]
}
[@@deriving sexp, yojson]
(** Single image with blob reference **)

type images_embed = {
  embed_type : string; [@key "$type"]
  images : image_embed list;
}
[@@deriving sexp, yojson]
(** Images embed (max 4 images) **)

type reply_ref = {
  parent : string * string; (* (uri, cid) *)
  root : string * string; (* (uri, cid) *)
}
[@@deriving sexp, yojson]
(** Reply reference for threading **)

type strong_ref = { uri : string; cid : string } [@@deriving sexp, yojson]
(** Strong reference for quote posts **)

type record_embed = { embed_type : string; [@key "$type"] record : strong_ref }
[@@deriving sexp, yojson]
(** Quote/record embed **)

(** General embed type (union of different embed kinds) **)
type embed =
  | Images of images_embed
  | Record of record_embed (* for quotes *)
  | RecordWithMedia of { record : record_embed; media : images_embed }
[@@deriving sexp]

(** Convert embed to yojson **)
let embed_to_yojson = function
  | Images images -> yojson_of_images_embed images
  | Record record -> yojson_of_record_embed record
  | RecordWithMedia { record; media } ->
    `Assoc
      [
        ("$type", `String "app.bsky.embed.recordWithMedia");
        ("record", yojson_of_record_embed record);
        ("media", yojson_of_images_embed media);
      ]

type image_param = { path : string; alt_text : string option [@yojson.option] }
[@@deriving sexp, yojson]
(** Image upload parameters **)

type reply_param = {
  parent_uri : string;
  parent_cid : string;
  root_uri : string option; [@yojson.option]
  root_cid : string option; [@yojson.option]
}
[@@deriving sexp, yojson]
(** Reply parameters **)

type quote_param = { uri : string; cid : string } [@@deriving sexp, yojson]
(** Quote parameters **)

type profile_update_param = {
  display_name : string option; [@yojson.option]
  description : string option; [@yojson.option]
  avatar_path : string option; [@yojson.option]
}
[@@deriving sexp, yojson]
(** Profile update parameters **)

type profile_update_result = {
  success : bool;
  updated_fields : string list;
  error : string option; [@yojson.option]
}
[@@deriving sexp, yojson]
(** Profile update result **)

let profile_update_result_to_yojson = yojson_of_profile_update_result

type detailed_profile = {
  did : string;
  handle : string;
  display_name : string option; [@yojson.option]
  description : string option; [@yojson.option]
  avatar : string option; [@yojson.option]
  banner : string option; [@yojson.option]
  followers_count : int;
  follows_count : int;
  posts_count : int;
}
[@@deriving sexp, yojson]
(** Detailed profile information **)

type profile_query_result = {
  success : bool;
  profile : detailed_profile option; [@yojson.option]
  error : string option; [@yojson.option]
}
[@@deriving sexp, yojson]
(** Profile query result **)

let profile_query_result_to_yojson = yojson_of_profile_query_result

type delete_result = {
  success : bool;
  record_key : string option; [@yojson.option]
  error : string option; [@yojson.option]
}
[@@deriving sexp, yojson]
(** Delete operation result **)

let delete_result_to_yojson = yojson_of_delete_result
