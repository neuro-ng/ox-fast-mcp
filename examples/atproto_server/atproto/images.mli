(** Image upload and blob storage interface *)

open! Core
open! Async
open Atproto_types.Types

val max_image_size_bytes : int
(** Maximum image size in bytes (1MB per AT Protocol spec) *)

val max_images_per_post : int
(** Maximum images per post *)

val supported_mime_types : string list
(** Supported image MIME types *)

val mime_type_from_path : string -> string
(** Detect MIME type from file path extension *)

val upload_blob : image_data:string -> mime_type:string -> blob_ref Deferred.t
(** Upload binary image data to blob storage
    @param image_data Binary image data
    @param mime_type MIME type (e.g., "image/jpeg")
    @return Blob reference for use in post embeds *)

val upload_image_file : path:string -> blob_ref Deferred.t
(** Upload an image file to blob storage
    @param path Path to image file
    @return Blob reference for use in post embeds *)

val create_image_embed :
  blob_ref:blob_ref ->
  alt_text:string ->
  ?aspect_ratio:int * int ->
  unit ->
  image_embed
(** Create an image embed structure
    @param blob_ref Reference to uploaded blob
    @param alt_text Alt text for accessibility
    @param aspect_ratio Optional aspect ratio (width, height)
    @return Image embed ready for post *)

val create_images_embed : image_param list -> images_embed option Deferred.t
(** Create images embed from list of image parameters Uploads all images and
    creates embed structure
    @param image_params List of image paths with alt text
    @return None if empty, Some embed if 1-4 images
    @raise Failure if more than 4 images or upload fails *)

val create_reply_ref : reply_param -> reply_ref
(** Create reply reference from parameters *)

val create_quote_embed : quote_param -> record_embed
(** Create quote embed from parameters *)
