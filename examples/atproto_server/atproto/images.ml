(** Image upload and blob storage for ATProto

    Handles uploading images to AT Protocol blob storage and creating image
    embeds for posts. *)

open! Core
open! Async
open Atproto_types.Types

let max_image_size_bytes = 1_000_000 (* 1MB per AT Protocol spec *)
let max_images_per_post = 4

let supported_mime_types =
  [ "image/jpeg"; "image/png"; "image/gif"; "image/webp" ]

(** Detect MIME type from file extension *)
let mime_type_from_path path =
  let ext =
    match String.rsplit2 path ~on:'.' with
    | Some (_, ext) -> String.lowercase ext
    | None -> ""
  in
  match ext with
  | "jpg" | "jpeg" -> "image/jpeg"
  | "png" -> "image/png"
  | "gif" -> "image/gif"
  | "webp" -> "image/webp"
  | _ -> failwith (Printf.sprintf "Unsupported image format: %s" ext)

(** Upload a blob to AT Protocol storage *)
let upload_blob ~image_data ~mime_type : blob_ref Deferred.t =
  let open Deferred.Let_syntax in
  (* Validate size *)
  let size = String.length image_data in
  if size > max_image_size_bytes then
    failwith
      (Printf.sprintf "Image too large: %d bytes (max %d)" size
         max_image_size_bytes);

  (* Validate MIME type *)
  if not (List.mem supported_mime_types mime_type ~equal:String.equal) then
    failwith (Printf.sprintf "Unsupported MIME type: %s" mime_type);

  (* Upload to blob endpoint *)
  let%bind response =
    Client.authenticated_post_blob ~endpoint:"/xrpc/com.atproto.repo.uploadBlob"
      ~content_type:mime_type ~body:image_data
  in

  (* Parse blob reference from response *)
  let blob_json = Yojson.Safe.Util.member "blob" response in
  let ref_type =
    Yojson.Safe.Util.member "$type" blob_json |> Yojson.Safe.Util.to_string
  in
  let ref_link =
    Yojson.Safe.Util.member "ref" blob_json
    |> Yojson.Safe.Util.member "$link"
    |> Yojson.Safe.Util.to_string
  in
  let mime =
    Yojson.Safe.Util.member "mimeType" blob_json |> Yojson.Safe.Util.to_string
  in
  let blob_size =
    Yojson.Safe.Util.member "size" blob_json |> Yojson.Safe.Util.to_int
  in

  return { ref_type; ref_link; mime_type = mime; size = blob_size }

(** Upload an image from a file path *)
let upload_image_file ~path : blob_ref Deferred.t =
  let open Deferred.Let_syntax in
  (* Detect MIME type *)
  let mime_type = mime_type_from_path path in

  (* Read file *)
  let%bind image_data = Reader.file_contents path in

  (* Upload blob *)
  upload_blob ~image_data ~mime_type

(** Create an image embed structure *)
let create_image_embed ~blob_ref ~alt_text ?aspect_ratio () : image_embed =
  { image = blob_ref; alt = alt_text; aspect_ratio }

(** Create images embed from list of image parameters Returns None if no images,
    Some embed if 1-4 images *)
let create_images_embed (image_params : image_param list) :
    images_embed option Deferred.t =
  let open Deferred.Let_syntax in
  match image_params with
  | [] -> return None
  | images when List.length images > max_images_per_post ->
    failwith
      (Printf.sprintf "Too many images: %d (max %d)" (List.length images)
         max_images_per_post)
  | images ->
    (* Upload each image *)
    let%bind image_embeds =
      Deferred.List.map ~how:`Sequential images ~f:(fun img ->
          let%bind blob_ref = upload_image_file ~path:img.path in
          let alt_text = Option.value img.alt_text ~default:"" in
          return (create_image_embed ~blob_ref ~alt_text ()))
    in

    return
      (Some { embed_type = "app.bsky.embed.images"; images = image_embeds })

(** Helper to create reply reference from parameters *)
let create_reply_ref (params : reply_param) : reply_ref =
  let root_uri = Option.value params.root_uri ~default:params.parent_uri in
  let root_cid = Option.value params.root_cid ~default:params.parent_cid in
  {
    parent = (params.parent_uri, params.parent_cid);
    root = (root_uri, root_cid);
  }

(** Helper to create quote embed from parameters *)
let create_quote_embed (params : quote_param) : record_embed =
  {
    embed_type = "app.bsky.embed.record";
    record = { uri = params.uri; cid = params.cid };
  }
