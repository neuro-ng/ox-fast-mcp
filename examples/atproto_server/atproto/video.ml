(** Video upload and embedding for ATProto *)

open! Core
open! Async
open Atproto_types.Types

let max_video_size_bytes = 50_000_000 (* 50MB rough limit *)

let supported_mime_types =
  [ "video/mp4"; "video/mpeg"; "video/webm"; "video/quicktime" ]

(** Detect MIME type from file extension *)
let mime_type_from_path path =
  let ext =
    match String.rsplit2 path ~on:'.' with
    | Some (_, ext) -> String.lowercase ext
    | None -> ""
  in
  match ext with
  | "mp4" -> "video/mp4"
  | "mpeg" | "mpg" -> "video/mpeg"
  | "webm" -> "video/webm"
  | "mov" -> "video/quicktime"
  | _ -> failwith (Printf.sprintf "Unsupported video format: %s" ext)

(** Upload a video file as a blob *)
let upload_video_file ~path : blob_ref Deferred.t =
  let open Deferred.Let_syntax in
  let mime_type = mime_type_from_path path in

  (* Read file *)
  let%bind video_data = Reader.file_contents path in
  let size = String.length video_data in

  if size > max_video_size_bytes then
    failwith
      (Printf.sprintf "Video too large: %d bytes (max %d)" size
         max_video_size_bytes);

  (* Upload blob *)
  let%bind response =
    Client.authenticated_post_blob ~endpoint:"/xrpc/com.atproto.repo.uploadBlob"
      ~content_type:mime_type ~body:video_data
  in

  (* Parse blob reference *)
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

(** Create a video embed structure *)
let create_video_embed ~blob_ref ?alt_text ?aspect_ratio () : video_embed_record
    =
  {
    embed_type = "app.bsky.embed.video";
    video = blob_ref;
    captions = None;
    (* Captions not yet supported in simple upload *)
    alt = alt_text;
    aspect_ratio;
  }
