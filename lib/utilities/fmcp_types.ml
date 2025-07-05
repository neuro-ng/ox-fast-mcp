open! Core
open! Async
open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Ppx_yojson_conv_lib
open Sexplib.Std

(** Basic JSON representation *)
type json = Yojson.Safe.t

let json_of_yojson x = x
let yojson_of_json x = x

let sexp_of_json (j : json) : Sexp.t =
  Yojson.Safe.to_string j |> Sexp.of_string

let json_of_sexp (s : Sexp.t) : json =
  Sexp.to_string s |> Yojson.Safe.from_string

(** Content types *)
type image_content = {
  data : string;
  mime_type : string;
  annotations : (string * json) list;
  format : string option;
} [@@deriving yojson]

type audio_content = {
  data : string;
  mime_type : string;
  annotations : (string * json) list;
  format : string option;
} [@@deriving yojson]

type resource_content = {
  data : string;
  mime_type : string;
  name : string option;
  annotations : (string * json) list;
  format : string option;
} [@@deriving yojson, sexp]

type content_type =
  | Text of string
  | Image of image_content
  | Audio of audio_content
  | Resource of resource_content
[@@deriving yojson]

(** Helper functions for content types *)
let get_mime_type_from_content = function
  | Text _ -> "text/plain"
  | Image { mime_type; _ } -> mime_type
  | Audio { mime_type; _ } -> mime_type
  | Resource { mime_type; _ } -> mime_type

let get_content_data = function
  | Text text -> text
  | Image { data; _ } -> data
  | Audio { data; _ } -> data
  | Resource { data; _ } -> data

let create_text_content text = Text text

let create_image_content ~data ~mime_type ?(annotations=[]) ?format () =
  Image { data; mime_type; annotations; format }

let create_audio_content ~data ~mime_type ?(annotations=[]) ?format () =
  Audio { data; mime_type; annotations; format }

let create_resource_content ~data ~mime_type ?name ?(annotations=[]) ?format () =
  Resource { data; mime_type; name; annotations; format }

(** Conversion functions *)
let image_content_to_content_type img =
  Image img

let content_type_to_image_content = function
  | Image img -> Some img
  | _ -> None

let audio_content_to_content_type audio =
  Audio audio

let content_type_to_audio_content = function
  | Audio audio -> Some audio
  | _ -> None

let resource_content_to_content_type res =
  Resource res

let content_type_to_resource_content = function
  | Resource res -> Some res
  | _ -> None

(** Resource contents types *)
type blob_resource_contents = {
  uri : string;
  mime_type : string;
  blob : string;
} [@@deriving yojson, sexp]

type text_resource_contents = {
  uri : string;
  mime_type : string;
  text : string;
} [@@deriving yojson, sexp]

type resource_contents =
  | Blob of blob_resource_contents
  | Text of text_resource_contents
[@@deriving yojson, sexp]

(** Embedded resource type *)
type embedded_resource = {
  type_ : [ `Resource ];
  resource : resource_contents;
  annotations : (string * json) list;
} [@@deriving yojson]

let create_embedded_resource ~resource ?(annotations=[]) () =
  { type_ = `Resource; resource; annotations }

let embedded_resource_to_content_type res =
  (* Placeholder conversion – simply wrap as Resource with dummy mime type *)
  match res.resource with
  | Text { text; mime_type; _ } ->
    resource_content_to_content_type { data = text; mime_type; name = None; annotations = res.annotations; format = None }
  | Blob { blob; mime_type; _ } ->
    resource_content_to_content_type { data = blob; mime_type; name = None; annotations = res.annotations; format = None }

let content_type_to_embedded_resource _ct = None

(** Stable module for versioned types *)
module Stable = struct
  module V1 = struct
    module Image = struct
      type t = {
        path : string option;
        data : string option;
        format : string option;
        annotations : (string * json) list;
      } [@@deriving yojson]

      let create ?path ?data ?format ?(annotations=[]) () =
        match path, data with
        | None, None -> failwith "Either path or data must be provided"
        | Some _, Some _ -> failwith "Only one of path or data can be provided"
        | _ -> { path; data; format; annotations }

      let get_mime_type t =
        match t.format with
        | Some fmt -> sprintf "image/%s" (String.lowercase fmt)
        | None ->
          (match t.path with
           | Some path ->
             let ext = Filename.split_extension path |> snd in
             (match ext with
              | Some ".png" -> "image/png"
              | Some ".jpg" | Some ".jpeg" -> "image/jpeg"
              | Some ".gif" -> "image/gif"
              | Some ".webp" -> "image/webp"
              | _ -> "application/octet-stream")
           | None -> "image/png")

      let to_image_content ?mime_type ?(annotations=[]) t : image_content =
        let data =
          match t.path, t.data with
          | Some path, None ->
            In_channel.read_all path |> Base64.encode_exn
          | None, Some data -> Base64.encode_exn data
          | _ -> failwith "No image data available"
        in
        {
          data;
          mime_type = Core.Option.value mime_type ~default:(get_mime_type t);
          annotations = annotations;
          format = t.format;
        }

      let path t = t.path
      let data t = t.data
    end

    module Audio = struct
      type t = {
        path : string option;
        data : string option;
        format : string option;
        annotations : (string * json) list;
      } [@@deriving yojson]

      let create ?path ?data ?format ?(annotations=[]) () =
        match path, data with
        | None, None -> failwith "Either path or data must be provided"
        | Some _, Some _ -> failwith "Only one of path or data can be provided"
        | _ -> { path; data; format; annotations }

      let get_mime_type t =
        match t.format with
        | Some fmt -> sprintf "audio/%s" (String.lowercase fmt)
        | None ->
          (match t.path with
           | Some path ->
             let ext = Filename.split_extension path |> snd in
             (match ext with
              | Some ".wav" -> "audio/wav"
              | Some ".mp3" -> "audio/mpeg"
              | Some ".ogg" -> "audio/ogg"
              | Some ".m4a" -> "audio/mp4"
              | Some ".flac" -> "audio/flac"
              | _ -> "application/octet-stream")
           | None -> "audio/wav")

      let to_audio_content ?mime_type ?(annotations=[]) t : audio_content =
        let data =
          match t.path, t.data with
          | Some path, None ->
            In_channel.read_all path |> Base64.encode_exn
          | None, Some data -> Base64.encode_exn data
          | _ -> failwith "No audio data available"
        in
        {
          data;
          mime_type = Core.Option.value mime_type ~default:(get_mime_type t);
          annotations = annotations;
          format = t.format;
        }

      let path t = t.path
      let data t = t.data
    end

    module File = struct
      type t = {
        path : string option;
        data : string option;
        format : string option;
        name : string option;
        annotations : (string * json) list;
      } [@@deriving yojson]

      let create ?path ?data ?format ?name ?(annotations=[]) () =
        match path, data with
        | None, None -> failwith "Either path or data must be provided"
        | Some _, Some _ -> failwith "Only one of path or data can be provided"
        | _ -> { path; data; format; name; annotations }

      let get_mime_type t =
        match t.format with
        | Some fmt ->
          let fmt = String.lowercase fmt in
          if String.is_prefix ~prefix:"plain" fmt || String.is_prefix ~prefix:"txt" fmt || String.is_prefix ~prefix:"text" fmt
          then "text/plain"
          else sprintf "application/%s" fmt
        | None ->
          (match t.path with
           | Some path ->
             (match Magic_mime.lookup path with
              | "" -> "application/octet-stream"
              | mime -> mime)
           | None -> "application/octet-stream")

      let to_resource_content ?mime_type ?(annotations=[]) t : embedded_resource =
        let mime_type = Core.Option.value mime_type ~default:(get_mime_type t) in
        let uri_base = Core.Option.value t.name ~default:"resource" in
        let ext = Core.Option.value_map t.format ~default:"" ~f:(fun f -> "." ^ f) in
        let uri = sprintf "file:///%s%s" uri_base ext in

        let resource : resource_contents =
          if String.is_prefix ~prefix:"text/" mime_type
          then (
            let text =
              match t.data, t.path with
              | Some d, _ -> d
              | _, Some path -> In_channel.read_all path
              | _ -> failwith "No resource data available"
            in
            Text { uri; mime_type; text }
          ) else (
            let blob_data =
              match t.data, t.path with
              | Some d, _ -> Base64.encode_exn d
              | _, Some path -> In_channel.read_all path |> Base64.encode_exn
              | _ -> failwith "No resource data available"
            in
            Blob { uri; mime_type; blob = blob_data }
          )
        in
        { type_ = `Resource; resource; annotations }

      let path t = t.path
      let data t = t.data
    end
  end
end

module Image = Stable.V1.Image
module Audio = Stable.V1.Audio
module File = Stable.V1.File

(** Type inspection functions *)
let rec is_class_member_of_type type_to_check base_type =
  match type_to_check with
  | None -> false
  | Some t ->
    try
      match t with
      | `Union types -> List.exists types ~f:(fun t -> is_class_member_of_type (Some t) base_type)
      | `Annotated (t, _) -> is_class_member_of_type (Some t) base_type
      | `Class c -> Poly.equal c base_type || issubclass_safe c base_type
      | _ -> false
    with _ -> false

and issubclass_safe sub super =
  match sub, super with
  | None, _ | _, None -> false
  | Some s, Some p ->
    try
      match s, p with
      | `Class s, `Class p -> Poly.equal s p || Poly.equal (Obj.magic s) (Obj.magic p)
      | _ -> false
    with _ -> false

let find_kwarg_by_type func target_type =
  try
    let params = Obj.magic (Obj.repr func) in
    let param_names = List.filter_map params ~f:(fun (name, type_) ->
      if is_class_member_of_type (Some type_) target_type
      then Some name
      else None)
    in
    List.hd param_names
  with _ -> None 

module Time = Time_float_unix