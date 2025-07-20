open! Core
open! Async

type json = Yojson.Safe.t
(** Basic JSON representation with proper OCaml types *)

val json_of_yojson : Yojson.Safe.t -> (json, string) result
val yojson_of_json : json -> Yojson.Safe.t

type json_value =
  [ `Null
  | `Bool of bool
  | `Int of int
  | `Float of float
  | `String of string
  | `List of json_value list
  | `Assoc of (string * json_value) list ]
[@@deriving yojson]

type role = [ `User | `Assistant ] [@@deriving yojson]
(** Basic types *)

type annotations = {
  audience : role list option; [@yojson.option]
  priority : float option; [@yojson.option]
}
[@@deriving yojson]
(** Resource types *)

type text_content = { type_ : [ `Text ]; [@key "type"] text : string }
[@@deriving yojson]
(** Content types *)

type image_content = {
  type_ : [ `Image ]; [@key "type"]
  data : string;
  mime_type : string; [@key "mimeType"]
  annotations : annotations option; [@yojson.option]
}
[@@deriving yojson]

type audio_content = {
  type_ : [ `Audio ]; [@key "type"]
  data : string;
  mime_type : string; [@key "mimeType"]
  annotations : annotations option; [@yojson.option]
}
[@@deriving yojson]

type text_resource_contents = {
  uri : string;
  mime_type : string option; [@key "mimeType"] [@yojson.option]
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
  text : string;
}
[@@deriving yojson]

type blob_resource_contents = {
  uri : string;
  mime_type : string option; [@key "mimeType"] [@yojson.option]
  meta : Yojson.Safe.t option; [@key "_meta"] [@yojson.option]
  blob : string;
}
[@@deriving yojson]

type embedded_resource = {
  type_ : [ `Resource ]; [@key "type"]
  resource :
    [ `Text of text_resource_contents | `Blob of blob_resource_contents ];
  annotations : annotations option; [@yojson.option]
}
[@@deriving yojson]

module ImageContent : sig
  type t = image_content [@@deriving yojson]

  val create :
    type_:[ `Image ] ->
    data:string ->
    mime_type:string ->
    ?annotations:annotations ->
    unit ->
    t
end

module AudioContent : sig
  type t = audio_content [@@deriving yojson]

  val create :
    type_:[ `Audio ] ->
    data:string ->
    mime_type:string ->
    ?annotations:annotations ->
    unit ->
    t
end

module TextResourceContents : sig
  type t = text_resource_contents [@@deriving yojson]

  val create :
    text:string ->
    mime_type:string ->
    uri:string ->
    ?meta:Yojson.Safe.t ->
    unit ->
    t
end

module BlobResourceContents : sig
  type t = blob_resource_contents [@@deriving yojson]

  val create :
    blob:string ->
    mime_type:string ->
    uri:string ->
    ?meta:Yojson.Safe.t ->
    unit ->
    t
end

module EmbeddedResource : sig
  type t = embedded_resource [@@deriving yojson]

  val create :
    type_:[ `Resource ] ->
    resource:
      [ `Text of text_resource_contents | `Blob of blob_resource_contents ] ->
    ?annotations:annotations ->
    unit ->
    t
end
