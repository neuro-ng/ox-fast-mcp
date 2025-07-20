open! Core
open! Async
open Yojson.Safe
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type json = Yojson.Safe.t
(** Basic JSON representation with proper OCaml types *)

let json_of_yojson j = Ok j
let yojson_of_json j = j

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

module ImageContent = struct
  type t = image_content [@@deriving yojson]

  let create ~type_ ~data ~mime_type ?annotations () =
    { type_; data; mime_type; annotations }
end

module AudioContent = struct
  type t = audio_content [@@deriving yojson]

  let create ~type_ ~data ~mime_type ?annotations () =
    { type_; data; mime_type; annotations }
end

module TextResourceContents = struct
  type t = text_resource_contents [@@deriving yojson]

  let create ~text ~mime_type ~uri ?meta () =
    { uri; mime_type = Some mime_type; meta; text }
end

module BlobResourceContents = struct
  type t = blob_resource_contents [@@deriving yojson]

  let create ~blob ~mime_type ~uri ?meta () =
    { uri; mime_type = Some mime_type; meta; blob }
end

module EmbeddedResource = struct
  type t = embedded_resource [@@deriving yojson]

  let create ~type_ ~resource ?annotations () = { type_; resource; annotations }
end
