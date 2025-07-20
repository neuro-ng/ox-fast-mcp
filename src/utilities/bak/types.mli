(** Types module for FastMCP utilities *)

open! Core
open! Async

type json_value =
  [ `Null
  | `Bool of bool
  | `Int of int
  | `Float of float
  | `String of string
  | `List of json_value list
  | `Assoc of (string * json_value) list ]
[@@deriving sexp, compare, yojson]

type parameter = {
  param_name : string; [@key "name"]
  param_type : string; [@key "type"]
  param_description : string option; [@yojson.option] [@key "description"]
  param_default : json_value option; [@yojson.option] [@key "default"]
  param_required : bool; [@key "required"]
}
[@@deriving fields, sexp, compare, yojson]

val create_parameter :
  name:string ->
  type_:string ->
  ?description:string ->
  ?default:json_value ->
  ?required:bool ->
  unit ->
  parameter

val json_schema : parameter list -> Yojson.Safe.t
val filter_schema : Yojson.Safe.t -> Yojson.Safe.t

type json = Yojson.Safe.t
(** Basic JSON representation *)

type function_signature = {
  name : string;
  description : string option;
  parameters : parameter list;
  return_type : string;
  is_async : bool;
  is_static : bool;
  is_method : bool;
  is_class_method : bool;
}
[@@deriving fields, sexp, compare, yojson]
(** Function signature type *)

type function_parameter = {
  param_name : string;
  param_type : string;
  param_description : string option;
  param_required : bool;
  param_default : json option;
}
[@@deriving fields, sexp, compare, yojson]
(** Function parameter definition for type adapters *)

(** Content types for MCP messages *)
type content_type =
  | Text of string
  | Image of {
      data : string;
      mime_type : string;
      annotations : (string * json) list option;
      format : string option;
    }
  | Audio of {
      data : string;
      mime_type : string;
      annotations : (string * json) list option;
      format : string option;
    }
  | File of {
      data : string;
      mime_type : string;
      name : string option;
      annotations : (string * json) list option;
      format : string option;
    }
[@@deriving sexp, compare, yojson]

(** Resource types *)
type resource_type =
  | TextResource of { text : string; mime_type : string }
  | BinaryResource of { data : bytes; mime_type : string }
  | FileResource of { path : string; is_binary : bool; mime_type : string }
  | DirectoryResource of {
      path : string;
      recursive : bool;
      pattern : string option;
      mime_type : string;
    }
  | HttpResource of { url : string; mime_type : string }
[@@deriving sexp, compare]

(** Resource error types *)
type resource_error =
  | ResourceNotFound of string
  | ResourceAccessDenied of string
  | ResourceInvalidPath of string
  | ResourceIOError of string
  | ResourceParseError of string
[@@deriving sexp, compare]

val is_content_type_match : content_type -> content_type -> bool
(** Helper functions *)

val is_option_type_safe : 'a option -> ('a -> bool) -> bool
val get_mime_type_from_content : content_type -> string
val get_content_data : content_type -> string
val create_text_content : string -> content_type

val create_image_content :
  data:string ->
  mime_type:string ->
  ?annotations:(string * json) list ->
  ?format:string ->
  unit ->
  content_type

val create_audio_content :
  data:string ->
  mime_type:string ->
  ?annotations:(string * json) list ->
  ?format:string ->
  unit ->
  content_type

val create_file_content :
  data:string ->
  mime_type:string ->
  ?name:string ->
  ?annotations:(string * json) list ->
  ?format:string ->
  unit ->
  content_type

val create_image_from_data : ?format:string -> string -> content_type
val create_audio_from_data : ?format:string -> string -> content_type

val create_file_from_data :
  ?format:string -> ?name:string -> string -> content_type

val base64_encode : string -> string
val base64_decode : string -> string option

val find_param_by_type :
  function_parameter list -> string -> function_parameter option

val get_mime_type_from_extension : string -> string

val read_resource :
  resource_type -> (content_type, resource_error) result Deferred.t

module Stable : sig
  module V1 : sig
    module Image : sig
      type t [@@deriving sexp, compare, yojson]

      val create :
        ?path:string ->
        ?data:string ->
        ?format:string ->
        ?annotations:Types.Annotations.t ->
        unit ->
        t

      val get_mime_type : t -> string

      val to_image_content :
        ?mime_type:string ->
        ?annotations:Types.Annotations.t ->
        t ->
        Types.ImageContent.t
    end

    module Audio : sig
      type t [@@deriving sexp, compare, yojson]

      val create :
        ?path:string ->
        ?data:string ->
        ?format:string ->
        ?annotations:Types.Annotations.t ->
        unit ->
        t

      val get_mime_type : t -> string

      val to_audio_content :
        ?mime_type:string ->
        ?annotations:Types.Annotations.t ->
        t ->
        Types.AudioContent.t
    end

    module File : sig
      type t [@@deriving sexp, compare, yojson]

      val create :
        ?path:string ->
        ?data:string ->
        ?format:string ->
        ?name:string ->
        ?annotations:Types.Annotations.t ->
        unit ->
        t

      val get_mime_type : t -> string

      val to_resource_content :
        ?mime_type:string ->
        ?annotations:Types.Annotations.t ->
        t ->
        Types.EmbeddedResource.t
    end
  end
end

module Image = Stable.V1.Image
module Audio = Stable.V1.Audio
module File = Stable.V1.File
