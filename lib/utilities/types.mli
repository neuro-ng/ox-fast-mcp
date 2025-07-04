open! Core
open! Async

(** Basic JSON representation *)
type json = Yojson.Safe.t

val json_of_yojson : Yojson.Safe.t -> json
val yojson_of_json : json -> Yojson.Safe.t

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
} [@@deriving yojson]

type content_type =
  | Text of string
  | Image of image_content
  | Audio of audio_content
  | Resource of resource_content
[@@deriving yojson]

(** Helper functions for content types *)
val get_mime_type_from_content : content_type -> string
val get_content_data : content_type -> string
val create_text_content : string -> content_type
val create_image_content : data:string -> mime_type:string -> ?annotations:(string * json) list -> ?format:string -> unit -> content_type
val create_audio_content : data:string -> mime_type:string -> ?annotations:(string * json) list -> ?format:string -> unit -> content_type
val create_resource_content : data:string -> mime_type:string -> ?name:string -> ?annotations:(string * json) list -> ?format:string -> unit -> content_type

(** Conversion functions *)
val image_content_to_content_type : image_content -> content_type
val content_type_to_image_content : content_type -> image_content option

val audio_content_to_content_type : audio_content -> content_type
val content_type_to_audio_content : content_type -> audio_content option

val resource_content_to_content_type : resource_content -> content_type
val content_type_to_resource_content : content_type -> resource_content option

(** Embedded resource type *)
type embedded_resource = {
  type_ : string;
  resource : resource_content;
  annotations : (string * json) list;
} [@@deriving yojson]

val create_embedded_resource : type_:string -> resource:resource_content -> ?annotations:(string * json) list -> unit -> embedded_resource
val embedded_resource_to_content_type : embedded_resource -> content_type
val content_type_to_embedded_resource : content_type -> embedded_resource option

(** Stable module for versioned types *)
module Stable : sig
  module V1 : sig
    module Image : sig
      type t = {
        path : string option;
        data : string option;
        format : string option;
        annotations : (string * json) list;
      } [@@deriving yojson]

      val create : ?path:string -> ?data:string -> ?format:string -> ?annotations:(string * json) list -> unit -> t
      val get_mime_type : t -> string
      val to_image_content : ?mime_type:string -> ?annotations:(string * json) list -> t -> image_content
    end

    module Audio : sig
      type t = {
        path : string option;
        data : string option;
        format : string option;
        annotations : (string * json) list;
      } [@@deriving yojson]

      val create : ?path:string -> ?data:string -> ?format:string -> ?annotations:(string * json) list -> unit -> t
      val get_mime_type : t -> string
      val to_audio_content : ?mime_type:string -> ?annotations:(string * json) list -> t -> audio_content
    end

    module File : sig
      type t = {
        path : string option;
        data : string option;
        format : string option;
        name : string option;
        annotations : (string * json) list;
      } [@@deriving yojson]

      val create : ?path:string -> ?data:string -> ?format:string -> ?name:string -> ?annotations:(string * json) list -> unit -> t
      val get_mime_type : t -> string
      val to_resource_content : ?mime_type:string -> ?annotations:(string * json) list -> t -> resource_content
    end
  end
end

module Image = Stable.V1.Image
module Audio = Stable.V1.Audio
module File = Stable.V1.File

(** Type inspection functions *)
val is_class_member_of_type :
  ([> `Annotated of 'a * 'b
   | `Class of ([> `Class of 'd ] as 'c) option
   | `Union of 'a list ] as 'a) option
  -> 'c option
  -> bool

val issubclass_safe :
  ([> `Class of 'b ] as 'a) option
  -> 'a option
  -> bool

val find_kwarg_by_type :
  'a
  -> [> `Class of 'b ] option
  -> string option 