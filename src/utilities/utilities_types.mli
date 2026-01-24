open Core
open Async

(** Utilities for handling media files and type introspection *)

(** {1 Function Name Utilities} *)

val get_fn_name : ('a -> 'b) -> string
(** Get the name of a function. In OCaml, this is a placeholder since runtime
    function introspection is not available. *)

(** {1 MIME Type Detection} *)

(** MIME type detection is delegated to Fmcp_types modules. Use
    Fmcp_types.Image.get_mime_type, Fmcp_types.Audio.get_mime_type,
    Fmcp_types.File.get_mime_type directly. *)

(** {1 Base64 Encoding} *)

module Encoding : sig
  val encode_base64 : bytes -> string
  (** Encode bytes to base64 string *)

  val decode_base64 : string -> (bytes, Error.t) Result.t
  (** Decode base64 string to bytes *)
end

(** {1 Path Utilities} *)

module Path_utils : sig
  val expand_path : string -> string
  (** Expand environment variables and tilde (~) in a path *)

  val get_extension : string -> string option
  (** Get file extension from path (including the dot) *)
end

(** {1 Media Helper Classes} *)

(** Image helper for returning images from tools *)
module Image : sig
  type t

  val create_from_path :
    ?format:string ->
    ?annotations:Mcp.Types.annotations ->
    path:string ->
    unit ->
    (t, Error.t) Result.t Deferred.t
  (** Create an Image from a file path. Returns Error if the file does not
      exist. *)

  val create_from_data :
    ?format:string ->
    ?annotations:Mcp.Types.annotations ->
    data:bytes ->
    unit ->
    t
  (** Create an Image from raw data bytes *)

  val to_image_content :
    ?mime_type:string ->
    ?annotations:Mcp.Types.annotations ->
    t ->
    (Mcp.Types.image_content, Error.t) Result.t Deferred.t
  (** Convert to MCP ImageContent. Reads file asynchronously if created from
      path. *)

  val to_data_uri :
    ?mime_type:string -> t -> (string, Error.t) Result.t Deferred.t
  (** Convert to data URI (data:image/png;base64,...) *)
end

(** Audio helper for returning audio from tools *)
module Audio : sig
  type t

  val create_from_path :
    ?format:string ->
    ?annotations:Mcp.Types.annotations ->
    path:string ->
    unit ->
    (t, Error.t) Result.t Deferred.t
  (** Create an Audio from a file path. Returns Error if the file does not
      exist. *)

  val create_from_data :
    ?format:string ->
    ?annotations:Mcp.Types.annotations ->
    data:bytes ->
    unit ->
    t
  (** Create an Audio from raw data bytes *)

  val to_audio_content :
    ?mime_type:string ->
    ?annotations:Mcp.Types.annotations ->
    t ->
    (Mcp.Types.audio_content, Error.t) Result.t Deferred.t
  (** Convert to MCP AudioContent. Reads file asynchronously if created from
      path. *)
end

(** File helper for returning file data from tools *)
module File : sig
  type t

  val create_from_path :
    ?format:string ->
    ?name:string ->
    ?annotations:Mcp.Types.annotations ->
    path:string ->
    unit ->
    (t, Error.t) Result.t Deferred.t
  (** Create a File from a file path. Returns Error if the file does not exist. *)

  val create_from_data :
    ?format:string ->
    ?name:string ->
    ?annotations:Mcp.Types.annotations ->
    data:bytes ->
    unit ->
    t
  (** Create a File from raw data bytes *)

  val to_resource_content :
    ?mime_type:string ->
    ?annotations:Mcp.Types.annotations ->
    t ->
    (Mcp.Types.embedded_resource, Error.t) Result.t Deferred.t
  (** Convert to MCP EmbeddedResource. Automatically creates
      TextResourceContents for text/* MIME types, BlobResourceContents for
      others. Reads file asynchronously if created from path. *)
end
