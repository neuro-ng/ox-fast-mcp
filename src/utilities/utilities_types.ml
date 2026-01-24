open Core
open Async

(** Utilities for handling media files - thin wrapper around Fmcp_types

    This module provides async-first API for media handling, delegating to the
    existing Fmcp_types implementation. It serves as a compatibility layer
    between async and sync APIs. *)

(** {1 Function Name Utilities} *)

(** Get the name of a function. In OCaml, this is primarily useful for debugging
    and logging purposes. *)
let get_fn_name (_fn : 'a -> 'b) : string =
  (* OCaml doesn't have runtime function introspection like Python. This is a
     placeholder that returns a generic string. In practice, callers should pass
     function names explicitly. *)
  "<function>"

(** {1 Path Utilities} *)

module Path_utils = struct
  (** Expand environment variables and tilde in a path *)
  let expand_path (path_str : string) : string =
    (* Expand environment variables using $VAR *)
    let expanded_env =
      Re.Str.global_substitute
        (Re.Str.regexp "\\$\\([A-Za-z_][A-Za-z0-9_]*\\)")
        (fun matched ->
          let var_name = Re.Str.matched_group 1 matched in
          match Sys.getenv var_name with
          | Some value -> value
          | None -> Re.Str.matched_string matched)
        path_str
    in
    (* Expand tilde *)
    match String.is_prefix expanded_env ~prefix:"~" with
    | true -> (
      match Sys.getenv "HOME" with
      | Some home ->
        home
        ^ String.sub expanded_env ~pos:1 ~len:(String.length expanded_env - 1)
      | None -> expanded_env)
    | false -> expanded_env

  (** Get file extension from path (including the dot) *)
  let get_extension (path : string) : string option =
    match Filename.split_extension path with
    | _, Some ext when String.is_prefix ext ~prefix:"." -> Some ext
    | _, Some ext -> Some ("." ^ ext)
    | _, None -> None
end

(** {1 MIME Type Detection} *)

(* MIME type detection is delegated to Fmcp_types modules. Use
   Fmcp_types.Image.get_mime_type, Fmcp_types.Audio.get_mime_type, etc. *)

(** {1 Base64 Encoding} *)

module Encoding = struct
  (** Encode bytes to base64 string *)
  let encode_base64 (data : bytes) : string =
    Base64.encode_exn (Bytes.to_string data)

  (** Decode base64 string to bytes *)
  let decode_base64 (s : string) : (bytes, Error.t) Result.t =
    match Base64.decode s with
    | Ok decoded -> Ok (Bytes.of_string decoded)
    | Error (`Msg msg) -> Error (Error.of_string msg)
end

(** {1 Media Helper Classes} *)

(** Image helper for returning images from tools *)
module Image = struct
  type t = Fmcp_types.Image.t

  (** Create an Image from a file path (with path expansion) *)
  let create_from_path ?format ?annotations ~path () :
      (t, Error.t) Result.t Deferred.t =
    ignore annotations;
    (* Fmcp_types doesn't use annotations, so we ignore them for creation *)
    let img = Fmcp_types.Image.create ~path ?format () in
    (* Check file exists *)
    let expanded_path = Path_utils.expand_path path in
    match%map Sys.file_exists expanded_path with
    | `Yes -> Ok img
    | `No | `Unknown ->
      Error (Error.of_string (sprintf "Image file not found: %s" expanded_path))

  (** Create an Image from raw data *)
  let create_from_data ?format ?annotations ~data () : t =
    ignore annotations;
    (* Fmcp_types doesn't use annotations, so we ignore them for creation *)
    Fmcp_types.Image.create ~data:(Bytes.to_string data) ?format ()

  (** Convert to MCP ImageContent using async file reading *)
  let to_image_content ?mime_type ?annotations (t : t) :
      (Mcp.Types.image_content, Error.t) Result.t Deferred.t =
    (* Read data asynchronously if from path *)
    let%bind data_result =
      match (Fmcp_types.Image.path t, Fmcp_types.Image.data t) with
      | Some path, None -> (
        let%bind result =
          Monitor.try_with (fun () -> Reader.file_contents path)
        in
        match result with
        | Ok contents -> return (Ok (Base64.encode_exn contents))
        | Error exn ->
          return
            (Error
               (Error.of_string
                  (sprintf "Failed to read image: %s" (Exn.to_string exn)))))
      | None, Some data -> return (Ok (Base64.encode_exn data))
      | Some _, Some _ ->
        return (Error (Error.of_string "Both path and data provided"))
      | None, None -> return (Error (Error.of_string "No image data available"))
    in
    match data_result with
    | Error e -> return (Error e)
    | Ok encoded_data ->
      (* Use async method to get MIME type *)
      let%bind content = Fmcp_types.Image.to_image_content ?mime_type t in
      return
        (Ok
           ({
              Mcp.Types.type_ = `Image;
              data = encoded_data;
              mime_type = content.mime_type;
              annotations;
              meta = None;
            }
             : Mcp.Types.image_content))

  (** Convert to data URI *)
  let to_data_uri ?mime_type (t : t) : (string, Error.t) Result.t Deferred.t =
    let%bind content_result = to_image_content ?mime_type t in
    match content_result with
    | Error e -> return (Error e)
    | Ok content ->
      return (Ok (sprintf "data:%s;base64,%s" content.mime_type content.data))
end

(** Audio helper for returning audio from tools *)
module Audio = struct
  type t = Fmcp_types.Audio.t

  (** Create an Audio from a file path (with path expansion) *)
  let create_from_path ?format ?annotations ~path () :
      (t, Error.t) Result.t Deferred.t =
    ignore annotations;
    (* Fmcp_types doesn't use annotations, so we ignore them for creation *)
    let audio = Fmcp_types.Audio.create ~path ?format () in
    let expanded_path = Path_utils.expand_path path in
    match%map Sys.file_exists expanded_path with
    | `Yes -> Ok audio
    | `No | `Unknown ->
      Error (Error.of_string (sprintf "Audio file not found: %s" expanded_path))

  (** Create an Audio from raw data *)
  let create_from_data ?format ?annotations ~data () : t =
    ignore annotations;
    (* Fmcp_types doesn't use annotations, so we ignore them for creation *)
    Fmcp_types.Audio.create ~data:(Bytes.to_string data) ?format ()

  (** Convert to MCP AudioContent **)
  let to_audio_content ?mime_type ?annotations (t : t) :
      (Mcp.Types.audio_content, Error.t) Result.t Deferred.t =
    (* Read data asynchronously if from path *)
    let%bind data_result =
      match (Fmcp_types.Audio.path t, Fmcp_types.Audio.data t) with
      | Some path, None -> (
        let%bind result =
          Monitor.try_with (fun () -> Reader.file_contents path)
        in
        match result with
        | Ok contents -> return (Ok (Base64.encode_exn contents))
        | Error exn ->
          return
            (Error
               (Error.of_string
                  (sprintf "Failed to read audio: %s" (Exn.to_string exn)))))
      | None, Some data -> return (Ok (Base64.encode_exn data))
      | Some _, Some _ ->
        return (Error (Error.of_string "Both path and data provided"))
      | None, None -> return (Error (Error.of_string "No audio data available"))
    in
    match data_result with
    | Error e -> return (Error e)
    | Ok encoded_data ->
      (* Use async method to get MIME type *)
      let%bind content = Fmcp_types.Audio.to_audio_content ?mime_type t in
      return
        (Ok
           ({
              Mcp.Types.type_ = `Audio;
              data = encoded_data;
              mime_type = content.mime_type;
              annotations;
              meta = None;
            }
             : Mcp.Types.audio_content))
end

(** File helper for returning file data from tools *)
module File = struct
  type t = Fmcp_types.File.t

  (** Create a File from a file path (with path expansion) *)
  let create_from_path ?format ?name ?annotations ~path () :
      (t, Error.t) Result.t Deferred.t =
    ignore annotations;
    (* Fmcp_types doesn't use annotations, so we ignore them for creation *)
    let file = Fmcp_types.File.create ~path ?format ?name () in
    let expanded_path = Path_utils.expand_path path in
    match%map Sys.file_exists expanded_path with
    | `Yes -> Ok file
    | `No | `Unknown ->
      Error (Error.of_string (sprintf "File not found: %s" expanded_path))

  (** Create a File from raw data *)
  let create_from_data ?format ?name ?annotations ~data () : t =
    ignore annotations;
    (* Fmcp_types doesn't use annotations, so we ignore them for creation *)
    Fmcp_types.File.create ~data:(Bytes.to_string data) ?format ?name ()

  (** Convert to MCP EmbeddedResource **)
  let to_resource_content ?mime_type ?annotations (t : t) :
      (Mcp.Types.embedded_resource, Error.t) Result.t Deferred.t =
    (* Determine MIME type and URI using fmcp_types logic *)
    let mime_type_val =
      Core.Option.value mime_type ~default:(Fmcp_types.File.get_mime_type t)
    in
    let uri_base =
      match Fmcp_types.File.path t with
      | Some p -> Filename.basename p
      | None -> "resource"
    in
    let uri = sprintf "file:///%s" uri_base in

    (* Read data asynchronously based on whether it's text or binary *)
    let%bind resource_result =
      if String.is_prefix ~prefix:"text/" mime_type_val then
        (* Text resource *)
        let%bind text_result =
          match (Fmcp_types.File.data t, Fmcp_types.File.path t) with
          | Some d, _ -> return (Ok d)
          | None, Some path -> (
            let%bind result =
              Monitor.try_with (fun () -> Reader.file_contents path)
            in
            match result with
            | Ok contents -> return (Ok contents)
            | Error exn ->
              return
                (Error
                   (Error.of_string
                      (sprintf "Failed to read file: %s" (Exn.to_string exn)))))
          | None, None ->
            return (Error (Error.of_string "No resource data available"))
        in
        match text_result with
        | Ok text ->
          return
            (Ok
               (`Text
                 {
                   Mcp.Types.text;
                   resource_contents =
                     { uri; mime_type = Some mime_type_val; meta = None };
                 }))
        | Error e -> return (Error e)
      else
        (* Blob resource *)
        let%bind blob_result =
          match (Fmcp_types.File.data t, Fmcp_types.File.path t) with
          | Some d, _ -> return (Ok (Base64.encode_exn d))
          | None, Some path -> (
            let%bind result =
              Monitor.try_with (fun () -> Reader.file_contents path)
            in
            match result with
            | Ok contents -> return (Ok (Base64.encode_exn contents))
            | Error exn ->
              return
                (Error
                   (Error.of_string
                      (sprintf "Failed to read file: %s" (Exn.to_string exn)))))
          | None, None ->
            return (Error (Error.of_string "No resource data available"))
        in
        match blob_result with
        | Ok blob ->
          return
            (Ok
               (`Blob
                 {
                   Mcp.Types.blob;
                   resource_contents =
                     { uri; mime_type = Some mime_type_val; meta = None };
                 }))
        | Error e -> return (Error e)
    in
    match resource_result with
    | Error e -> return (Error e)
    | Ok resource_contents ->
      return
        (Ok
           {
             Mcp.Types.type_ = `Resource;
             resource = resource_contents;
             annotations;
             meta = None;
           })
end
