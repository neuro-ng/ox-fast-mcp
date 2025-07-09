open Core
open Lwt.Syntax

type resource = {
  uri : Uri.t;
  name : string;
  mime_type : string;
  description : string option;
  tags : string list;
  enabled : bool;
}
[@@deriving sexp, yojson]
(** Base resource type *)

type text_resource = { base : resource; content : string }
[@@deriving sexp, yojson]
(** Text resource *)

type binary_resource = { base : resource; content : bytes }
[@@deriving sexp, yojson]
(** Binary resource *)

type file_resource = { base : resource; path : string }
[@@deriving sexp, yojson]
(** File resource *)

type http_resource = { base : resource; url : Uri.t } [@@deriving sexp, yojson]
(** HTTP resource *)

type directory_resource = { base : resource; path : string }
[@@deriving sexp, yojson]
(** Directory resource *)

type 'a function_resource = { base : resource; fn : unit -> 'a Lwt.t }
[@@deriving sexp]
(** Function resource *)

(** Helper functions for creating resources *)
let make_text_resource ?name ?description ?mime_type ?tags ?(enabled = true)
    ~uri content =
  let base =
    {
      uri;
      name = Option.value name ~default:(Uri.to_string uri);
      mime_type = Option.value mime_type ~default:"text/plain";
      description;
      tags = Option.value tags ~default:[];
      enabled;
    }
  in
  { base; content }

let make_binary_resource ?name ?description ?mime_type ?tags ?(enabled = true)
    ~uri content =
  let base =
    {
      uri;
      name = Option.value name ~default:(Uri.to_string uri);
      mime_type = Option.value mime_type ~default:"application/octet-stream";
      description;
      tags = Option.value tags ~default:[];
      enabled;
    }
  in
  { base; content }

let make_file_resource ?name ?description ?mime_type ?tags ?(enabled = true)
    ~uri path =
  let base =
    {
      uri;
      name = Option.value name ~default:(Uri.to_string uri);
      mime_type = Option.value mime_type ~default:"application/octet-stream";
      description;
      tags = Option.value tags ~default:[];
      enabled;
    }
  in
  { base; path }

let make_http_resource ?name ?description ?mime_type ?tags ?(enabled = true)
    ~uri url =
  let base =
    {
      uri;
      name = Option.value name ~default:(Uri.to_string uri);
      mime_type = Option.value mime_type ~default:"application/octet-stream";
      description;
      tags = Option.value tags ~default:[];
      enabled;
    }
  in
  { base; url }

let make_directory_resource ?name ?description ?mime_type ?tags
    ?(enabled = true) ~uri path =
  let base =
    {
      uri;
      name = Option.value name ~default:(Uri.to_string uri);
      mime_type = Option.value mime_type ~default:"inode/directory";
      description;
      tags = Option.value tags ~default:[];
      enabled;
    }
  in
  { base; path }

let make_function_resource ?name ?description ?mime_type ?tags ?(enabled = true)
    ~uri fn =
  let base =
    {
      uri;
      name = Option.value name ~default:(Uri.to_string uri);
      mime_type = Option.value mime_type ~default:"text/plain";
      description;
      tags = Option.value tags ~default:[];
      enabled;
    }
  in
  { base; fn }
