(** Resource types for FastMCP *)

open Core

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

val make_text_resource :
  ?name:string ->
  ?description:string ->
  ?mime_type:string ->
  ?tags:string list ->
  ?enabled:bool ->
  uri:Uri.t ->
  string ->
  text_resource
(** Create a text resource *)

val make_binary_resource :
  ?name:string ->
  ?description:string ->
  ?mime_type:string ->
  ?tags:string list ->
  ?enabled:bool ->
  uri:Uri.t ->
  bytes ->
  binary_resource
(** Create a binary resource *)

val make_file_resource :
  ?name:string ->
  ?description:string ->
  ?mime_type:string ->
  ?tags:string list ->
  ?enabled:bool ->
  uri:Uri.t ->
  string ->
  file_resource
(** Create a file resource *)

val make_http_resource :
  ?name:string ->
  ?description:string ->
  ?mime_type:string ->
  ?tags:string list ->
  ?enabled:bool ->
  uri:Uri.t ->
  Uri.t ->
  http_resource
(** Create an HTTP resource *)

val make_directory_resource :
  ?name:string ->
  ?description:string ->
  ?mime_type:string ->
  ?tags:string list ->
  ?enabled:bool ->
  uri:Uri.t ->
  string ->
  directory_resource
(** Create a directory resource *)

val make_function_resource :
  ?name:string ->
  ?description:string ->
  ?mime_type:string ->
  ?tags:string list ->
  ?enabled:bool ->
  uri:Uri.t ->
  (unit -> 'a Lwt.t) ->
  'a function_resource
(** Create a function resource *)
