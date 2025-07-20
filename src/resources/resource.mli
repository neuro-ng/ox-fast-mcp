open Core

(** Base module for all MCP resources *)

(** Content type for resources *)
type content =
  | Text of string
  | Binary of string (* Base64 encoded binary data *)
[@@deriving sexp, yojson_of]

type t = {
  uri : Uri.t;  (** URI of the resource *)
  name : string;  (** Name of the resource *)
  mime_type : string;  (** MIME type of the resource content *)
  description : string option;  (** Optional description of the resource *)
  tags : string list;  (** List of tags associated with the resource *)
  enabled : bool;  (** Whether the resource is enabled *)
  read_fn : (unit -> content Lwt.t) option;
      (** Optional function to read resource content *)
}
[@@deriving sexp, yojson_of]
(** Base type for all resources *)

val validate_mime_type : string -> bool
(** Validates a MIME type string *)

val from_function :
  ?name:string ->
  ?description:string ->
  ?mime_type:string ->
  ?tags:string list ->
  ?enabled:bool ->
  uri:Uri.t ->
  (unit -> content Lwt.t) ->
  t
(** Creates a resource from a function *)

val read : t -> content Lwt.t
(** Reads the content of a resource *)

val enable : t -> t Lwt.t
(** Enables a resource *)

val disable : t -> t Lwt.t
(** Disables a resource *)

val key : t -> string
(** Gets the key of a resource (used for internal bookkeeping) *)

val to_mcp_resource :
  ?overrides:(string * Yojson.Safe.t) list -> t -> Mcp.Types.Resource.t
(** Converts to MCP resource type *)

val text : string -> content
(** Helper to create text content *)

val binary : bytes -> content
(** Helper to create binary content *)
