(** Server Resource Adapter

    Provides an adapter that wraps the server's inline resource storage with an
    interface compatible with Resource_manager patterns. Uses a generic type
    parameter to work with any resource record type. *)

open! Core
open! Async

type 'resource t
(** Generic adapter type working with any resource type *)

val create :
  get_resources:(unit -> (string, 'resource) Hashtbl.t) ->
  set_resource:(key:string -> data:'resource -> unit) ->
  remove_resource:(string -> unit) ->
  get_key:('resource -> string) ->
  get_uri:('resource -> string) ->
  get_name:('resource -> string) ->
  get_tags:('resource -> String.Set.t) ->
  get_description:('resource -> string option) ->
  get_mime_type:('resource -> string) ->
  read_handler:('resource -> string Deferred.t) ->
  on_duplicate:[ `Warn | `Error | `Replace | `Ignore ] ->
  'resource t

(** {1 Manager-like Interface} *)

val has_resource : 'resource t -> string -> bool
val get_resource : 'resource t -> string -> 'resource option
val list_resources : 'resource t -> 'resource list
val count : 'resource t -> int
val add_resource : 'resource t -> 'resource -> (unit, string) Result.t
val remove_resource : 'resource t -> string -> (unit, string) Result.t
val find_by_tags : 'resource t -> string list -> 'resource list
val find_by_name : 'resource t -> string -> 'resource option
val find_by_scheme : 'resource t -> string -> 'resource list

val read_resource :
  'resource t -> string -> (string, string) Result.t Deferred.t
