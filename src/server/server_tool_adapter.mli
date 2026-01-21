(** Server Tool Adapter

    Provides an adapter that wraps the server's inline tool storage with an
    interface compatible with Tool_manager patterns. Uses a generic type
    parameter to work with any tool record type. *)

open! Core
open! Async

type 'tool t
(** Generic adapter type working with any tool type *)

val create :
  get_tools:(unit -> (string, 'tool) Hashtbl.t) ->
  set_tool:(key:string -> data:'tool -> unit) ->
  remove_tool:(string -> unit) ->
  get_key:('tool -> string) ->
  get_name:('tool -> string) ->
  get_tags:('tool -> String.Set.t) ->
  call_handler:('tool -> Yojson.Safe.t -> Yojson.Safe.t Deferred.t) ->
  on_duplicate:[ `Warn | `Error | `Replace | `Ignore ] ->
  mask_error_details:bool ->
  'tool t

(** {1 Manager-like Interface} *)

val has_tool : 'tool t -> string -> bool
val get_tool : 'tool t -> string -> 'tool option
val list_tools : 'tool t -> 'tool list
val count : 'tool t -> int
val add_tool : 'tool t -> 'tool -> (unit, string) Result.t
val remove_tool : 'tool t -> string -> (unit, string) Result.t
val find_by_tags : 'tool t -> string list -> 'tool list
val find_by_name : 'tool t -> string -> 'tool option

val call_tool :
  'tool t ->
  string ->
  Yojson.Safe.t ->
  (Yojson.Safe.t, string) Result.t Deferred.t
