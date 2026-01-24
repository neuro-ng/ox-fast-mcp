(** Server Prompt Adapter

    Provides an adapter that wraps the server's inline prompt storage with an
    interface compatible with Prompt_manager patterns. Uses a generic type
    parameter to work with any prompt record type. *)

open! Core
open! Async

type 'prompt t
(** Generic adapter type working with any prompt type *)

val create :
  get_prompts:(unit -> (string, 'prompt) Hashtbl.t) ->
  set_prompt:(key:string -> data:'prompt -> unit) ->
  remove_prompt:(string -> unit) ->
  get_key:('prompt -> string) ->
  get_name:('prompt -> string) ->
  get_tags:('prompt -> String.Set.t) ->
  get_description:('prompt -> string option) ->
  render_handler:('prompt -> Yojson.Safe.t -> Yojson.Safe.t Deferred.t) ->
  on_duplicate:[ `Warn | `Error | `Replace | `Ignore ] ->
  'prompt t

(** {1 Manager-like Interface} *)

val has_prompt : 'prompt t -> string -> bool
val get_prompt : 'prompt t -> string -> 'prompt option
val list_prompts : 'prompt t -> 'prompt list
val count : 'prompt t -> int
val add_prompt : 'prompt t -> 'prompt -> (unit, string) Result.t
val remove_prompt : 'prompt t -> string -> (unit, string) Result.t
val find_by_tags : 'prompt t -> string list -> 'prompt list
val find_by_name : 'prompt t -> string -> 'prompt option

val render_prompt :
  'prompt t ->
  string ->
  Yojson.Safe.t ->
  (Yojson.Safe.t, string) Result.t Deferred.t
