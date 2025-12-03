(** Prompt Manager for OxFastMCP
    
    Manages a collection of prompts with support for registration, retrieval, and rendering.
    Implements the Manager.S pattern for consistency.
*)

open! Core
open! Async

(** {1 Duplicate Behavior} *)

module DuplicateBehavior : sig
  type t =
    | Warn
    | Replace
    | Error
    | Ignore
  [@@deriving sexp, yojson]

  val of_string : string -> t Or_error.t
  val of_string_exn : string -> t
end

(** {1 Prompt Manager Type} *)

type t
(** Manager for handling multiple prompts *)

(** {1 Manager Creation} *)

val create :
  ?duplicate_behavior:DuplicateBehavior.t ->
  ?mask_error_details:bool ->
  unit ->
  t
(** Create a new prompt manager *)

(** {1 Prompt Operations} *)

val add : t -> Prompt_types.t -> unit Deferred.t
(** Add a prompt to the manager *)

val remove : t -> string -> unit Deferred.t
(** Remove a prompt by key *)

val get : t -> string -> Prompt_types.t option
(** Get a prompt by key *)

val has_prompt : t -> string -> bool
(** Check if a prompt exists *)

val list : t -> Prompt_types.t list
(** List all prompts *)

val list_enabled : t -> Prompt_types.t list
(** List only enabled prompts *)

val enable : t -> string -> bool
(** Enable a prompt. Returns true if successful *)

val disable : t -> string -> bool
(** Disable a prompt. Returns true if successful *)

val is_enabled : t -> string -> bool
(** Check if a prompt is enabled *)

val clear : t -> unit Deferred.t
(** Clear all prompts *)

val count : t -> int
(** Get the number of prompts *)

val find_by_tags : t -> string list -> Prompt_types.t list
(** Find prompts by tags *)

val find_by_name : t -> string -> Prompt_types.t option
(** Find prompt by name *)

(** {1 Prompt Rendering} *)

val render_prompt :
  t ->
  string ->
  arguments:(string * Yojson.Safe.t) list option ->
  (Prompt_types.prompt_message list, Ox_fast_mcp.Exceptions.error_data) Deferred.Result.t
(** Render a prompt with the given arguments *)

(** {1 Utility Functions} *)

val get_prompts : t -> Prompt_types.t list
(** Alias for list *)

val set_duplicate_behavior : t -> DuplicateBehavior.t -> unit
(** Set the duplicate behavior *)

val set_mask_error_details : t -> bool -> unit
(** Set whether to mask error details *)

