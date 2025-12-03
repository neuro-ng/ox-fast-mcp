(** Resource Manager for OxFastMCP
    
    Manages a collection of resources with support for registration, retrieval, and reading.
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

(** {1 Resource Manager Type} *)

type t
(** Manager for handling multiple resources *)

(** {1 Manager Creation} *)

val create :
  ?duplicate_behavior:DuplicateBehavior.t ->
  ?mask_error_details:bool ->
  unit ->
  t
(** Create a new resource manager *)

(** {1 Resource Operations} *)

val add : t -> Resource_types.t -> unit Deferred.t
(** Add a resource to the manager *)

val remove : t -> string -> unit Deferred.t
(** Remove a resource by key *)

val get : t -> string -> Resource_types.t option
(** Get a resource by key *)

val has_resource : t -> string -> bool
(** Check if a resource exists *)

val list : t -> Resource_types.t list
(** List all resources *)

val list_enabled : t -> Resource_types.t list
(** List only enabled resources *)

val enable : t -> string -> bool
(** Enable a resource. Returns true if successful *)

val disable : t -> string -> bool
(** Disable a resource. Returns true if successful *)

val is_enabled : t -> string -> bool
(** Check if a resource is enabled *)

val enable_manager : t -> unit Deferred.t
(** Enable the entire resource manager *)

val disable_manager : t -> unit Deferred.t
(** Disable the entire resource manager *)

val is_manager_enabled : t -> bool
(** Check if the resource manager is enabled *)

val clear : t -> unit Deferred.t
(** Clear all resources *)

val count : t -> int
(** Get the number of resources *)

val find_by_tags : t -> string list -> Resource_types.t list
(** Find resources by tags *)

val find_by_name : t -> string -> Resource_types.t option
(** Find resource by name *)

(** {1 Resource Reading} *)

val read_resource :
  t ->
  string ->
  (Resource_types.content, Ox_fast_mcp.Exceptions.error_data) Deferred.Result.t
(** Read a resource's content *)

(** {1 Utility Functions} *)

val get_resources : t -> Resource_types.t list
(** Alias for list *)

val set_duplicate_behavior : t -> DuplicateBehavior.t -> unit
(** Set the duplicate behavior *)

val set_mask_error_details : t -> bool -> unit
(** Set whether to mask error details *)
