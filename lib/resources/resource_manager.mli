(** Resource manager for FastMCP *)

open Core
open Types

type t
(** Resource manager type *)

val create : unit -> t
(** Create a new resource manager *)

val add_resource : t -> resource -> unit Lwt.t
(** Add a resource to the manager *)

val remove_resource : t -> Uri.t -> unit Lwt.t
(** Remove a resource from the manager *)

val get_resource : t -> Uri.t -> resource option
(** Get a resource by URI *)

val list_resources : t -> resource list
(** List all enabled resources *)

val enable : t -> unit Lwt.t
(** Enable the resource manager *)

val disable : t -> unit Lwt.t
(** Disable the resource manager *)

val is_enabled : t -> bool
(** Check if the resource manager is enabled *)

val clear : t -> unit Lwt.t
(** Remove all resources from the manager *) 