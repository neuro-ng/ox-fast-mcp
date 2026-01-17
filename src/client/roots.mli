(** Client Roots Module - Simplified Async Version

    Provides root configuration and management for MCP clients. *)

open Async

type root = { uri : Uri.t }
(** Simple root type with just URI *)

type roots_list = [ `String of string | `Root of root | `Uri of Uri.t ]
(** Type for roots list - can be a string, Root record, or Uri.t *)

type roots_handler = unit -> root list Deferred.t
(** Type for roots handler function *)

val convert_roots_list : roots_list list -> root list
(** Convert a list of roots (strings, Root record, or Uri.t) to a list of root records *)

val create_callback :
  [< `Static of roots_list list | `Handler of roots_handler ] ->
  (unit -> root list Deferred.t)
(** Create a roots callback function from either a static list or a handler function. *)
