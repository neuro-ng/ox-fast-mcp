(** Client Roots Module - Simplified Async Version

    Provides root configuration and management for MCP clients. *)

open Core
open Async

type root = { uri : Uri.t }
(** Simple root type with just URI *)

type roots_list = [ `String of string | `Root of root | `Uri of Uri.t ]
(** Type for roots list - can be a string, root record, or Uri.t *)

type roots_handler = unit -> root list Deferred.t
(** Type for roots handler function *)

let convert_roots_list (roots : roots_list list) : root list =
  List.map roots ~f:(function
    | `String uri -> { uri = Uri.of_string uri }
    | `Root r -> r
    | `Uri uri -> { uri })

let create_callback_from_roots (roots : roots_list list) :
    unit -> root list Deferred.t =
  let converted_roots = convert_roots_list roots in
  fun () -> return converted_roots

let create_callback_from_fn (fn : roots_handler) :
    unit -> root list Deferred.t =
  fn

let create_callback
    (handler : [< `Static of roots_list list | `Handler of roots_handler ]) =
  match handler with
  | `Static roots -> create_callback_from_roots roots
  | `Handler fn -> create_callback_from_fn fn
