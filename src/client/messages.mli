(** Client Messages Module - Simplified Async Version

    Provides message handling for MCP client callbacks. *)

open Async

type message =
  | Request of { method_ : string; params : Yojson.Safe.t option }
  | Notification of { method_ : string; params : Yojson.Safe.t option }
  | Error of { message : string; code : int }
(** Simplified message type *)

type t
(** Message handler type - opaque *)

val create : unit -> t
(** Create a new message handler *)

val dispatch : t -> message -> unit Deferred.t
(** Dispatch a message to appropriate handlers *)
