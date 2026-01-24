(** Client Sampling Module - Simplified Async Version

    Provides sampling callback management for MCP clients. *)

open Async

type sampling_handler =
  string list ->
  (* messages *)
  Yojson.Safe.t ->
  (* params *)
  (Yojson.Safe.t, string) result Deferred.t
(** Simplified sampling handler - takes messages and params, returns JSON result *)

val create_callback : sampling_handler -> sampling_handler
(** Create a sampling callback function from a handler *)
