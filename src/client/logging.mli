(** Client Logging Module - Simplified Async Version

    Provides simple logging handlers for MCP client messages. *)

open Async

type log_handler = Logs.level -> string -> unit Deferred.t
(** Simplified log handler function type - takes level and formatted message *)

val default_handler : log_handler
(** Default log handler that outputs to Logs *)

val create_callback : ?handler:log_handler -> unit -> log_handler
(** Create a log callback function from a handler *)
