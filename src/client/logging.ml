(** Client Logging Module - Simplified Async Version

    Provides simple logging handlers for MCP client messages. *)

open Async

type log_handler = Logs.level -> string -> unit Deferred.t
(** Simplified log handler function type - takes level and formatted message *)

(** Logger source for client logging *)
let logger = Logs.Src.create "client.logging" ~doc:"Client logging module"

module Log = (val Logs.src_log logger)

let default_handler level message =
  Log.msg level (fun m -> m "%s" message);
  return ()

(** Create a log callback with optional custom handler *)
let create_callback ?(handler = default_handler) () =
  (* Initialize logger with Debug level if not already set *)
  Logs.set_level (Some Logs.Debug);
  handler
