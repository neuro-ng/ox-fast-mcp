(** Logging utilities for OxFastMCP *)

(** Log levels supported by OxFastMCP *)
type level = Debug | Info | Warning | Error | Critical

val level_of_string : string -> level
(** Convert string level to level type *)

val to_async_level : level -> Async_log.Level.t
(** Convert level type to Log.Level.t *)

val string_to_async_level : string -> Async_log.Level.t
(** Convert string level to Log.Level.t *)

val configure_logging :
  ?level:string ->
  ?output:[ `Stderr | `File of string ] ->
  unit ->
  unit Async.Deferred.t
(** Configure logging for OxFastMCP.
    @param level The log level to use (default: "INFO")
    @param enable_rich_tracebacks
      Whether to enable rich tracebacks (default: true)
    @param output Where to write logs (default: stderr)
    @return Deferred.unit *)
