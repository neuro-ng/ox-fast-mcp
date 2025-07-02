(** Logging utilities for OxFastMCP *)

(** Log levels supported by OxFastMCP *)
type level = Debug | Info | Warning | Error | Critical

(** Convert string level to level type *)
val level_of_string : string -> level

(** Convert level type to Logs.level *)
val to_logs_level : level -> Logs.level

(** Convert string level to Logs.level *)
val string_to_logs_level : string -> Logs.level

(** Get a logger nested under OxFastMCP namespace.
    @param name The name of the logger, which will be prefixed with 'OxFastMCP.'
    @return A configured logger module *)
val get_logger : string -> (module Logs.LOG)

(** Configure logging for OxFastMCP.
    @param level The log level to use (default: "INFO")
    @param enable_rich_tracebacks Whether to enable rich tracebacks (default: true) *)
val configure_logging :
  ?level:string ->
  ?enable_rich_tracebacks:bool ->
  unit -> unit 