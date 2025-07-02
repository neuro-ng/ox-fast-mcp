open Mcp_types
open Mcp_client.Types

(** Type alias for log messages *)
type log_message = logging_message_notification_params

(** Type alias for log handlers *)
type log_handler = log_message -> unit Lwt.t

(** Format a log message with proper structure *)
val format_log_message : log_message -> string

(** Convert MCP log level to Logs level *)
val to_logs_level : string -> Logs.level

(** Default log handler *)
val default_log_handler : log_handler

(** Create a log callback function from a handler.
    If no handler is provided, uses the default_log_handler.
    Ensures the logger is properly initialized with at least Debug level. *)
val create_log_callback : ?handler:log_handler -> unit -> logging_fn 