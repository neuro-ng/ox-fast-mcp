(** Comprehensive logging middleware for FastMCP servers *)

open Async
open Middleware

(* Simple logger type for middleware *)
type logger_t = {
  name: string;
  level: [`Debug | `Info | `Warning | `Error | `Critical];
}

(** Basic logging middleware *)
module Logging : sig
  type t = {
    logger : logger_t;
    log_level : [`Debug | `Info | `Warning | `Error | `Critical];
    include_payloads : bool;
    max_payload_length : int;
    methods : string list option;
  }

  val create :
    ?logger:logger_t ->
    ?log_level:[`Debug | `Info | `Warning | `Error | `Critical] ->
    ?include_payloads:bool ->
    ?max_payload_length:int ->
    ?methods:string list ->
    unit ->
    t
  (** Create logging middleware
      @param logger
        Logger instance to use. If None, creates a logger named
        'fastmcp.requests'
      @param log_level Log level for messages (default: Info)
      @param include_payloads Whether to include message payloads in logs
      @param max_payload_length
        Maximum length of payload to log (prevents huge logs)
      @param methods List of methods to log. If None, logs all methods *)

  val format_message : t -> context -> string
  (** Format a message for logging *)

  val on_message : t -> context -> 'a call_next -> 'a Deferred.t
  (** Log all messages *)
end

(** Structured JSON logging middleware *)
module Structured_logging : sig
  type t = {
    logger : logger_t;
    log_level : [`Debug | `Info | `Warning | `Error | `Critical];
    include_payloads : bool;
    methods : string list option;
  }

  val create :
    ?logger:logger_t ->
    ?log_level:[`Debug | `Info | `Warning | `Error | `Critical] ->
    ?include_payloads:bool ->
    ?methods:string list ->
    unit ->
    t
  (** Create structured logging middleware
      @param logger
        Logger instance to use. If None, creates a logger named
        'fastmcp.structured'
      @param log_level Log level for messages (default: Info)
      @param include_payloads Whether to include message payloads in logs
      @param methods List of methods to log. If None, logs all methods *)

  val create_log_entry :
    t -> context -> string -> (string * Yojson.Safe.t) list -> Yojson.Safe.t
  (** Create a structured log entry *)

  val on_message : t -> context -> 'a call_next -> 'a Deferred.t
  (** Log structured message information *)
end
