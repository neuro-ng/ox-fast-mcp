(** Comprehensive logging middleware for OxFastMCP servers *)

open Async
open Middleware
open Logging

(** Basic logging middleware *)
module Logging_middleware : sig
  type t = {
    middleware_logger : Logger.t;
    log_level : Level.t;
    include_payloads : bool;
    max_payload_length : int;
    methods : string list option;
  }

  val create :
    ?middleware_logger:Logger.t ->
    ?log_level:Level.t ->
    ?include_payloads:bool ->
    ?max_payload_length:int ->
    ?methods:string list option ->
    unit ->
    t
  (** Create logging middleware
      @param middleware_logger Logger instance to use
      @param log_level Log level for messages (default: Info)
      @param include_payloads Whether to include message payloads in logs
      @param max_payload_length Maximum length of payload to log
      @param methods List of methods to log. If None, logs all methods *)

  val format_message : t -> context -> string
  (** Format a message for logging *)

  val on_message : t -> context -> 'a call_next -> 'a Deferred.t
  (** Log all messages *)
end

(** Structured JSON logging middleware *)
module Structured_logging : sig
  type t = {
    middleware_logger : Logger.t;
    log_level : Level.t;
    include_payloads : bool;
    methods : string list option;
  }

  val create :
    ?middleware_logger:Logger.t ->
    ?log_level:Level.t ->
    ?include_payloads:bool ->
    ?methods:string list option ->
    unit ->
    t
  (** Create structured logging middleware
      @param middleware_logger Logger instance to use
      @param log_level Log level for messages (default: Info)
      @param include_payloads Whether to include message payloads in logs
      @param methods List of methods to log. If None, logs all methods *)

  val create_log_entry :
    t -> context -> string -> (string * Yojson.Safe.t) list -> Yojson.Safe.t
  (** Create a structured log entry *)

  val on_message : t -> context -> 'a call_next -> 'a Deferred.t
  (** Log structured message information *)
end
