(** Comprehensive logging middleware for OxFastMCP servers *)

open Async
open Middleware
open Logging

val get_duration_ms : Core.Time_ns.t -> float
(** Get duration in milliseconds since start time *)

val default_serializer : Yojson.Safe.t -> string
(** Default payload serializer using Yojson *)

(** Basic logging middleware *)
module Logging_middleware : sig
  type t = {
    middleware_logger : Logger.t;
    log_level : Level.t;
    include_payloads : bool;
    include_payload_length : bool;
    estimate_payload_tokens : bool;
    max_payload_length : int;
    methods : string list option;
    payload_serializer : (Yojson.Safe.t -> string) option;
  }

  val create :
    ?middleware_logger:Logger.t ->
    ?log_level:Level.t ->
    ?include_payloads:bool ->
    ?include_payload_length:bool ->
    ?estimate_payload_tokens:bool ->
    ?max_payload_length:int ->
    ?methods:string list option ->
    ?payload_serializer:(Yojson.Safe.t -> string) option ->
    unit ->
    t
  (** Create logging middleware
      @param middleware_logger Logger instance to use
      @param log_level Log level for messages (default: Info)
      @param include_payloads Whether to include message payloads in logs
      @param include_payload_length Whether to include payload size in logs
      @param estimate_payload_tokens
        Whether to estimate token count using length // 4
      @param max_payload_length
        Maximum length of payload to log (prevents huge logs)
      @param methods List of methods to log. If None, logs all methods
      @param payload_serializer Custom serializer for payloads *)

  val serialize_payload : t -> context -> string
  (** Serialize the payload from context *)

  val create_before_message : t -> context -> string
  (** Create a formatted message for the start of processing *)

  val create_after_message : context -> Core.Time_ns.t -> string
  (** Create a formatted message for successful completion *)

  val create_error_message : context -> Core.Time_ns.t -> exn -> string
  (** Create a formatted message for error cases *)

  val on_message : t -> context -> 'a call_next -> 'a Deferred.t
  (** Log all messages *)
end

(** Structured JSON logging middleware *)
module Structured_logging : sig
  type t = {
    middleware_logger : Logger.t;
    log_level : Level.t;
    include_payloads : bool;
    include_payload_length : bool;
    estimate_payload_tokens : bool;
    methods : string list option;
    payload_serializer : (Yojson.Safe.t -> string) option;
  }

  val create :
    ?middleware_logger:Logger.t ->
    ?log_level:Level.t ->
    ?include_payloads:bool ->
    ?include_payload_length:bool ->
    ?estimate_payload_tokens:bool ->
    ?methods:string list option ->
    ?payload_serializer:(Yojson.Safe.t -> string) option ->
    unit ->
    t
  (** Create structured logging middleware
      @param middleware_logger Logger instance to use
      @param log_level Log level for messages (default: Info)
      @param include_payloads Whether to include message payloads in logs
      @param include_payload_length Whether to include payload size in logs
      @param estimate_payload_tokens Whether to estimate token count
      @param methods List of methods to log. If None, logs all methods
      @param payload_serializer Custom serializer for payloads *)

  val serialize_payload : t -> context -> string
  (** Serialize the payload from context *)

  val create_before_message : t -> context -> Yojson.Safe.t
  (** Create a structured log entry for the start of processing *)

  val create_after_message : context -> Core.Time_ns.t -> Yojson.Safe.t
  (** Create a structured log entry for successful completion *)

  val create_error_message : context -> Core.Time_ns.t -> exn -> Yojson.Safe.t
  (** Create a structured log entry for error cases *)

  val on_message : t -> context -> 'a call_next -> 'a Deferred.t
  (** Log structured message information *)
end
