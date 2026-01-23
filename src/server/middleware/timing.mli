(** Timing middleware for monitoring MCP operation performance *)

open Async
open Middleware
open Logging

type timing_config = {
  logger : Logger.t;
  log_level : Logs.level;
  detailed : bool; (** Enable detailed operation logging *)
}
(** Timing middleware configuration *)

val create :
  ?logger:Logger.t -> ?log_level:Logs.level -> ?detailed:bool -> unit -> timing_config
(** Create a new timing middleware configuration.
    
    @param logger Logger instance for timing messages (default: "OxFastMCP.Timing")
    @param log_level Logging level for timing messages (default: Info)
    @param detailed Enable detailed operation logging with names extracted from params (default: false)
*)

val time_operation : timing_config -> context -> 'a call_next -> 'a Deferred.t
(** Time an operation and log the duration *)

val on_message : timing_config -> context -> 'a call_next -> 'a Deferred.t
(** Handle any message with timing *)

(** Timing middleware module implementing the Middleware.S interface *)
module Timing : Middleware.S
