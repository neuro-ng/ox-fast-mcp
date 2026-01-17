open Async

type handler = float -> float option -> string option -> unit Deferred.t
(** Progress handler function type *)

val default_handler : handler
(** Default handler for progress notifications. Logs progress updates at debug
    level, properly handling missing total or message values.

    @param progress Current progress value
    @param total Optional total expected value
    @param message Optional status message *)
