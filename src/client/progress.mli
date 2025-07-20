open Mcp.Types
open Mcp.Shared
open Lwt.Syntax

type progress_handler = float -> float option -> string option -> unit Lwt.t
(** Progress handler function type *)

val default_progress_handler : progress_handler
(** Default handler for progress notifications. Logs progress updates at debug
    level, properly handling missing total or message values.

    @param progress Current progress value
    @param total Optional total expected value
    @param message Optional status message *)
