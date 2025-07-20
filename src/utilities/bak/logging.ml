open! Core
open! Async
open! Async_log (* This opens Async_log directly *)

(** Log levels supported by OxFastMCP *)
type level = Debug | Info | Warning | Error | Critical
[@@deriving compare, sexp]

(** Convert string level to level type *)
let level_of_string = function
  | "DEBUG" -> Debug
  | "INFO" -> Info
  | "WARNING" -> Warning
  | "ERROR" -> Error
  | "CRITICAL" -> Critical
  | _ -> Info (* Default to INFO *)

(** Convert level type to Log.Level.t *)
let to_async_level = function
  | Debug -> `Debug
  | Info -> `Info
  | Warning -> `Info
  | Error -> `Error
  | Critical ->
    `Error (* Map Critical to Error since Async_log doesn't have Critical *)

(** Convert string level to Log.Level.t *)
let string_to_async_level s = to_async_level (level_of_string s)

(** Configure logging for OxFastMCP *)
let configure_logging ?(level = "INFO") ?(output = `Stderr) () =
  let output_list =
    match output with
    | `Stderr -> [ Async_log.Output.stderr () ]
    | `File path ->
      let file_output = Async_log.Output.file `Text ~filename:path in
      [ file_output ]
  in

  (* Set global log level *)
  (* Assuming Async_log itself is the Log module, so Global is directly under it *)
  Async_log.Global.set_level (string_to_async_level level);

  (* Set up default logger *)
  let _ =
    Async_log.create
      ~level:(string_to_async_level level)
      ~output:output_list ~on_error:`Raise ()
  in

  (* Corrected line: Set output for the global logger *)
  Async_log.set_output Async_log.Global output_list;

  return ()
