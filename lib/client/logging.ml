open Lwt.Syntax
open Mcp_types
open Mcp_client.Types

type log_message = logging_message_notification_params
type log_handler = log_message -> unit Lwt.t

(** Logger source for client logging *)
let logger = Logs.Src.create "client.logging" ~doc:"Client logging module"

module Log = (val Logs.src_log logger)

(** Format a log message with proper structure *)
let format_log_message message =
  let open Yojson.Safe.Util in
  let json = logging_message_notification_params_to_yojson message in
  let level =
    json |> member "level" |> to_string_option |> Option.value ~default:"debug"
  in
  let msg =
    json |> member "message" |> to_string_option |> Option.value ~default:""
  in
  let source = json |> member "source" |> to_string_option in
  let timestamp = json |> member "timestamp" |> to_float_option in
  let formatted =
    match (source, timestamp) with
    | Some src, Some ts -> Printf.sprintf "[%s][%f] %s: %s" src ts level msg
    | Some src, None -> Printf.sprintf "[%s] %s: %s" src level msg
    | None, Some ts -> Printf.sprintf "[%f] %s: %s" ts level msg
    | None, None -> Printf.sprintf "%s: %s" level msg
  in
  formatted

(** Convert MCP log level to Logs level *)
let to_logs_level = function
  | "debug" -> Logs.Debug
  | "info" -> Logs.Info
  | "warning" -> Logs.Warning
  | "error" -> Logs.Error
  | _ -> Logs.Debug

let default_log_handler message =
  let json = logging_message_notification_params_to_yojson message in
  let level =
    Yojson.Safe.Util.(json |> member "level" |> to_string_option)
    |> Option.value ~default:"debug"
    |> to_logs_level
  in
  let formatted = format_log_message message in
  Log.msg level (fun m -> m "%s" formatted);
  Lwt.return_unit

(** Create a log callback with optional custom handler *)
let create_log_callback ?(handler = default_log_handler) () =
  (* Ensure logger is properly initialized *)
  if not @@ Logs.Src.is_enabled logger then Logs.set_level (Some Logs.Debug);
  fun params -> handler params
