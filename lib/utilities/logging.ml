open Logs

(** Log levels supported by OxFastMCP *)
type level = Debug | Info | Warning | Error | Critical

(** Convert string level to level type *)
let level_of_string = function
  | "DEBUG" -> Debug
  | "INFO" -> Info
  | "WARNING" -> Warning
  | "ERROR" -> Error
  | "CRITICAL" -> Critical
  | _ -> Info (* Default to INFO *)

(** Convert level type to Logs.level *)
let to_logs_level = function
  | Debug -> Logs.Debug
  | Info -> Logs.Info
  | Warning -> Logs.Warning
  | Error -> Logs.Error
  | Critical -> Logs.Error (* Logs doesn't have Critical, map to Error *)

(** Convert string level to Logs.level *)
let string_to_logs_level s = to_logs_level (level_of_string s)

(** Get a logger nested under OxFastMCP namespace *)
let get_logger name =
  let src = Logs.Src.create ("OxFastMCP." ^ name) in
  let module L = struct
    let src = src
    
    let msg level msgf =
      let log = Logs.msg ~src level in
      log msgf
    
    let app msgf =
      let log = Logs.app ~src in
      log msgf
    
    let err msgf =
      let log = Logs.err ~src in
      log msgf
    
    let warn msgf =
      let log = Logs.warn ~src in
      log msgf
    
    let info msgf =
      let log = Logs.info ~src in
      log msgf
    
    let debug msgf =
      let log = Logs.debug ~src in
      log msgf
    
    let kmsg level k msgf =
      let log = Logs.kmsg ~src level k in
      log msgf
    
    let on_error ?level ?header ?tags ~pp ~use res =
      let pp_error ppf e = pp ppf e in
      Logs.on_error ~src ?level ?header ?tags ~pp:pp_error ~use res
    
    let on_error_msg ?level ?header ?tags ~use res =
      Logs.on_error_msg ~src ?level ?header ?tags ~use res
  end in
  (module L : LOG)

(** Configure logging for OxFastMCP *)
let configure_logging
    ?(level="INFO")
    ?(enable_rich_tracebacks=true)
    () =
  (* Set up colored output to stderr *)
  let formatter _src level ~over k msgf =
    let style = match level with
      | Logs.App -> `Blue
      | Logs.Debug -> `Blue
      | Logs.Info -> `Green
      | Logs.Warning -> `Yellow
      | Logs.Error -> `Red in
    let ppf = Fmt.stderr in
    let k _ = over (); k () in
    msgf @@ fun ?header:_ ?tags:_ fmt ->
    Fmt.kpf k ppf ("%a @[" ^^ fmt ^^ "@]@.")
      (Fmt.styled style Fmt.string)
      (Logs.level_to_string (Some level)) in
  
  (* Create reporter *)
  let reporter = {
    Logs.report = formatter
  } in
  
  (* Set reporter and log level *)
  Logs.set_reporter reporter;
  Logs.set_level (Some (string_to_logs_level level));
  
  (* Enable backtraces if requested *)
  if enable_rich_tracebacks then
    Printexc.record_backtrace true 