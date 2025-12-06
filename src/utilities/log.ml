open! Core
open! Async

(** Log levels with comparison and serialization support. *)
module Level = struct
  module T = struct
    type t = Debug | Info | Warning | Error | Critical
    [@@deriving sexp, compare, yojson]

    let level_value = function
      | Debug -> 0
      | Info -> 1
      | Warning -> 2
      | Error -> 3
      | Critical -> 4

    let compare t1 t2 = Int.compare (level_value t1) (level_value t2)
  end

  include T
  include Comparable.Make (T)

  let of_string = function
    | "DEBUG" -> Debug
    | "INFO" -> Info
    | "WARNING" -> Warning
    | "ERROR" -> Error
    | "CRITICAL" -> Critical
    | s -> failwith (sprintf "Invalid log level: %s" s)

  let to_string = function
    | Debug -> "DEBUG"
    | Info -> "INFO"
    | Warning -> "WARNING"
    | Error -> "ERROR"
    | Critical -> "CRITICAL"

  let to_level = function
    | Debug -> `Debug
    | Info -> `Info
    | Warning -> `Warning
    | Error -> `Error
    | Critical -> `Critical

  let compare_level = compare
  let level_ge = ( >= )
  let level_le = ( <= )
  let level_gt = ( > )
  let level_lt = ( < )
  let level_eq = ( = )
end

(* Internal: format a log message with level prefix *)
let format_message ~level ~msg = sprintf "%s %s" (Level.to_string level) msg

(* Internal: format with timestamp *)
let format_with_timestamp ~level ~msg =
  let time = Core_unix.time () in
  let tm = Core_unix.localtime time in
  let microseconds =
    Float.round_decimal
      (Float.mod_float time 1.0 *. 1_000_000.0)
      ~decimal_digits:0
    |> Float.to_int
  in
  let timestamp =
    sprintf "%04d-%02d-%02d %02d:%02d:%02d.%06d+08:00" (tm.tm_year + 1900)
      (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec microseconds
  in
  sprintf "%s %s %s" timestamp (Level.to_string level) msg

(* Global configuration *)
let include_timestamp = ref false
let configure ?(with_timestamp = false) () = include_timestamp := with_timestamp

(* Internal: output a log message to stderr *)
let log_output ~level ~msg =
  let formatted =
    if !include_timestamp then format_with_timestamp ~level ~msg
    else format_message ~level ~msg
  in
  eprintf "%s\n%!" formatted

(** Global logging functions - the primary API for simple use cases. *)
let debug msg = log_output ~level:Level.Debug ~msg

let info msg = log_output ~level:Level.Info ~msg
let warning msg = log_output ~level:Level.Warning ~msg
let error msg = log_output ~level:Level.Error ~msg
let critical msg = log_output ~level:Level.Critical ~msg

(** Handler type signature - kept for backward compatibility. *)
module type Handler = sig
  type t
end

(** Log handler for backward compatibility with existing Logger.add_handler
    calls. *)
module Log_handler = struct
  type t = { mutable log : level:Level.t -> msg:string -> unit }

  let create () = { log = (fun ~level ~msg -> log_output ~level ~msg) }
  let log t ~level ~msg = t.log ~level ~msg
end

(** Rich handler type for backward compatibility. *)
module Rich_handler = struct
  type t = { enable_rich_tracebacks : bool }

  let create ?(enable_rich_tracebacks = true) () = { enable_rich_tracebacks }
  let format _t ~level ~msg = format_message ~level ~msg
  let log _t ~level ~msg = log_output ~level ~msg
end

(** Simple handler type for backward compatibility. *)
module Simple_handler = struct
  type t = { format_pattern : string }

  let create ?(format_pattern = "%(level)s %(message)s") () = { format_pattern }
  let format _t ~level ~msg = format_message ~level ~msg
  let log _t ~level ~msg = log_output ~level ~msg
end

(** Logger with handler management and level-based filtering. *)
module Logger = struct
  type handler = {
    module_instance : (module Handler);
    instance : Log_handler.t;
  }

  type t = { name : string; level : Level.t; mutable handlers : handler list }

  let create ?(level = Level.Info) name = { name; level; handlers = [] }
  let get_logger name = create ~level:Level.Info (sprintf "OxFastMCP.%s" name)
  let get_name t = t.name
  let get_level t = t.level
  let get_handlers t = t.handlers

  let add_handler t handler_module =
    let handler =
      { module_instance = handler_module; instance = Log_handler.create () }
    in
    t.handlers <- handler :: t.handlers

  let remove_handler t handler_module =
    t.handlers <-
      List.filter t.handlers ~f:(fun h ->
          not (phys_equal h.module_instance handler_module))

  let clear_handlers t = t.handlers <- []

  let log t level msg =
    if Level.compare_level level t.level >= 0 then
      List.iter t.handlers ~f:(fun handler ->
          Log_handler.log handler.instance ~level ~msg)

  let debug t msg = log t Level.Debug msg
  let info t msg = log t Level.Info msg
  let warning t msg = log t Level.Warning msg
  let error t msg = log t Level.Error msg
  let critical t msg = log t Level.Critical msg
end

(** Configure logging with custom level and handlers. *)
let configure_logging ?(level = Level.Info) ?(enable_rich_tracebacks = true)
    ?logger () =
  let logger =
    Option.value logger ~default:(Logger.create ~level "OxFastMCP")
  in
  Logger.clear_handlers logger;
  let _rich_handler = Rich_handler.create ~enable_rich_tracebacks () in
  Logger.add_handler logger (module Rich_handler);
  logger

(** Global logging module - for backward compatibility with Logging.Global.*
    calls. *)
module Global = struct
  let configure = configure
  let debug = debug
  let info = info
  let warning = warning
  let error = error
  let critical = critical
end

(* ============================================================ CLIENT LOGGING
   SUPPORT Functions used by the client module for MCP logging callbacks.
   ============================================================ *)

type handler = Level.t -> string -> unit -> unit

let default_handler level message () = log_output ~level ~msg:message
let create_callback handler level message = handler level message ()
