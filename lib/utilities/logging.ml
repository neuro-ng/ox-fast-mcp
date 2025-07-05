open! Core
open! Async

module Log_level = struct
  type t =
    | Debug
    | Info
    | Warning
    | Error
    | Critical
  [@@deriving sexp, compare, yojson]

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
end

module Logger = struct
  type handler = {
    module_instance : (module Log_types.Handler);
    instance : Log_handler.t;
  }

  type t = {
    name : string;
    level : Log_types.Level.t;
    mutable handlers : handler list;
  }

  let create ?(level = Log_types.Level.Info) name =
    { name; level; handlers = [] }

  let get_logger name =
    create ~level:Log_types.Level.Info (sprintf "FastMCP.%s" name)

  let get_name t = t.name
  let get_level t = t.level
  let get_handlers t = t.handlers

  let add_handler t handler_module =
    let handler = {
      module_instance = handler_module;
      instance = Log_handler.create ();
    } in
    t.handlers <- handler :: t.handlers

  let remove_handler t handler_module =
    t.handlers <- List.filter t.handlers ~f:(fun h -> not (phys_equal h.module_instance handler_module))

  let clear_handlers t =
    t.handlers <- []

  let log t level msg =
    if Log_types.Level.compare_level level t.level >= 0 then
      List.iter t.handlers ~f:(fun handler ->
        Log_handler.log handler.instance ~level ~msg)

  let debug t msg = log t Log_types.Level.Debug msg
  let info t msg = log t Log_types.Level.Info msg
  let warning t msg = log t Log_types.Level.Warning msg
  let error t msg = log t Log_types.Level.Error msg
  let critical t msg = log t Log_types.Level.Critical msg
end

module Rich_handler = struct
  type t = {
    enable_rich_tracebacks : bool;
    formatter : Log_formatter.t;
  }

  let create ?(enable_rich_tracebacks = true) () =
    { enable_rich_tracebacks
    ; formatter = Log_formatter.create "%(message)s"
    }

  let format t ~level ~msg =
    Log_formatter.format t.formatter ~level ~msg

  let log t ~level ~msg =
    let formatted = format t ~level ~msg in
    if t.enable_rich_tracebacks then
      eprintf "%s\n%!" formatted
    else
      eprintf "%s\n%!" formatted
end

let configure_logging
    ?(level = Log_types.Level.Info)
    ?(enable_rich_tracebacks = true)
    ?logger
    () =
  let logger = Option.value logger ~default:(Logger.create ~level "FastMCP") in
  Logger.clear_handlers logger;
  let rich_handler = Rich_handler.create ~enable_rich_tracebacks () in
  ignore (rich_handler : Rich_handler.t);  (* Will be used when we implement proper rich handling *)
  Logger.add_handler logger (module Rich_handler);
  logger 