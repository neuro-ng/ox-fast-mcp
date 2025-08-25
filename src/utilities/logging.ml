open! Core
open! Async

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
    create ~level:Log_types.Level.Info (sprintf "OxFastMCP.%s" name)

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
  type t = { enable_rich_tracebacks : bool; formatter : Log_formatter.t }

  let create ?(enable_rich_tracebacks = true) () =
    { enable_rich_tracebacks; formatter = Log_formatter.create "%(message)s" }

  let format t ~level ~msg = Log_formatter.format t.formatter ~level ~msg

  let log t ~level ~msg =
    let formatted = format t ~level ~msg in
    if t.enable_rich_tracebacks then eprintf "%s\n%!" formatted
    else eprintf "%s\n%!" formatted
end

module Simple_handler = struct
  type t = { formatter : Log_formatter.t }

  let create ?(format_pattern = "%(level)s %(message)s") () =
    { formatter = Log_formatter.create format_pattern }

  let format t ~level ~msg = Log_formatter.format t.formatter ~level ~msg

  let log t ~level ~msg =
    let formatted = format t ~level ~msg in
    eprintf "%s\n%!" formatted
end

let configure_logging ?(level = Log_types.Level.Info)
    ?(enable_rich_tracebacks = true) ?logger () =
  let logger =
    Option.value logger ~default:(Logger.create ~level "OxFastMCP")
  in
  Logger.clear_handlers logger;
  let rich_handler = Rich_handler.create ~enable_rich_tracebacks () in
  ignore (rich_handler : Rich_handler.t);
  (* Will be used when we implement proper rich handling *)
  Logger.add_handler logger (module Rich_handler);
  logger

module Global = struct
  (* Configuration for timestamp inclusion *)
  let include_timestamp = ref false
  
  let simple_formatter = Log_formatter.create "%(level)s %(message)s"
  let timestamped_formatter = Log_formatter.create "%(timestamp)s %(level)s %(message)s"
  
  let get_timestamp () =
    let time = Core_unix.time () in
    let tm = Core_unix.localtime time in
    let microseconds = Float.round_decimal (Float.mod_float time 1.0 *. 1_000_000.0) ~decimal_digits:0 |> Float.to_int in
    sprintf "%04d-%02d-%02d %02d:%02d:%02d.%06d%s"
      (tm.tm_year + 1900)
      (tm.tm_mon + 1)
      tm.tm_mday
      tm.tm_hour
      tm.tm_min
      tm.tm_sec
      microseconds
      (if tm.tm_isdst then "+08:00" else "+08:00") (* Simplified timezone *)
  
  let configure ?(with_timestamp = false) () =
    include_timestamp := with_timestamp
  
  let log_direct level msg =
    let formatted = 
      if !include_timestamp then
        let timestamp = get_timestamp () in
        let msg_with_timestamp = String.substr_replace_all 
          (Log_formatter.format timestamped_formatter ~level ~msg)
          ~pattern:"%(timestamp)s" ~with_:timestamp
        in
        msg_with_timestamp
      else
        Log_formatter.format simple_formatter ~level ~msg
    in
    eprintf "%s\n%!" formatted

  let debug msg = log_direct Log_types.Level.Debug msg
  let info msg = log_direct Log_types.Level.Info msg
  let warning msg = log_direct Log_types.Level.Warning msg
  let error msg = log_direct Log_types.Level.Error msg
  let critical msg = log_direct Log_types.Level.Critical msg
end
