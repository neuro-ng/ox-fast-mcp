open! Core
open! Async

type t = {
  formatter : Log_formatter.t;
  mutable log : level:Log_types.Level.t -> msg:string -> unit;
}

let create ?(formatter = Log_formatter.create "%(message)s") () =
  {
    formatter;
    log =
      (fun ~level ~msg ->
        let formatted = Log_formatter.format formatter ~level ~msg in
        let alog_level = Log_types.Level.to_level level in
        match alog_level with
        | `Debug -> Log.Global.debug "%s" formatted
        | `Info -> Log.Global.info "%s" formatted
        | `Warning -> Log.Global.info "%s" formatted
        | `Error -> Log.Global.error "%s" formatted
        | `Critical -> Log.Global.error "%s" formatted);
  }

let format t ~level ~msg = Log_formatter.format t.formatter ~level ~msg
let log t ~level ~msg = t.log ~level ~msg
