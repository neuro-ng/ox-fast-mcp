open! Core
open! Async

type t = {
  formatter : Log_formatter.t;
}

let create ?(formatter = Log_formatter.create "%(message)s") () =
  { formatter }

let format t ~level ~msg =
  Log_formatter.format t.formatter ~level ~msg

let log t ~level ~msg =
  let formatted = format t ~level ~msg in
  match Log_types.Level.to_level level with
  | `Debug -> Log.Global.debug "%s" formatted
  | `Info -> Log.Global.info "%s" formatted
  | `Warning -> Log.Global.info ~level:Async_log_kernel__Level.Warning "%s" formatted
  | `Error -> Log.Global.error "%s" formatted
  | `Critical -> Log.Global.error "%s" formatted  (* Log doesn't have Critical, using Error *) 