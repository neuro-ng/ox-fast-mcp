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
  let alog_level = Log_types.Level.to_level level in

  (* Use the level-specific _s (sexp) functions from Log.Global *)
  match alog_level with
  | `Debug -> Log.Global.debug "%s" formatted
  | `Info -> Log.Global.info "%s" formatted
  | `Warning -> Log.Global.info "%s" formatted
  | `Error -> Log.Global.error "%s" formatted
  (* Map `Critical` to `Error` for Async_log as it doesn't have a specific `Critical` level *)
  | `Critical -> Log.Global.error "%s" formatted