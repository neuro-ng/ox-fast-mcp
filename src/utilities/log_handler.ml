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
        eprintf "%s\n%!" formatted);
  }

let format t ~level ~msg = Log_formatter.format t.formatter ~level ~msg
let log t ~level ~msg = t.log ~level ~msg
