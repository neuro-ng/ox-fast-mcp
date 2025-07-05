open! Core
open! Async

type t = {
  format_string : string;
}

let create format_string =
  { format_string }

let format t ~level ~msg =
  let level_str = Log_types.Level.to_string level in
  String.substr_replace_all t.format_string
    ~pattern:"%(message)s"
    ~with_:msg
  |> String.substr_replace_all
    ~pattern:"%(level)s"
    ~with_:level_str 