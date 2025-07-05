open! Core
open! Async

type t = {
  formatter : Log_formatter.t;
  mutable log : level:Log_types.Level.t -> msg:string -> unit;
}

val create : ?formatter:Log_formatter.t -> unit -> t
val log : t -> level:Log_types.Level.t -> msg:string -> unit
val format : t -> level:Log_types.Level.t -> msg:string -> string 