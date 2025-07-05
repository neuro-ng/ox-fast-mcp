open! Core
open! Async

type t

val create : ?formatter:Log_formatter.t -> unit -> t
val log : t -> level:Log_types.Level.t -> msg:string -> unit
val format : t -> level:Log_types.Level.t -> msg:string -> string 