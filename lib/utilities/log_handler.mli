open! Core
open! Async

module type S = Log_types.Handler

type t

val create : ?formatter:Log_formatter.t -> unit -> t
val log : t -> level:Log_types.Level.t -> msg:string -> unit
val format : t -> level:Log_types.Level.t -> msg:string -> string 