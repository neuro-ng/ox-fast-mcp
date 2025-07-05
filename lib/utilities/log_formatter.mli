open! Core
open! Async

type t

val create : string -> t
val format : t -> level:Log_types.Level.t -> msg:string -> string 