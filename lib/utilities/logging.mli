open! Core
open! Async

module Logger : sig
  type t
  type handler = private {
    module_instance : (module Log_types.Handler);
    instance : Log_handler.t;
  }

  val create : ?level:Log_types.Level.t -> string -> t
  val get_logger : string -> t
  val get_name : t -> string
  val get_level : t -> Log_types.Level.t
  val get_handlers : t -> handler list
  val add_handler : t -> (module Log_types.Handler) -> unit
  val remove_handler : t -> (module Log_types.Handler) -> unit
  val clear_handlers : t -> unit

  val debug : t -> string -> unit
  val info : t -> string -> unit
  val warning : t -> string -> unit
  val error : t -> string -> unit
  val critical : t -> string -> unit
end

module Rich_handler : sig
  type t

  val create : ?enable_rich_tracebacks:bool -> unit -> t
  val format : t -> level:Log_types.Level.t -> msg:string -> string
  val log : t -> level:Log_types.Level.t -> msg:string -> unit
end

val configure_logging :
  ?level:Log_types.Level.t ->
  ?enable_rich_tracebacks:bool ->
  ?logger:Logger.t ->
  unit ->
  Logger.t

module Global : sig
  val debug : string -> unit
  val info : string -> unit
  val warning : string -> unit
  val error : string -> unit
  val critical : string -> unit
end 