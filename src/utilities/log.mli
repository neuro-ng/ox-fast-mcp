open! Core
open! Async

(** Log levels with comparison and serialization support. *)
module Level : sig
  type t = Debug | Info | Warning | Error | Critical
  [@@deriving sexp, compare, yojson]

  val of_string : string -> t
  val to_string : t -> string
  val to_level : t -> [> `Debug | `Info | `Warning | `Error | `Critical ]

  include Comparable.S with type t := t

  val compare_level : t -> t -> int
  val level_ge : t -> t -> bool
  val level_le : t -> t -> bool
  val level_gt : t -> t -> bool
  val level_lt : t -> t -> bool
  val level_eq : t -> t -> bool
end

val configure : ?with_timestamp:bool -> unit -> unit
(** Configure global logging behavior. *)

val debug : string -> unit
(** Global logging functions - the primary API. *)

val info : string -> unit
val warning : string -> unit
val error : string -> unit
val critical : string -> unit

(** Handler module type - for backward compatibility. *)
module type Handler = sig
  type t
end

(** Log handler - for backward compatibility. *)
module Log_handler : sig
  type t = { mutable log : level:Level.t -> msg:string -> unit }

  val create : unit -> t
  val log : t -> level:Level.t -> msg:string -> unit
end

(** Named logger for structured logging. *)
module Logger : sig
  type handler = private {
    module_instance : (module Handler);
    instance : Log_handler.t;
  }

  type t

  val create : ?level:Level.t -> string -> t
  val get_logger : string -> t
  val get_name : t -> string
  val get_level : t -> Level.t
  val get_handlers : t -> handler list
  val add_handler : t -> (module Handler) -> unit
  val remove_handler : t -> (module Handler) -> unit
  val clear_handlers : t -> unit
  val debug : t -> string -> unit
  val info : t -> string -> unit
  val warning : t -> string -> unit
  val error : t -> string -> unit
  val critical : t -> string -> unit
end

(** Rich handler - for backward compatibility. *)
module Rich_handler : sig
  type t

  val create : ?enable_rich_tracebacks:bool -> unit -> t
  val format : t -> level:Level.t -> msg:string -> string
  val log : t -> level:Level.t -> msg:string -> unit
end

(** Simple handler - for backward compatibility. *)
module Simple_handler : sig
  type t

  val create : ?format_pattern:string -> unit -> t
  val format : t -> level:Level.t -> msg:string -> string
  val log : t -> level:Level.t -> msg:string -> unit
end

val configure_logging :
  ?level:Level.t ->
  ?enable_rich_tracebacks:bool ->
  ?logger:Logger.t ->
  unit ->
  Logger.t
(** Configure logging - backward compatible. *)

(** Global module - for backward compatibility with Logging.Global.* *)
module Global : sig
  val configure : ?with_timestamp:bool -> unit -> unit
  val debug : string -> unit
  val info : string -> unit
  val warning : string -> unit
  val error : string -> unit
  val critical : string -> unit
end

type handler = Level.t -> string -> unit -> unit
(** Client logging callback type. *)

val default_handler : handler
(** Default log handler for client. *)

val create_callback : handler -> Level.t -> string -> unit
(** Create logging callback for client. *)
