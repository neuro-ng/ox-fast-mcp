(** Ox-Fast-MCP - An ergonomic MCP interface *)

(** Settings module for configuring library behavior *)
module Settings : sig
  type t

  val create : ?deprecation_warnings:bool -> unit -> t
  val default : t
  val get_deprecation_warnings : t -> bool
  val set_deprecation_warnings : t -> bool -> t
end

(** Version information *)
module Version : sig
  val version : string
  val get_version : unit -> string
end

(** Global settings instance *)
val settings : Settings.t

(** Get the current version of ox-fast-mcp *)
val version : string 