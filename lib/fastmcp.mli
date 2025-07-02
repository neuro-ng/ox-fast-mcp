(** FastMCP - An ergonomic MCP interface *)

(** Settings module for configuring library behavior *)
module Settings : sig
  type t = {
    deprecation_warnings : bool;
  }

  val create : ?deprecation_warnings:bool -> unit -> t
  (** Create settings with optional parameters *)

  val default : t
  (** Default settings instance *)
end

(** Global settings instance *)
val settings : Settings.t

(** Server module containing FastMCP server implementation *)
module Server : sig
  module Context : sig
    type t
    (** Context type for server operations *)
  end

  type t
  (** FastMCP server type *)

  val create : string -> t
  (** Create a new FastMCP server instance 
      @param name The name of the server *)
end

(** Client module containing FastMCP client implementation *)
module Client : sig
  type t
  (** FastMCP client type *)

  val create : Server.t -> t
  (** Create a new FastMCP client instance *)
end

(** Get the current version of FastMCP *)
val version : string

(** [deprecated] Image type - use Utilities.Types.Image instead *)
module Image : sig
  type t [@@deprecated "Use Utilities.Types.Image instead"]
end 