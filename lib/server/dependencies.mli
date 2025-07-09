open Core
open Cohttp
open Cohttp_lwt_unix

(** Context management *)
module Context : sig
  val get_context : unit -> Server.Context.t
  val set_context : Server.Context.t -> unit
  val clear_context : unit -> unit
end

(** HTTP Request management *)
module Http_request : sig
  val get_request : unit -> Request.t
  val set_request : Request.t -> unit
  val clear_request : unit -> unit

  val get_headers : ?include_all:bool -> unit -> string String.Map.t
  (** Get headers from the current HTTP request.
      @param include_all
        If true, include all headers. If false (default), exclude problematic
        headers like content-length.
      @return
        Map of header names (lowercase) to values. Empty if no request exists. *)
end

(** Access token management *)
module Access_token : sig
  include module type of Auth.Middleware.Bearer_auth

  val get_token : unit -> string option
  (** Get access token from current request.
      @return Some token if present and valid, None otherwise *)
end
