open Core
open Lwt
open Cohttp
open Bearer_auth

(** Get the access token from the current context *)
val get_access_token : unit -> authenticated_user option Lwt.t

(** Auth context middleware module *)
module Auth_context_middleware : sig
  (** Create a new auth context middleware *)
  val create : unit -> unit

  (** Handle a request *)
  val handle : 
    auth_result option ->
    Request.t ->
    (Response.t * Body.t) Lwt.t ->
    (Response.t * Body.t) Lwt.t
end 