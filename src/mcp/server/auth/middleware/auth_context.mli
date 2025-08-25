open Cohttp
open Bearer_auth

val get_access_token :
  unit -> Mcp_server_auth.Provider.access_token option Lwt.t
(** Get the access token from the current context *)

(** Auth context middleware module *)
module Auth_context_middleware : sig
  val create : unit -> unit
  (** Create a new auth context middleware *)

  val handle :
    auth_result option ->
    Request.t ->
    (Response.t * Body.t) Lwt.t ->
    (Response.t * Body.t) Lwt.t
  (** Handle a request *)
end
