(** OAuth Callback Server Interface

    HTTP server to handle OAuth browser authorization callbacks. *)

open Core
open Async

type callback_result = {
  code : string option;
  state : string option;
  error : string option;
  error_description : string option;
}
(** Callback result containing code and state from OAuth redirect *)

val start_callback_server :
  port:int -> timeout:Time_ns.Span.t -> callback_result Deferred.t
(** Start HTTP server to receive OAuth callback.

    Listens on [localhost:port/callback] for the OAuth redirect. Returns when
    callback is received or timeout expires.

    @param port Port to listen on (e.g., 8080)
    @param timeout Maximum time to wait for callback (e.g., 5 minutes) *)

val success_html : string
(** HTML page returned to browser after successful callback *)

val error_html : error:string -> string
(** HTML page returned to browser on error *)
