open Mcp.Types
open Mcp.Shared
open Lwt.Syntax

(** Type for roots list - can be a string, Root.t, or Uri.t *)
type roots_list = [
  | `String of string
  | `Root of root
  | `Uri of Uri.t
]

(** Type for roots handler function *)
type roots_handler = request_context -> roots_list list Lwt.t

(** Convert a list of roots (strings, Root.t, or Uri.t) to a list of Root.t *)
val convert_roots_list : roots_list list -> root list

(** Create a roots callback function from either a static list or a handler function.
    @param handler Either a static list of roots or a function that returns roots
    @return A function that takes a request context and returns a list_roots_result
    @raise Invalid_argument if the handler is invalid *)
val create_roots_callback : 
  [< `Static of roots_list list | `Handler of roots_handler] -> 
  (request_context -> list_roots_result Lwt.t)

(** Internal function to create a callback from a static list of roots *)
val create_roots_callback_from_roots :
  roots_list list ->
  (request_context -> list_roots_result Lwt.t)

(** Internal function to create a callback from a handler function *)
val create_roots_callback_from_fn :
  roots_handler ->
  (request_context -> list_roots_result Lwt.t) 