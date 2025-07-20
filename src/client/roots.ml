open Core
open Mcp.Types
open Mcp.Shared
open Lwt.Syntax

type roots_list = [ `String of string | `Root of root | `Uri of Uri.t ]
type roots_handler = request_context -> roots_list list Lwt.t

let convert_roots_list (roots : roots_list list) : root list =
  try
    List.map roots ~f:(function
      | `String uri -> (
        try { uri = Uri.of_string uri }
        with _ -> raise (Invalid_argument ("Invalid URI string: " ^ uri)))
      | `Root r -> r
      | `Uri uri -> { uri })
  with exn ->
    raise (Invalid_argument ("Invalid root in list: " ^ Exn.to_string exn))

let create_roots_callback_from_roots (roots : roots_list list) :
    request_context -> list_roots_result Lwt.t =
  let converted_roots = convert_roots_list roots in
  fun _context -> Lwt.return { roots = converted_roots; meta = None }

let create_roots_callback_from_fn (fn : roots_handler) :
    request_context -> list_roots_result Lwt.t =
 fun context ->
  let* result =
    try%lwt
      let* roots_list = fn context in
      let roots = convert_roots_list roots_list in
      Lwt.return (`Ok { roots; meta = None })
    with exn ->
      let error =
        {
          code = internal_error;
          message = Exn.to_string exn;
          data = None;
          meta = None;
        }
      in
      Lwt.return (`Error error)
  in
  match result with
  | `Ok result -> Lwt.return result
  | `Error error -> Lwt.fail (Mcp_error error)

let create_roots_callback
    (handler : [< `Static of roots_list list | `Handler of roots_handler ]) =
  match handler with
  | `Static roots -> create_roots_callback_from_roots roots
  | `Handler fn -> create_roots_callback_from_fn fn
