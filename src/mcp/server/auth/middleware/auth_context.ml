open Core
open Lwt
open Cohttp
open Bearer_auth

(** Auth context key for Lwt local storage *)
let auth_context_key : authenticated_user Lwt.key = Lwt.new_key ()

(** Get the access token from the current context *)
let get_access_token () =
  match Lwt.get auth_context_key with
  | None -> Lwt.return_none
  | Some user -> Lwt.return (Some user.access_token)

module Auth_context_middleware = struct
  let create () = ()

  let handle
      (auth_result : Bearer_auth.auth_result option)
      (_req : Cohttp.Request.t)
      (next : (Cohttp.Response.t * Cohttp.Body.t) Lwt.t)
      : (Cohttp.Response.t * Cohttp.Body.t) Lwt.t =
    match auth_result with
    | None -> next
    | Some None -> next
    | Some (Some (_auth_creds, auth_user)) ->
      (* Set the authenticated user in Lwt local storage *)
      Lwt.with_value auth_context_key auth_user (fun () -> next)
end
