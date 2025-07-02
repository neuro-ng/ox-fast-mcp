open Core
open Lwt
open Cohttp
open Bearer_auth

(** Auth context key for Lwt local storage *)
let auth_context_key : authenticated_user option Lwt.key =
  Lwt.new_key ()

(** Get the access token from the current context *)
let get_access_token () =
  Lwt.with_value auth_context_key None (fun () ->
    match Lwt.get auth_context_key with
    | None -> Lwt.return None
    | Some user -> Lwt.return (Some user)
  )

module Auth_context_middleware = struct
  let create () = ()

  let handle auth_result _req next =
    match auth_result with
    | None -> next
    | Some (_, auth_user) ->
      (* Set the authenticated user in Lwt local storage *)
      Lwt.with_value auth_context_key (Some auth_user) (fun () ->
        next
      )
end 