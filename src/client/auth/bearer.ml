open! Core
open! Async

(* Logger for bearer authentication *)
let logger = Logging.Logger.get_logger "Client.Auth.Bearer"

(* BearerAuth provides HTTP Bearer token authentication

   This is analogous to Python's httpx.Auth interface, but adapted for use with
   Cohttp/Async in OCaml. *)
module BearerAuth = struct
  type t = { token : Secret_string.t } [@@deriving sexp, compare]

  let create token =
    let bearer_auth = { token = Secret_string.create token } in
    Logging.Logger.debug logger "Created BearerAuth instance";
    bearer_auth

  (* Apply bearer authentication to a request by adding the Authorization header

     In Python httpx, auth_flow is a generator that yields modified requests. In
     OCaml with Cohttp, we directly modify headers before sending the request.

     @param t The BearerAuth instance containing the token @param headers The
     existing headers to modify @return Updated headers with Authorization
     header added *)
  let apply_auth t headers =
    let token_value = Secret_string.get_secret_value t.token in
    let auth_value = sprintf "Bearer %s" token_value in
    Logging.Logger.debug logger "Applying bearer authentication to request";
    Cohttp.Header.add headers "Authorization" auth_value

  (* Get the token value

     This provides access to the token for debugging or custom use cases. Note:
     In production, tokens should be treated as secrets and logged carefully. *)
  let get_token t = Secret_string.get_secret_value t.token
end
