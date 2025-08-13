open! Core
open! Expect_test_helpers_core

module Provider = Ox_fast_mcp.Mcp.Server.Auth.Provider
module Bearer = Ox_fast_mcp.Mcp.Server.Auth.Middleware.Bearer_auth
module Ctx = Ox_fast_mcp.Mcp.Server.Auth.Middleware.Auth_context

let dummy_token : Provider.access_token =
  { token = "tkn"; client_id = "cid"; scopes = [ "s1" ]; expires_at = None; resource = None }

let dummy_user : Bearer.authenticated_user =
  { client_id = "cid"; access_token = dummy_token; scopes = [ "s1" ] }

let%expect_test "get_access_token returns None when unset" =
  let tok = Lwt_main.run (Ctx.get_access_token ()) in
  require ~here:[%here] (Option.is_none tok)

let%expect_test "get_access_token returns token when set in context via middleware handle" =
  let open Lwt in
  let next = return (Cohttp.Response.make (), Cohttp.Body.empty) in
  let run =
    Ctx.Auth_context_middleware.handle
      (Some (({ scopes = [ "s1" ] }, dummy_user)))
      (Cohttp.Request.make (Uri.of_string "/"))
      next
  in
  let _resp = Lwt_main.run run in
  let tok = Lwt_main.run (Ctx.get_access_token ()) in
  require_equal ~here:[%here]
    (module struct
      type t = Provider.access_token option [@@deriving compare, sexp_of, equal]
    end)
    tok (Some dummy_token)