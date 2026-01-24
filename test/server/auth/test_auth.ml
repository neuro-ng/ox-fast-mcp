open! Core
open Lwt.Syntax
open Server_auth
open Mcp_server_auth.Provider

[@@@alert "-unsafe_multidomain"]

(* Mock Token Verifier *)
module Mock_verifier : Auth.TOKEN_VERIFIER = struct
  let verify_token token =
    if String.equal token "valid" then
      Lwt.return
        (Some
           {
             token;
             client_id = "test-client";
             scopes = [ "read" ];
             expires_at = None;
             resource = None;
           })
    else Lwt.return None
end

let%expect_test "Remote_auth_provider creation and verification" =
  Lwt_main.run
    (let module Remote_impl = Auth.Remote_auth_provider (Mock_verifier) in
    let module Remote =
      (val Remote_impl.create ~base_url:"https://api.example.com"
             ~authorization_servers:[ "https://auth.example.com" ]
             ())
    in
    (* Test verify_token delegation *)
    let* valid = Remote.verify_token "valid" in
    (match valid with
    | Some t ->
      print_endline "Valid token accepted";
      print_s [%sexp (t.client_id : string)]
    | None -> print_endline "Valid token rejected");

    let* invalid = Remote.verify_token "invalid" in
    (match invalid with
    | Some _ -> print_endline "Invalid token accepted"
    | None -> print_endline "Invalid token rejected");

    (* Test base_url *)
    (match Remote.base_url with
    | Some url -> printf "Base URL: %s\n" url
    | None -> print_endline "No Base URL");

    Lwt.return ());
  [%expect
    {|
    Valid token accepted
    test-client
    Invalid token rejected
    Base URL: https://api.example.com
    |}]

(* Mock OAuth Provider for Functor Test *)
module Mock_oauth_server : OAUTH_AUTHORIZATION_SERVER_PROVIDER = struct
  type authorization_code_t = authorization_code
  type refresh_token_t = refresh_token
  type access_token_t = access_token

  let get_client _ = Lwt.return None
  let register_client _ = Lwt.return_unit
  let authorize _ _ = Lwt.return "http://redirect"
  let load_authorization_code _ _ = Lwt.return None
  let exchange_authorization_code _ _ = failwith "not implemented"
  let load_refresh_token _ _ = Lwt.return None
  let exchange_refresh_token _ _ _ = failwith "not implemented"
  let revoke_token _ = Lwt.return_unit

  let load_access_token token =
    if String.equal token "oauth-valid" then
      Lwt.return
        (Some
           {
             token;
             client_id = "oauth-client";
             scopes = [ "write" ];
             expires_at = None;
             resource = None;
           })
    else Lwt.return None
end

let%expect_test "Make_oauth_provider functor" =
  Lwt_main.run
    (let module OAuth = Auth.Make_oauth_provider (Mock_oauth_server) in
    (* Test verify_token via AUTH_PROVIDER interface *)
    let* valid = OAuth.verify_token "oauth-valid" in
    (match valid with
    | Some t ->
      print_endline "OAuth token accepted";
      print_s [%sexp (t.client_id : string)]
    | None -> print_endline "OAuth token rejected");

    (* Test get_routes stub *)
    let routes = OAuth.get_routes ~mcp_path:None in
    printf "Routes count: %d\n" (List.length routes);

    Lwt.return ());
  [%expect
    {|
    OAuth token accepted
    oauth-client
    Routes count: 0
    |}]
