open! Core

(* open Lwt.Syntax *)
open Server_auth

[@@@alert "-unsafe_multidomain"]

(* Test KDF *)
let%expect_test "derive_jwt_key_produces_valid_base64_result" =
  let key =
    Jwt_issuer.derive_jwt_key ~high_entropy_material:"test-secret"
      ~salt:"test-salt" ()
  in
  let decoded = Base64.decode_exn ~alphabet:Base64.uri_safe_alphabet key in
  printf "Length: %d\n" (String.length decoded);
  [%expect {| Length: 32 |}];

  let key2 =
    Jwt_issuer.derive_jwt_key ~low_entropy_material:"test-secret"
      ~salt:"test-salt" ()
  in
  let decoded2 = Base64.decode_exn ~alphabet:Base64.uri_safe_alphabet key2 in
  printf "Length: %d\n" (String.length decoded2);
  [%expect {| Length: 32 |}]

(* Test JWTIssuer *)

let%expect_test "issue_and_verify_access_token" =
  (* Setup *)
  let signing_key =
    Jwt_issuer.derive_jwt_key ~high_entropy_material:"secret" ~salt:"salt" ()
  in
  let issuer =
    Jwt_issuer.JWTIssuer.create ~issuer:"https://test.com"
      ~audience:"https://test.com/mcp" ~signing_key
  in

  (* Issue *)
  let token =
    Jwt_issuer.JWTIssuer.issue_access_token issuer ~client_id:"client-123"
      ~scopes:[ "read"; "write" ] ~jti:"jti-123" ()
  in

  (* Verify *)
  let payload = Jwt_issuer.JWTIssuer.verify_token issuer token in
  let open Yojson.Safe.Util in
  printf "Client: %s\n" (payload |> member "client_id" |> to_string);
  printf "Scope: %s\n" (payload |> member "scope" |> to_string);
  printf "JTI: %s\n" (payload |> member "jti" |> to_string);

  [%expect
    {|
    Client: client-123
    Scope: read write
    JTI: jti-123
  |}]

let%expect_test "issue_refresh_token" =
  let signing_key =
    Jwt_issuer.derive_jwt_key ~high_entropy_material:"secret" ~salt:"salt" ()
  in
  let issuer =
    Jwt_issuer.JWTIssuer.create ~issuer:"https://test.com"
      ~audience:"https://test.com/mcp" ~signing_key
  in

  let token =
    Jwt_issuer.JWTIssuer.issue_refresh_token issuer ~client_id:"client-123"
      ~scopes:[ "read" ] ~jti:"refresh-123" ~expires_in:3600
  in

  let payload = Jwt_issuer.JWTIssuer.verify_token issuer token in
  let open Yojson.Safe.Util in
  printf "Token Use: %s\n" (payload |> member "token_use" |> to_string);

  [%expect {|
    Token Use: refresh
  |}]

let%expect_test "verify_token_validates_issuer" =
  let signing_key =
    Jwt_issuer.derive_jwt_key ~high_entropy_material:"secret" ~salt:"salt" ()
  in
  let issuer =
    Jwt_issuer.JWTIssuer.create ~issuer:"https://test.com"
      ~audience:"https://test.com/mcp" ~signing_key
  in
  let token =
    Jwt_issuer.JWTIssuer.issue_access_token issuer ~client_id:"id" ~scopes:[]
      ~jti:"id" ()
  in

  let bad_issuer =
    Jwt_issuer.JWTIssuer.create ~issuer:"https://evil.com"
      ~audience:"https://test.com/mcp" ~signing_key
  in

  (try
     ignore (Jwt_issuer.JWTIssuer.verify_token bad_issuer token);
     print_endline "Should fail"
   with Failure msg -> printf "Failed: %s\n" msg);

  [%expect {| Failed: Invalid token issuer |}]
