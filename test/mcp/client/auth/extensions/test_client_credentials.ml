(** OAuth2 Client Credentials Extension Tests

    Tests for RFC 7523 JWT bearer grant implementation.

    Translation from Python test_client_credentials.py *)

open Core
open Async
open Mcp_client_auth_extensions

(** Mock token storage for testing *)
module Mock_token_storage = struct
  type t = {
    mutable tokens : Mcp_shared.Auth.oauth_token option;
    mutable client_info : Mcp_shared.Auth.oauth_client_information_full option;
  }

  let create () = { tokens = None; client_info = None }
  let get_tokens t = return t.tokens

  let set_tokens t tokens =
    t.tokens <- Some tokens;
    return ()

  let get_client_info t = return t.client_info

  let set_client_info t info =
    t.client_info <- Some info;
    return ()
end

(** Create test client metadata *)
let create_test_metadata () =
  Mcp_shared.Auth.
    {
      client_name = Some "Test Client";
      client_uri = Some "https://example.com";
      redirect_uris = [ "http://localhost:3030/callback" ];
      token_endpoint_auth_method = `None;
      grant_types = [ `Authorization_code; `Refresh_token ];
      response_types = [ `Code ];
      scope = Some "read write";
      contacts = None;
      logo_uri = None;
      policy_uri = None;
      tos_uri = None;
      jwks_uri = None;
      jwks = None;
      software_id = None;
      software_version = None;
    }

let%expect_test "create jwt parameters with defaults" =
  let params = Client_credentials.create_jwt_parameters () in
  printf "Algorithm: %s\n"
    (Option.value params.jwt_signing_algorithm ~default:"None");
  printf "Lifetime: %d seconds\n" params.jwt_lifetime_seconds;
  [%expect {|
    Algorithm: RS256
    Lifetime: 300 seconds |}];
  return ()

let%expect_test "create jwt parameters with custom values" =
  let params =
    Client_credentials.create_jwt_parameters ~issuer:"test-issuer"
      ~subject:"test-subject" ~audience:"test-audience"
      ~jwt_signing_algorithm:"HS256" ~jwt_lifetime_seconds:600 ()
  in
  printf "Issuer: %s\n" (Option.value_exn params.issuer);
  printf "Subject: %s\n" (Option.value_exn params.subject);
  printf "Audience: %s\n" (Option.value_exn params.audience);
  printf "Algorithm: %s\n" (Option.value_exn params.jwt_signing_algorithm);
  printf "Lifetime: %d\n" params.jwt_lifetime_seconds;
  [%expect
    {|
    Issuer: test-issuer
    Subject: test-subject
    Audience: test-audience
    Algorithm: HS256
    Lifetime: 600 |}];
  return ()

let%expect_test "to_assertion with predefined JWT" =
  let params =
    Client_credentials.create_jwt_parameters
      ~assertion:"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.test.signature" ()
  in
  let%bind result =
    Client_credentials.to_assertion params ~with_audience_fallback:"fallback"
  in
  (match result with
  | Ok assertion ->
    printf "Assertion: %s\n" assertion;
    [%expect
      {| Assertion: eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.test.signature |}]
  | Error e ->
    printf "Error: %s\n" (Error.to_string_hum e);
    [%expect.unreachable]);
  return ()

let%expect_test "to_assertion without signing key fails" =
  let params =
    Client_credentials.create_jwt_parameters ~issuer:"test" ~subject:"subject"
      ()
  in
  let%bind result =
    Client_credentials.to_assertion params ~with_audience_fallback:"audience"
  in
  (match result with
  | Ok _assertion ->
    printf "Should have failed\n";
    [%expect.unreachable]
  | Error e ->
    printf "Error (expected): %s\n" (Error.to_string_hum e);
    [%expect {| Error (expected): Missing signing key for JWT bearer grant |}]);
  return ()

let%expect_test "create rfc7523 provider" =
  let storage_impl = Mock_token_storage.create () in
  let storage : Mcp_client_auth.Oauth2.storage_wrapper =
    Mcp_client_auth.Oauth2.Storage ((module Mock_token_storage), storage_impl)
  in
  let metadata = create_test_metadata () in
  let _provider =
    Client_credentials.create ~server_url:"https://api.example.com/v1/mcp"
      ~client_metadata:metadata ~storage ()
  in
  printf "RFC 7523 provider created successfully\n";
  [%expect {| RFC 7523 provider created successfully |}];
  return ()

(** NOTE: Full integration tests require:
    1. JWT encoding library (jose/jwto)
    2. HTTP client integration
    3. Mock OAuth server responses

    These tests would verify:
    - JWT generation with custom claims
    - Token exchange request building
    - Client authentication with JWT
    - Full OAuth flow with client credentials grant

    See client_credentials.todo for implementation status. *)

let () =
  Logs.info (fun m ->
      m "Client credentials extension tests - basic functionality only");
  Logs.info (fun m ->
      m "JWT encoding integration tests pending jose/jwto library")
