(** Tests for WorkOS OAuth providers. *)

open! Core
open! Expect_test_helpers_core
open Server_auth_providers.Workos

(* =============================================================================
   Tests for Workos_settings
   ============================================================================= *)

let%expect_test "Workos_settings - create with all options" =
  let settings =
    Workos_settings.create ~client_id:"client_test123"
      ~client_secret:"secret_test456" ~authkit_domain:"https://test.authkit.app"
      ~base_url:"https://myserver.com" ~required_scopes:[ "openid"; "profile" ]
      ()
  in
  printf "client_id: %s\n" (Option.value settings.client_id ~default:"NONE");
  printf "authkit_domain: %s\n"
    (Option.value settings.authkit_domain ~default:"NONE");
  printf "base_url: %s\n" (Option.value settings.base_url ~default:"NONE");
  [%expect
    {|
    client_id: client_test123
    authkit_domain: https://test.authkit.app
    base_url: https://myserver.com
    |}]

let%expect_test "Workos_settings - validate requires client_id" =
  let s =
    Workos_settings.create ~client_secret:"secret"
      ~authkit_domain:"https://test.authkit.app" ()
  in
  (match Workos_settings.validate s with
  | Ok () -> printf "Valid\n"
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {| Error: client_id is required - set via parameter or OXFASTMCP_SERVER_AUTH_WORKOS_CLIENT_ID |}]

let%expect_test "Workos_settings - validate requires client_secret" =
  let s =
    Workos_settings.create ~client_id:"client"
      ~authkit_domain:"https://test.authkit.app" ()
  in
  (match Workos_settings.validate s with
  | Ok () -> printf "Valid\n"
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {| Error: client_secret is required - set via parameter or OXFASTMCP_SERVER_AUTH_WORKOS_CLIENT_SECRET |}]

let%expect_test "Workos_settings - validate requires authkit_domain" =
  let s =
    Workos_settings.create ~client_id:"client" ~client_secret:"secret" ()
  in
  (match Workos_settings.validate s with
  | Ok () -> printf "Valid\n"
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {| Error: authkit_domain is required - set via parameter or OXFASTMCP_SERVER_AUTH_WORKOS_AUTHKIT_DOMAIN |}]

(* =============================================================================
   Tests for Workos_token_verifier
   ============================================================================= *)

let%expect_test "Workos_token_verifier - create with domain" =
  let verifier =
    Workos_token_verifier.create ~authkit_domain:"https://test.authkit.app" ()
  in
  printf "authkit_domain: %s\n" (Workos_token_verifier.authkit_domain verifier);
  printf "userinfo_url: %s\n" (Workos_token_verifier.userinfo_url verifier);
  printf "timeout_seconds: %d\n"
    (Workos_token_verifier.timeout_seconds verifier);
  [%expect
    {|
    authkit_domain: https://test.authkit.app
    userinfo_url: https://test.authkit.app/oauth2/userinfo
    timeout_seconds: 10
    |}]

let%expect_test "Workos_token_verifier - strips trailing slash" =
  let verifier =
    Workos_token_verifier.create ~authkit_domain:"https://test.authkit.app/" ()
  in
  printf "authkit_domain: %s\n" (Workos_token_verifier.authkit_domain verifier);
  [%expect {| authkit_domain: https://test.authkit.app |}]

(* =============================================================================
   Tests for Workos_provider
   ============================================================================= *)

let%expect_test "Workos_provider - create with explicit params" =
  let result =
    Workos_provider.create ~client_id:"client_test123"
      ~client_secret:"secret_test456" ~authkit_domain:"https://test.authkit.app"
      ~base_url:"https://myserver.com" ~required_scopes:[ "openid"; "profile" ]
      ()
  in
  (match result with
  | Ok provider ->
    printf "client_id: %s\n" (Workos_provider.client_id provider);
    printf "authkit_domain: %s\n" (Workos_provider.authkit_domain provider);
    printf "base_url: %s\n"
      (Option.value (Workos_provider.base_url provider) ~default:"NONE")
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {|
    client_id: client_test123
    authkit_domain: https://test.authkit.app
    base_url: https://myserver.com
    |}]

let%expect_test "Workos_provider - adds https prefix" =
  let result =
    Workos_provider.create ~client_id:"test_client" ~client_secret:"test_secret"
      ~authkit_domain:"test.authkit.app" ~base_url:"https://myserver.com" ()
  in
  (match result with
  | Ok provider ->
    printf "authkit_domain: %s\n" (Workos_provider.authkit_domain provider)
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect {| authkit_domain: https://test.authkit.app |}]

let%expect_test "Workos_provider - preserves http prefix" =
  let result =
    Workos_provider.create ~client_id:"test_client" ~client_secret:"test_secret"
      ~authkit_domain:"http://localhost:8080" ~base_url:"https://myserver.com"
      ()
  in
  (match result with
  | Ok provider ->
    printf "authkit_domain: %s\n" (Workos_provider.authkit_domain provider)
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect {| authkit_domain: http://localhost:8080 |}]

let%expect_test "Workos_provider - oauth endpoints configured correctly" =
  let result =
    Workos_provider.create ~client_id:"test_client" ~client_secret:"test_secret"
      ~authkit_domain:"https://test.authkit.app"
      ~base_url:"https://myserver.com" ()
  in
  (match result with
  | Ok provider ->
    printf "authorization_endpoint: %s\n"
      (Workos_provider.upstream_authorization_endpoint provider);
    printf "token_endpoint: %s\n"
      (Workos_provider.upstream_token_endpoint provider)
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {|
    authorization_endpoint: https://test.authkit.app/oauth2/authorize
    token_endpoint: https://test.authkit.app/oauth2/token
    |}]

let%expect_test "Workos_provider - default redirect_path" =
  let result =
    Workos_provider.create ~client_id:"test_client" ~client_secret:"test_secret"
      ~authkit_domain:"https://test.authkit.app" ()
  in
  (match result with
  | Ok provider ->
    printf "redirect_path: %s\n" (Workos_provider.redirect_path provider)
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect {| redirect_path: /auth/callback |}]

let%expect_test "Workos_provider - missing client_id raises error" =
  let result =
    Workos_provider.create ~client_secret:"test_secret"
      ~authkit_domain:"https://test.authkit.app" ()
  in
  (match result with
  | Ok _ -> printf "Unexpected success\n"
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {| Error: client_id is required - set via parameter or OXFASTMCP_SERVER_AUTH_WORKOS_CLIENT_ID |}]

(* =============================================================================
   Tests for Authkit_settings
   ============================================================================= *)

let%expect_test "Authkit_settings - validate requires authkit_domain" =
  let s = Authkit_settings.create ~base_url:"http://x" () in
  (match Authkit_settings.validate s with
  | Ok () -> printf "Valid\n"
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {| Error: authkit_domain is required - set via parameter or OXFASTMCP_SERVER_AUTH_AUTHKITPROVIDER_AUTHKIT_DOMAIN |}]

let%expect_test "Authkit_settings - validate requires base_url" =
  let s =
    Authkit_settings.create ~authkit_domain:"https://test.authkit.app" ()
  in
  (match Authkit_settings.validate s with
  | Ok () -> printf "Valid\n"
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {| Error: base_url is required - set via parameter or OXFASTMCP_SERVER_AUTH_AUTHKITPROVIDER_BASE_URL |}]

(* =============================================================================
   Tests for Authkit_provider
   ============================================================================= *)

let%expect_test "Authkit_provider - create with explicit params" =
  let result =
    Authkit_provider.create
      ~authkit_domain:"https://respectful-lullaby-34-staging.authkit.app"
      ~base_url:"http://localhost:4321" ()
  in
  (match result with
  | Ok provider ->
    printf "authkit_domain: %s\n" (Authkit_provider.authkit_domain provider);
    printf "base_url: %s\n" (Authkit_provider.base_url provider)
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {|
    authkit_domain: https://respectful-lullaby-34-staging.authkit.app
    base_url: http://localhost:4321
    |}]

let%expect_test "Authkit_provider - jwks_uri configured correctly" =
  let result =
    Authkit_provider.create ~authkit_domain:"https://test.authkit.app"
      ~base_url:"https://myserver.com" ()
  in
  (match result with
  | Ok provider ->
    printf "jwks_uri: %s\n" (Authkit_provider.jwks_uri provider);
    printf "issuer: %s\n" (Authkit_provider.issuer provider)
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {|
    jwks_uri: https://test.authkit.app/oauth2/jwks
    issuer: https://test.authkit.app
    |}]

let%expect_test "Authkit_provider - authorization_servers configured correctly"
    =
  let result =
    Authkit_provider.create ~authkit_domain:"https://test.authkit.app"
      ~base_url:"https://myserver.com" ()
  in
  (match result with
  | Ok provider ->
    printf "authorization_servers: %s\n"
      (Authkit_provider.authorization_servers provider
      |> String.concat ~sep:", ")
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect {| authorization_servers: https://test.authkit.app |}]

let%expect_test "Authkit_provider - metadata_url" =
  let result =
    Authkit_provider.create ~authkit_domain:"https://test.authkit.app"
      ~base_url:"http://localhost:4321" ()
  in
  (match result with
  | Ok provider ->
    printf "metadata_url: %s\n" (Authkit_provider.metadata_url provider)
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {| metadata_url: https://test.authkit.app/.well-known/oauth-authorization-server |}]

let%expect_test "Authkit_provider - missing authkit_domain raises error" =
  let result = Authkit_provider.create ~base_url:"https://myserver.com" () in
  (match result with
  | Ok _ -> printf "Unexpected success\n"
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {| Error: authkit_domain is required - set via parameter or OXFASTMCP_SERVER_AUTH_AUTHKITPROVIDER_AUTHKIT_DOMAIN |}]

let%expect_test "Authkit_provider - missing base_url raises error" =
  let result =
    Authkit_provider.create ~authkit_domain:"https://test.authkit.app" ()
  in
  (match result with
  | Ok _ -> printf "Unexpected success\n"
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {| Error: base_url is required - set via parameter or OXFASTMCP_SERVER_AUTH_AUTHKITPROVIDER_BASE_URL |}]
