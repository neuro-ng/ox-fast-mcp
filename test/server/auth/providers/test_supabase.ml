(** Tests for Supabase Auth provider. *)

open! Core
open! Expect_test_helpers_core
open Server_auth_providers.Supabase

(* =============================================================================
   Tests for Settings
   ============================================================================= *)

let%expect_test "Settings - create with all options" =
  let settings =
    Settings.create ~project_url:"https://abc123.supabase.co"
      ~base_url:"https://myserver.com" ~algorithm:"ES256"
      ~required_scopes:[ "openid"; "email" ] ()
  in
  printf "project_url: %s\n" (Option.value settings.project_url ~default:"NONE");
  printf "base_url: %s\n" (Option.value settings.base_url ~default:"NONE");
  printf "algorithm: %s\n" (Option.value settings.algorithm ~default:"NONE");
  printf "required_scopes: %s\n"
    (settings.required_scopes |> Option.value ~default:[]
   |> String.concat ~sep:", ");
  [%expect
    {|
    project_url: https://abc123.supabase.co
    base_url: https://myserver.com
    algorithm: ES256
    required_scopes: openid, email
    |}]

let%expect_test "Settings - validate requires project_url" =
  let s = Settings.create ~base_url:"http://x" () in
  (match Settings.validate s with
  | Ok () -> printf "Valid\n"
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {| Error: project_url is required - set via parameter or OXFASTMCP_SERVER_AUTH_SUPABASE_PROJECT_URL |}]

let%expect_test "Settings - validate requires base_url" =
  let s = Settings.create ~project_url:"https://abc123.supabase.co" () in
  (match Settings.validate s with
  | Ok () -> printf "Valid\n"
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {| Error: base_url is required - set via parameter or OXFASTMCP_SERVER_AUTH_SUPABASE_BASE_URL |}]

let%expect_test "Settings - validate rejects invalid algorithm" =
  let s =
    Settings.create ~project_url:"https://abc123.supabase.co"
      ~base_url:"http://x" ~algorithm:"INVALID" ()
  in
  (match Settings.validate s with
  | Ok () -> printf "Valid\n"
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {| Error: Unsupported algorithm: INVALID (must be HS256, RS256, or ES256) |}]

let%expect_test "Settings - validate accepts HS256" =
  let s =
    Settings.create ~project_url:"https://abc123.supabase.co"
      ~base_url:"http://x" ~algorithm:"HS256" ()
  in
  (match Settings.validate s with
  | Ok () -> printf "Valid\n"
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect {| Valid |}]

let%expect_test "Settings - validate accepts RS256" =
  let s =
    Settings.create ~project_url:"https://abc123.supabase.co"
      ~base_url:"http://x" ~algorithm:"RS256" ()
  in
  (match Settings.validate s with
  | Ok () -> printf "Valid\n"
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect {| Valid |}]

let%expect_test "Settings - validate accepts ES256" =
  let s =
    Settings.create ~project_url:"https://abc123.supabase.co"
      ~base_url:"http://x" ~algorithm:"ES256" ()
  in
  (match Settings.validate s with
  | Ok () -> printf "Valid\n"
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect {| Valid |}]

(* =============================================================================
   Tests for Supabase_provider
   ============================================================================= *)

let%expect_test "Supabase_provider - create with explicit params" =
  let result =
    Supabase_provider.create ~project_url:"https://abc123.supabase.co"
      ~base_url:"https://myserver.com" ()
  in
  (match result with
  | Ok provider ->
    printf "project_url: %s\n" (Supabase_provider.project_url provider);
    printf "base_url: %s\n" (Supabase_provider.base_url provider);
    printf "algorithm: %s\n" (Supabase_provider.algorithm provider)
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {|
    project_url: https://abc123.supabase.co
    base_url: https://myserver.com
    algorithm: ES256
    |}]

let%expect_test "Supabase_provider - strips trailing slash from project_url" =
  let result =
    Supabase_provider.create ~project_url:"https://abc123.supabase.co/"
      ~base_url:"https://myserver.com" ()
  in
  (match result with
  | Ok provider ->
    printf "project_url: %s\n" (Supabase_provider.project_url provider)
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect {| project_url: https://abc123.supabase.co |}]

let%expect_test "Supabase_provider - default algorithm is ES256" =
  let result =
    Supabase_provider.create ~project_url:"https://abc123.supabase.co"
      ~base_url:"https://myserver.com" ()
  in
  (match result with
  | Ok provider ->
    printf "algorithm: %s\n" (Supabase_provider.algorithm provider)
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect {| algorithm: ES256 |}]

let%expect_test "Supabase_provider - jwks_uri configured correctly" =
  let result =
    Supabase_provider.create ~project_url:"https://abc123.supabase.co"
      ~base_url:"https://myserver.com" ()
  in
  (match result with
  | Ok provider ->
    printf "jwks_uri: %s\n" (Supabase_provider.jwks_uri provider);
    printf "issuer: %s\n" (Supabase_provider.issuer provider)
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {|
    jwks_uri: https://abc123.supabase.co/auth/v1/.well-known/jwks.json
    issuer: https://abc123.supabase.co/auth/v1
    |}]

let%expect_test "Supabase_provider - authorization_servers configured correctly"
    =
  let result =
    Supabase_provider.create ~project_url:"https://abc123.supabase.co"
      ~base_url:"https://myserver.com" ()
  in
  (match result with
  | Ok provider ->
    printf "authorization_servers: %s\n"
      (Supabase_provider.authorization_servers provider
      |> String.concat ~sep:", ")
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect {| authorization_servers: https://abc123.supabase.co/auth/v1 |}]

let%expect_test "Supabase_provider - with required_scopes" =
  let result =
    Supabase_provider.create ~project_url:"https://abc123.supabase.co"
      ~base_url:"https://myserver.com" ~required_scopes:[ "openid"; "email" ] ()
  in
  (match result with
  | Ok provider ->
    printf "required_scopes: %s\n"
      (Supabase_provider.required_scopes provider |> String.concat ~sep:", ")
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect {| required_scopes: openid, email |}]

let%expect_test "Supabase_provider - with custom algorithm HS256" =
  let result =
    Supabase_provider.create ~project_url:"https://abc123.supabase.co"
      ~base_url:"https://myserver.com" ~algorithm:"HS256" ()
  in
  (match result with
  | Ok provider ->
    printf "algorithm: %s\n" (Supabase_provider.algorithm provider)
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect {| algorithm: HS256 |}]

let%expect_test "Supabase_provider - with custom algorithm RS256" =
  let result =
    Supabase_provider.create ~project_url:"https://abc123.supabase.co"
      ~base_url:"https://myserver.com" ~algorithm:"RS256" ()
  in
  (match result with
  | Ok provider ->
    printf "algorithm: %s\n" (Supabase_provider.algorithm provider)
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect {| algorithm: RS256 |}]

let%expect_test "Supabase_provider - metadata_url" =
  let result =
    Supabase_provider.create ~project_url:"https://test123.supabase.co"
      ~base_url:"http://localhost:4321" ()
  in
  (match result with
  | Ok provider ->
    printf "metadata_url: %s\n" (Supabase_provider.metadata_url provider)
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {| metadata_url: https://test123.supabase.co/auth/v1/.well-known/oauth-authorization-server |}]

let%expect_test "Supabase_provider - missing project_url raises error" =
  let result = Supabase_provider.create ~base_url:"https://myserver.com" () in
  (match result with
  | Ok _ -> printf "Unexpected success\n"
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {| Error: project_url is required - set via parameter or OXFASTMCP_SERVER_AUTH_SUPABASE_PROJECT_URL |}]

let%expect_test "Supabase_provider - missing base_url raises error" =
  let result =
    Supabase_provider.create ~project_url:"https://abc123.supabase.co" ()
  in
  (match result with
  | Ok _ -> printf "Unexpected success\n"
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {| Error: base_url is required - set via parameter or OXFASTMCP_SERVER_AUTH_SUPABASE_BASE_URL |}]
