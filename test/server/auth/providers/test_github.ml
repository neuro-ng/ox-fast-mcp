(** Tests for GitHub OAuth provider. *)

open! Core
open! Expect_test_helpers_core
open Server_auth_providers.Github

(* =============================================================================
   Tests for Settings
   ============================================================================= *)

let%expect_test "Settings - create with all options" =
  let settings =
    Settings.create ~client_id:"test_client" ~client_secret:"test_secret"
      ~base_url:"https://example.com" ~redirect_path:"/custom/callback"
      ~required_scopes:[ "user"; "repo" ] ~timeout_seconds:30
      ~jwt_signing_key:"test-secret" ()
  in
  printf "client_id: %s\n" (Option.value settings.client_id ~default:"NONE");
  printf "client_secret: %s\n"
    (Option.value settings.client_secret ~default:"NONE");
  printf "base_url: %s\n" (Option.value settings.base_url ~default:"NONE");
  printf "redirect_path: %s\n"
    (Option.value settings.redirect_path ~default:"NONE");
  printf "required_scopes: %s\n"
    (settings.required_scopes |> Option.value ~default:[]
   |> String.concat ~sep:", ");
  printf "timeout_seconds: %s\n"
    (settings.timeout_seconds
    |> Option.value_map ~default:"NONE" ~f:Int.to_string);
  [%expect
    {|
    client_id: test_client
    client_secret: test_secret
    base_url: https://example.com
    redirect_path: /custom/callback
    required_scopes: user, repo
    timeout_seconds: 30
    |}]

let%expect_test "Settings - merge prioritizes first non-None" =
  let s1 =
    Settings.create ~client_id:"explicit_client"
      ~client_secret:"explicit_secret" ()
  in
  let s2 =
    Settings.create ~client_id:"env_client" ~client_secret:"env_secret"
      ~base_url:"https://env.com" ()
  in
  let merged = Settings.merge s1 s2 in
  printf "client_id: %s\n" (Option.value merged.client_id ~default:"NONE");
  printf "base_url: %s\n" (Option.value merged.base_url ~default:"NONE");
  [%expect
    {|
    client_id: explicit_client
    base_url: https://env.com
    |}]

let%expect_test "Settings - validate requires client_id" =
  let s = Settings.create ~client_secret:"secret" () in
  (match Settings.validate s with
  | Ok () -> printf "Valid\n"
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {| Error: client_id is required - set via parameter or OXFASTMCP_SERVER_AUTH_GITHUB_CLIENT_ID |}]

let%expect_test "Settings - validate requires client_secret" =
  let s = Settings.create ~client_id:"test_client" () in
  (match Settings.validate s with
  | Ok () -> printf "Valid\n"
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {| Error: client_secret is required - set via parameter or OXFASTMCP_SERVER_AUTH_GITHUB_CLIENT_SECRET |}]

let%expect_test "Settings - validate success" =
  let s = Settings.create ~client_id:"test" ~client_secret:"secret" () in
  (match Settings.validate s with
  | Ok () -> printf "Valid\n"
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect {| Valid |}]

(* =============================================================================
   Tests for Github_token_verifier
   ============================================================================= *)

let%expect_test "Github_token_verifier - create with defaults" =
  let verifier = Github_token_verifier.create () in
  printf "required_scopes: %s\n"
    (Github_token_verifier.required_scopes verifier |> String.concat ~sep:", ");
  printf "timeout_seconds: %d\n"
    (Github_token_verifier.timeout_seconds verifier);
  printf "user_agent: %s\n" Github_token_verifier.user_agent;
  [%expect
    {|
    required_scopes:
    timeout_seconds: 10
    user_agent: OxFastMCP-GitHub-OAuth
    |}]

let%expect_test "Github_token_verifier - create with custom values" =
  let verifier =
    Github_token_verifier.create ~required_scopes:[ "user"; "repo" ]
      ~timeout_seconds:30 ()
  in
  printf "required_scopes: %s\n"
    (Github_token_verifier.required_scopes verifier |> String.concat ~sep:", ");
  printf "timeout_seconds: %d\n"
    (Github_token_verifier.timeout_seconds verifier);
  [%expect {|
    required_scopes: user, repo
    timeout_seconds: 30
    |}]

(* =============================================================================
   Tests for Github_provider
   ============================================================================= *)

let%expect_test "Github_provider - create with explicit params" =
  let result =
    Github_provider.create ~client_id:"test_client" ~client_secret:"test_secret"
      ~base_url:"https://example.com" ~redirect_path:"/custom/callback"
      ~required_scopes:[ "user"; "repo" ] ~timeout_seconds:30
      ~jwt_signing_key:"test-secret" ()
  in
  (match result with
  | Ok provider ->
    printf "client_id: %s\n" (Github_provider.client_id provider);
    printf "base_url: %s\n"
      (Github_provider.base_url provider |> Option.value ~default:"NONE");
    printf "redirect_path: %s\n" (Github_provider.redirect_path provider);
    printf "required_scopes: %s\n"
      (Github_provider.required_scopes provider |> String.concat ~sep:", ");
    printf "timeout_seconds: %d\n" (Github_provider.timeout_seconds provider)
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {|
    client_id: test_client
    base_url: https://example.com
    redirect_path: /custom/callback
    required_scopes: user, repo
    timeout_seconds: 30
    |}]

let%expect_test "Github_provider - default values" =
  let result =
    Github_provider.create ~client_id:"test_client" ~client_secret:"test_secret"
      ~jwt_signing_key:"test-secret" ()
  in
  (match result with
  | Ok provider ->
    printf "redirect_path: %s\n" (Github_provider.redirect_path provider);
    printf "required_scopes: %s\n"
      (Github_provider.required_scopes provider |> String.concat ~sep:", ");
    printf "timeout_seconds: %d\n" (Github_provider.timeout_seconds provider)
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {|
    redirect_path: /auth/callback
    required_scopes: user
    timeout_seconds: 10
    |}]

let%expect_test "Github_provider - missing client_id raises error" =
  let result = Github_provider.create ~client_secret:"test_secret" () in
  (match result with
  | Ok _ -> printf "Unexpected success\n"
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {| Error: client_id is required - set via parameter or OXFASTMCP_SERVER_AUTH_GITHUB_CLIENT_ID |}]

let%expect_test "Github_provider - missing client_secret raises error" =
  let result = Github_provider.create ~client_id:"test_client" () in
  (match result with
  | Ok _ -> printf "Unexpected success\n"
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {| Error: client_secret is required - set via parameter or OXFASTMCP_SERVER_AUTH_GITHUB_CLIENT_SECRET |}]

let%expect_test "Github_provider - issuer_url defaults to base_url" =
  let result =
    Github_provider.create ~client_id:"test" ~client_secret:"secret"
      ~base_url:"https://example.com" ()
  in
  (match result with
  | Ok provider ->
    printf "issuer_url: %s\n"
      (Github_provider.issuer_url provider |> Option.value ~default:"NONE")
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect {| issuer_url: https://example.com |}]

let%expect_test "Github_provider - explicit issuer_url overrides base_url" =
  let result =
    Github_provider.create ~client_id:"test" ~client_secret:"secret"
      ~base_url:"https://base.com" ~issuer_url:"https://issuer.com" ()
  in
  (match result with
  | Ok provider ->
    printf "issuer_url: %s\n"
      (Github_provider.issuer_url provider |> Option.value ~default:"NONE")
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect {| issuer_url: https://issuer.com |}]

let%expect_test "Github_provider - GitHub endpoints" =
  printf "authorization_endpoint: %s\n"
    (Github_provider.authorization_endpoint ());
  printf "token_endpoint: %s\n" (Github_provider.token_endpoint ());
  [%expect
    {|
    authorization_endpoint: https://github.com/login/oauth/authorize
    token_endpoint: https://github.com/login/oauth/access_token
    |}]

let%expect_test "Github_provider - token_verifier settings" =
  let result =
    Github_provider.create ~client_id:"test" ~client_secret:"secret"
      ~required_scopes:[ "user"; "repo" ] ~timeout_seconds:20 ()
  in
  (match result with
  | Ok provider ->
    let verifier = Github_provider.token_verifier provider in
    printf "verifier required_scopes: %s\n"
      (Github_token_verifier.required_scopes verifier |> String.concat ~sep:", ");
    printf "verifier timeout_seconds: %d\n"
      (Github_token_verifier.timeout_seconds verifier)
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {|
    verifier required_scopes: user, repo
    verifier timeout_seconds: 20
    |}]
