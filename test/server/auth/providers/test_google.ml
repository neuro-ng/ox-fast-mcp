(** Tests for Google OAuth provider. *)

open! Core
open! Expect_test_helpers_core
open Server_auth_providers.Google

(* =============================================================================
   Tests for Settings
   ============================================================================= *)

let%expect_test "Settings - create with all options" =
  let settings =
    Settings.create ~client_id:"123456789.apps.googleusercontent.com"
      ~client_secret:"GOCSPX-test123" ~base_url:"https://myserver.com"
      ~redirect_path:"/custom/callback"
      ~required_scopes:[ "openid"; "email"; "profile" ]
      ~timeout_seconds:30 ~jwt_signing_key:"test-secret" ()
  in
  printf "client_id: %s\n" (Option.value settings.client_id ~default:"NONE");
  printf "base_url: %s\n" (Option.value settings.base_url ~default:"NONE");
  printf "required_scopes: %s\n"
    (settings.required_scopes |> Option.value ~default:[]
   |> String.concat ~sep:", ");
  [%expect
    {|
    client_id: 123456789.apps.googleusercontent.com
    base_url: https://myserver.com
    required_scopes: openid, email, profile
    |}]

let%expect_test "Settings - validate requires client_id" =
  let s = Settings.create ~client_secret:"secret" () in
  (match Settings.validate s with
  | Ok () -> printf "Valid\n"
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {| Error: client_id is required - set via parameter or OXFASTMCP_SERVER_AUTH_GOOGLE_CLIENT_ID |}]

let%expect_test "Settings - validate requires client_secret" =
  let s =
    Settings.create ~client_id:"123456789.apps.googleusercontent.com" ()
  in
  (match Settings.validate s with
  | Ok () -> printf "Valid\n"
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {| Error: client_secret is required - set via parameter or OXFASTMCP_SERVER_AUTH_GOOGLE_CLIENT_SECRET |}]

let%expect_test "Settings - validate success" =
  let s =
    Settings.create ~client_id:"123456789.apps.googleusercontent.com"
      ~client_secret:"GOCSPX-test123" ()
  in
  (match Settings.validate s with
  | Ok () -> printf "Valid\n"
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect {| Valid |}]

(* =============================================================================
   Tests for Google_token_verifier
   ============================================================================= *)

let%expect_test "Google_token_verifier - create with defaults" =
  let verifier = Google_token_verifier.create () in
  printf "required_scopes: %s\n"
    (Google_token_verifier.required_scopes verifier |> String.concat ~sep:", ");
  printf "timeout_seconds: %d\n"
    (Google_token_verifier.timeout_seconds verifier);
  printf "user_agent: %s\n" Google_token_verifier.user_agent;
  [%expect
    {|
    required_scopes:
    timeout_seconds: 10
    user_agent: OxFastMCP-Google-OAuth
    |}]

let%expect_test "Google_token_verifier - create with custom values" =
  let verifier =
    Google_token_verifier.create ~required_scopes:[ "openid"; "email" ]
      ~timeout_seconds:30 ()
  in
  printf "required_scopes: %s\n"
    (Google_token_verifier.required_scopes verifier |> String.concat ~sep:", ");
  printf "timeout_seconds: %d\n"
    (Google_token_verifier.timeout_seconds verifier);
  [%expect {|
    required_scopes: openid, email
    timeout_seconds: 30
    |}]

(* =============================================================================
   Tests for Google_provider
   ============================================================================= *)

let%expect_test "Google_provider - create with explicit params" =
  let result =
    Google_provider.create ~client_id:"123456789.apps.googleusercontent.com"
      ~client_secret:"GOCSPX-test123" ~base_url:"https://myserver.com"
      ~required_scopes:[ "openid"; "email"; "profile" ]
      ~jwt_signing_key:"test-secret" ()
  in
  (match result with
  | Ok provider ->
    printf "client_id: %s\n" (Google_provider.client_id provider);
    printf "base_url: %s\n"
      (Google_provider.base_url provider |> Option.value ~default:"NONE");
    printf "redirect_path: %s\n" (Google_provider.redirect_path provider);
    printf "required_scopes: %s\n"
      (Google_provider.required_scopes provider |> String.concat ~sep:", ")
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {|
    client_id: 123456789.apps.googleusercontent.com
    base_url: https://myserver.com
    redirect_path: /auth/callback
    required_scopes: openid, email, profile
    |}]

let%expect_test "Google_provider - default values" =
  let result =
    Google_provider.create ~client_id:"123456789.apps.googleusercontent.com"
      ~client_secret:"GOCSPX-test123" ~jwt_signing_key:"test-secret" ()
  in
  (match result with
  | Ok provider ->
    printf "redirect_path: %s\n" (Google_provider.redirect_path provider);
    printf "required_scopes: %s\n"
      (Google_provider.required_scopes provider |> String.concat ~sep:", ");
    printf "timeout_seconds: %d\n" (Google_provider.timeout_seconds provider)
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {|
    redirect_path: /auth/callback
    required_scopes: openid
    timeout_seconds: 10
    |}]

let%expect_test "Google_provider - missing client_id raises error" =
  let result = Google_provider.create ~client_secret:"GOCSPX-test123" () in
  (match result with
  | Ok _ -> printf "Unexpected success\n"
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {| Error: client_id is required - set via parameter or OXFASTMCP_SERVER_AUTH_GOOGLE_CLIENT_ID |}]

let%expect_test "Google_provider - missing client_secret raises error" =
  let result =
    Google_provider.create ~client_id:"123456789.apps.googleusercontent.com" ()
  in
  (match result with
  | Ok _ -> printf "Unexpected success\n"
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {| Error: client_secret is required - set via parameter or OXFASTMCP_SERVER_AUTH_GOOGLE_CLIENT_SECRET |}]

let%expect_test "Google_provider - Google endpoints" =
  printf "authorization_endpoint: %s\n"
    (Google_provider.authorization_endpoint ());
  printf "token_endpoint: %s\n" (Google_provider.token_endpoint ());
  [%expect
    {|
    authorization_endpoint: https://accounts.google.com/o/oauth2/v2/auth
    token_endpoint: https://oauth2.googleapis.com/token
    |}]

let%expect_test "Google_provider - extra_authorize_params defaults" =
  let result =
    Google_provider.create ~client_id:"123456789.apps.googleusercontent.com"
      ~client_secret:"GOCSPX-test123" ~jwt_signing_key:"test-secret" ()
  in
  (match result with
  | Ok provider ->
    let params = Google_provider.extra_authorize_params provider in
    let access_type =
      List.Assoc.find params ~equal:String.equal "access_type"
    in
    let prompt = List.Assoc.find params ~equal:String.equal "prompt" in
    printf "access_type: %s\n" (Option.value access_type ~default:"NONE");
    printf "prompt: %s\n" (Option.value prompt ~default:"NONE")
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect {|
    access_type: offline
    prompt: consent
    |}]

let%expect_test "Google_provider - extra_authorize_params override defaults" =
  let result =
    Google_provider.create ~client_id:"123456789.apps.googleusercontent.com"
      ~client_secret:"GOCSPX-test123" ~jwt_signing_key:"test-secret"
      ~extra_authorize_params:[ ("prompt", "select_account") ]
      ()
  in
  (match result with
  | Ok provider ->
    let params = Google_provider.extra_authorize_params provider in
    let access_type =
      List.Assoc.find params ~equal:String.equal "access_type"
    in
    let prompt = List.Assoc.find params ~equal:String.equal "prompt" in
    printf "access_type: %s\n" (Option.value access_type ~default:"NONE");
    printf "prompt: %s\n" (Option.value prompt ~default:"NONE")
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect {|
    access_type: offline
    prompt: select_account
    |}]

let%expect_test "Google_provider - extra_authorize_params add new params" =
  let result =
    Google_provider.create ~client_id:"123456789.apps.googleusercontent.com"
      ~client_secret:"GOCSPX-test123" ~jwt_signing_key:"test-secret"
      ~extra_authorize_params:[ ("login_hint", "user@example.com") ]
      ()
  in
  (match result with
  | Ok provider ->
    let params = Google_provider.extra_authorize_params provider in
    let access_type =
      List.Assoc.find params ~equal:String.equal "access_type"
    in
    let prompt = List.Assoc.find params ~equal:String.equal "prompt" in
    let login_hint = List.Assoc.find params ~equal:String.equal "login_hint" in
    printf "access_type: %s\n" (Option.value access_type ~default:"NONE");
    printf "prompt: %s\n" (Option.value prompt ~default:"NONE");
    printf "login_hint: %s\n" (Option.value login_hint ~default:"NONE")
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {|
    access_type: offline
    prompt: consent
    login_hint: user@example.com
    |}]

let%expect_test "Google_provider - token_verifier configured correctly" =
  let result =
    Google_provider.create ~client_id:"123456789.apps.googleusercontent.com"
      ~client_secret:"GOCSPX-test123" ~required_scopes:[ "openid"; "email" ]
      ~timeout_seconds:20 ()
  in
  (match result with
  | Ok provider ->
    let verifier = Google_provider.token_verifier provider in
    printf "verifier required_scopes: %s\n"
      (Google_token_verifier.required_scopes verifier |> String.concat ~sep:", ");
    printf "verifier timeout_seconds: %d\n"
      (Google_token_verifier.timeout_seconds verifier)
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {|
    verifier required_scopes: openid, email
    verifier timeout_seconds: 20
    |}]
