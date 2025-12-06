open! Core
open! Async
open! Expect_test_helpers_core
open Server_auth_providers.Aws

[@@@alert "-unsafe_multidomain"]

let%expect_test "test settings from env vars" =
  Core_unix.putenv ~key:"FASTMCP_SERVER_AUTH_AWS_COGNITO_USER_POOL_ID"
    ~data:"us-east-1_XXXXXXXXX";
  Core_unix.putenv ~key:"FASTMCP_SERVER_AUTH_AWS_COGNITO_AWS_REGION"
    ~data:"us-east-1";
  Core_unix.putenv ~key:"FASTMCP_SERVER_AUTH_AWS_COGNITO_CLIENT_ID"
    ~data:"env_client_id";
  Core_unix.putenv ~key:"FASTMCP_SERVER_AUTH_AWS_COGNITO_CLIENT_SECRET"
    ~data:"env_secret";
  Core_unix.putenv ~key:"FASTMCP_SERVER_AUTH_AWS_COGNITO_BASE_URL"
    ~data:"https://example.com";
  Core_unix.putenv ~key:"FASTMCP_SERVER_AUTH_AWS_COGNITO_REDIRECT_PATH"
    ~data:"/custom/callback";

  let settings = Settings.load_from_env () in

  print_s [%sexp (settings : Settings.t)];
  [%expect
    {|
    ((user_pool_id  (us-east-1_XXXXXXXXX))
     (aws_region    (us-east-1))
     (client_id     (env_client_id))
     (client_secret (env_secret))
     (base_url      (https://example.com))
     (issuer_url ())
     (redirect_path (/custom/callback))
     (required_scopes              ())
     (allowed_client_redirect_uris ())
     (jwt_signing_key              ()))
    |}];

  Core_unix.unsetenv "FASTMCP_SERVER_AUTH_AWS_COGNITO_USER_POOL_ID";
  Core_unix.unsetenv "FASTMCP_SERVER_AUTH_AWS_COGNITO_AWS_REGION";
  Core_unix.unsetenv "FASTMCP_SERVER_AUTH_AWS_COGNITO_CLIENT_ID";
  Core_unix.unsetenv "FASTMCP_SERVER_AUTH_AWS_COGNITO_CLIENT_SECRET";
  Core_unix.unsetenv "FASTMCP_SERVER_AUTH_AWS_COGNITO_BASE_URL";
  Core_unix.unsetenv "FASTMCP_SERVER_AUTH_AWS_COGNITO_REDIRECT_PATH";
  return ()

let%expect_test "test settings explicit override env" =
  Core_unix.putenv ~key:"FASTMCP_SERVER_AUTH_AWS_COGNITO_USER_POOL_ID"
    ~data:"env_pool_id";
  Core_unix.putenv ~key:"FASTMCP_SERVER_AUTH_AWS_COGNITO_CLIENT_ID"
    ~data:"env_client_id";
  Core_unix.putenv ~key:"FASTMCP_SERVER_AUTH_AWS_COGNITO_CLIENT_SECRET"
    ~data:"env_secret";

  let explicit_settings =
    Settings.create ~user_pool_id:"explicit_pool_id"
      ~client_id:"explicit_client_id" ~client_secret:"explicit_secret" ()
  in

  print_s [%sexp (explicit_settings.user_pool_id : string option)];
  [%expect {| (explicit_pool_id) |}];

  Core_unix.unsetenv "FASTMCP_SERVER_AUTH_AWS_COGNITO_USER_POOL_ID";
  Core_unix.unsetenv "FASTMCP_SERVER_AUTH_AWS_COGNITO_CLIENT_ID";
  Core_unix.unsetenv "FASTMCP_SERVER_AUTH_AWS_COGNITO_CLIENT_SECRET";
  return ()

let%expect_test "test init missing user_pool_id raises error" =
  let settings = Settings.create ~client_id:"id" ~client_secret:"secret" () in
  let result = Settings.validate settings in
  match result with
  | Ok _ ->
    print_string "Unexpected success";
    return ()
  | Error e ->
    print_s [%sexp (Error.to_string_hum e : string)];
    [%expect
      {| "user_pool_id is required - set via parameter or FASTMCP_SERVER_AUTH_AWS_COGNITO_USER_POOL_ID" |}];
    return ()

let%expect_test "test init defaults and config url construction" =
  let provider_result =
    Aws_cognito_provider.create ~user_pool_id:"us-east-1_XXXXXXXXX"
      ~client_id:"test_default" ~client_secret:"secret" ()
  in
  match provider_result with
  | Error e ->
    print_s [%sexp (e : Error.t)];
    return ()
  | Ok provider ->
    print_endline ("Config URL: " ^ provider.config_url);
    print_s [%sexp (provider.settings.aws_region : string option)];
    [%expect
      {|
        Config URL: https://cognito-idp.eu-central-1.amazonaws.com/us-east-1_XXXXXXXXX/.well-known/openid-configuration
        ()
        |}];
    return ()

let%expect_test "test init with custom region" =
  let provider_result =
    Aws_cognito_provider.create ~user_pool_id:"us-west-2_YYYY"
      ~aws_region:"us-west-2" ~client_id:"test" ~client_secret:"secret" ()
  in
  match provider_result with
  | Error e ->
    print_s [%sexp (e : Error.t)];
    return ()
  | Ok provider ->
    print_endline ("Config URL: " ^ provider.config_url);
    [%expect
      {| Config URL: https://cognito-idp.us-west-2.amazonaws.com/us-west-2_YYYY/.well-known/openid-configuration |}];
    return ()
