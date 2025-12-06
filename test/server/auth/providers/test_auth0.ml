open! Core
open! Async
open! Expect_test_helpers_core
open Server_auth_providers.Auth0

[@@@alert "-unsafe_multidomain"]

let%expect_test "test settings from env vars" =
  (* Setup environment *)
  Core_unix.putenv ~key:"FASTMCP_SERVER_AUTH_AUTH0_CONFIG_URL"
    ~data:"https://example.com/.well-known/openid-configuration";
  Core_unix.putenv ~key:"FASTMCP_SERVER_AUTH_AUTH0_CLIENT_ID"
    ~data:"test-client-id";
  Core_unix.putenv ~key:"FASTMCP_SERVER_AUTH_AUTH0_CLIENT_SECRET"
    ~data:"test-client-secret";
  Core_unix.putenv ~key:"FASTMCP_SERVER_AUTH_AUTH0_AUDIENCE"
    ~data:"test-audience";
  Core_unix.putenv ~key:"FASTMCP_SERVER_AUTH_AUTH0_BASE_URL"
    ~data:"https://example.com:8000/";
  Core_unix.putenv ~key:"FASTMCP_SERVER_AUTH_AUTH0_REDIRECT_PATH"
    ~data:"/test/callback";
  Core_unix.putenv ~key:"FASTMCP_SERVER_AUTH_AUTH0_REQUIRED_SCOPES"
    ~data:"openid,email";
  Core_unix.putenv ~key:"FASTMCP_SERVER_AUTH_AUTH0_JWT_SIGNING_KEY"
    ~data:"test-secret";

  let settings = Settings.load_from_env () in

  print_s [%sexp (settings : Settings.t)];
  [%expect
    {|
    ((config_url (https://example.com/.well-known/openid-configuration))
     (client_id     (test-client-id))
     (client_secret (test-client-secret))
     (audience      (test-audience))
     (base_url      (https://example.com:8000/))
     (issuer_url ())
     (redirect_path (/test/callback))
     (required_scopes ((openid email)))
     (allowed_client_redirect_uris ())
     (jwt_signing_key (test-secret)))
    |}];

  (* Cleanup *)
  Core_unix.unsetenv "FASTMCP_SERVER_AUTH_AUTH0_CONFIG_URL";
  Core_unix.unsetenv "FASTMCP_SERVER_AUTH_AUTH0_CLIENT_ID";
  return ()

let%expect_test "test settings explicit override env" =
  Core_unix.putenv ~key:"FASTMCP_SERVER_AUTH_AUTH0_CLIENT_ID"
    ~data:"env-client-id";

  let env_settings = Settings.load_from_env () in
  let explicit_settings = Settings.create ~client_id:"explicit-client-id" () in

  print_s [%sexp (explicit_settings.client_id : string option)];
  [%expect {| (explicit-client-id) |}];
  print_s [%sexp (env_settings.client_id : string option)];
  [%expect {| (env-client-id) |}];

  Core_unix.unsetenv "FASTMCP_SERVER_AUTH_AUTH0_CLIENT_ID";
  return ()

let%expect_test "test init missing config_url raises error" =
  let settings =
    Settings.create ~client_id:"id" ~client_secret:"secret" ~audience:"aud"
      ~base_url:"url" ()
  in
  let result = Settings.validate settings in
  match result with
  | Ok _ ->
    print_string "Unexpected success";
    return ()
  | Error e ->
    print_s [%sexp (Error.to_string_hum e : string)];
    [%expect
      {| "config_url is required - set via parameter or FASTMCP_SERVER_AUTH_AUTH0_CONFIG_URL" |}];
    return ()

let%expect_test "test init defaults" =
  let settings =
    Settings.create ~config_url:"url" ~client_id:"id" ~client_secret:"secret"
      ~audience:"aud" ~base_url:"base" ()
  in
  (match Settings.validate settings with
  | Ok () ->
    print_endline "Validation successful"
    (* Default scopes and redirect handled in create/load logic or
       Provider.create *)
  | Error e -> print_s [%sexp (e : Error.t)]);
  [%expect {| Validation successful |}];
  return ()

let%expect_test "test provider creation success" =
  let provider =
    Auth0_provider.create ~config_url:"https://config" ~client_id:"id"
      ~client_secret:"secret" ~audience:"aud" ~base_url:"https://base" ()
  in
  (match provider with
  | Ok _ -> print_string "Provider created successfully"
  | Error e -> print_s [%sexp (e : Error.t)]);
  [%expect {| Provider created successfully |}];
  return ()
