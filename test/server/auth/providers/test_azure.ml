open! Core
open! Async
open! Expect_test_helpers_core
open Server_auth_providers.Azure

[@@@alert "-unsafe_multidomain"]

let%expect_test "test settings from env vars" =
  Core_unix.putenv ~key:"FASTMCP_SERVER_AUTH_AZURE_CLIENT_ID"
    ~data:"env-client-id";
  Core_unix.putenv ~key:"FASTMCP_SERVER_AUTH_AZURE_CLIENT_SECRET"
    ~data:"env-secret";
  Core_unix.putenv ~key:"FASTMCP_SERVER_AUTH_AZURE_TENANT_ID"
    ~data:"env-tenant-id";
  Core_unix.putenv ~key:"FASTMCP_SERVER_AUTH_AZURE_BASE_URL"
    ~data:"https://envserver.com";
  Core_unix.putenv ~key:"FASTMCP_SERVER_AUTH_AZURE_REQUIRED_SCOPES"
    ~data:"read,write";
  Core_unix.putenv ~key:"FASTMCP_SERVER_AUTH_AZURE_JWT_SIGNING_KEY"
    ~data:"test-secret";

  let settings = Settings.load_from_env () in

  print_s [%sexp (settings : Settings.t)];
  [%expect
    {|
    ((client_id     (env-client-id))
     (client_secret (env-secret))
     (tenant_id     (env-tenant-id))
     (identifier_uri ())
     (base_url (https://envserver.com))
     (issuer_url    ())
     (redirect_path ())
     (required_scopes ((read write)))
     (additional_authorize_scopes  ())
     (allowed_client_redirect_uris ())
     (jwt_signing_key (test-secret))
     (base_authority ()))
    |}];

  Core_unix.unsetenv "FASTMCP_SERVER_AUTH_AZURE_CLIENT_ID";
  Core_unix.unsetenv "FASTMCP_SERVER_AUTH_AZURE_CLIENT_SECRET";
  Core_unix.unsetenv "FASTMCP_SERVER_AUTH_AZURE_TENANT_ID";
  Core_unix.unsetenv "FASTMCP_SERVER_AUTH_AZURE_BASE_URL";
  Core_unix.unsetenv "FASTMCP_SERVER_AUTH_AZURE_REQUIRED_SCOPES";
  Core_unix.unsetenv "FASTMCP_SERVER_AUTH_AZURE_JWT_SIGNING_KEY";
  return ()

let%expect_test "test init explicit overrides env" =
  Core_unix.putenv ~key:"FASTMCP_SERVER_AUTH_AZURE_CLIENT_ID"
    ~data:"env_client_id";

  let explicit_settings = Settings.create ~client_id:"explicit_client_id" () in

  print_s [%sexp (explicit_settings.client_id : string option)];
  [%expect {| (explicit_client_id) |}];

  Core_unix.unsetenv "FASTMCP_SERVER_AUTH_AZURE_CLIENT_ID";
  return ()

let%expect_test "test init missing client_id raises error" =
  let settings =
    Settings.create ~client_secret:"secret" ~tenant_id:"tid"
      ~required_scopes:[ "read" ] ()
  in
  match Settings.validate settings with
  | Ok _ ->
    print_string "Unexpected success";
    return ()
  | Error e ->
    print_s [%sexp (Error.to_string_hum e : string)];
    [%expect
      {| "client_id is required - set via parameter or FASTMCP_SERVER_AUTH_AZURE_CLIENT_ID" |}];
    return ()

let%expect_test "test init missing tenant_id raises error" =
  let settings =
    Settings.create ~client_id:"id" ~client_secret:"secret"
      ~required_scopes:[ "read" ] ()
  in
  match Settings.validate settings with
  | Ok _ ->
    print_string "Unexpected success";
    return ()
  | Error e ->
    print_s [%sexp (Error.to_string_hum e : string)];
    [%expect
      {| "tenant_id is required - set via parameter or FASTMCP_SERVER_AUTH_AZURE_TENANT_ID. Use your Azure tenant ID (found in Azure Portal), 'organizations', or 'consumers'" |}];
    return ()

let%expect_test "test init missing required_scopes raises error" =
  let settings =
    Settings.create ~client_id:"id" ~client_secret:"secret" ~tenant_id:"tid" ()
  in
  match Settings.validate settings with
  | Ok _ ->
    print_string "Unexpected success";
    return ()
  | Error e ->
    print_s [%sexp (Error.to_string_hum e : string)];
    [%expect
      {| "required_scopes must include at least one scope - set via parameter or FASTMCP_SERVER_AUTH_AZURE_REQUIRED_SCOPES. Azure's OAuth API requires the 'scope' parameter in authorization requests. Use the unprefixed scope names from your Azure App registration (e.g., ['read', 'write'])" |}];
    return ()

let%expect_test "test endpoints configured correctly" =
  let provider_result =
    Azure_provider.create ~client_id:"test-client" ~client_secret:"secret"
      ~tenant_id:"my-tenant-id" ~required_scopes:[ "read" ] ()
  in
  match provider_result with
  | Error e ->
    print_s [%sexp (e : Error.t)];
    return ()
  | Ok provider ->
    print_endline ("Auth endpoint: " ^ provider.authorization_endpoint);
    print_endline ("Token endpoint: " ^ provider.token_endpoint);
    print_endline ("Issuer: " ^ provider.issuer);
    print_endline ("JWKS URI: " ^ provider.jwks_uri);
    [%expect
      {|
        Auth endpoint: https://login.microsoftonline.com/my-tenant-id/oauth2/v2.0/authorize
        Token endpoint: https://login.microsoftonline.com/my-tenant-id/oauth2/v2.0/token
        Issuer: https://login.microsoftonline.com/my-tenant-id/v2.0
        JWKS URI: https://login.microsoftonline.com/my-tenant-id/discovery/v2.0/keys
        |}];
    return ()

let%expect_test "test base authority azure government" =
  let provider_result =
    Azure_provider.create ~client_id:"test-client" ~client_secret:"secret"
      ~tenant_id:"gov-tenant-id" ~required_scopes:[ "read" ]
      ~base_authority:"login.microsoftonline.us" ()
  in
  match provider_result with
  | Error e ->
    print_s [%sexp (e : Error.t)];
    return ()
  | Ok provider ->
    print_endline ("Auth endpoint: " ^ provider.authorization_endpoint);
    [%expect
      {|
        Auth endpoint: https://login.microsoftonline.us/gov-tenant-id/oauth2/v2.0/authorize
        |}];
    return ()

let%expect_test "test prefix scopes for azure" =
  let identifier_uri = "api://my-api" in
  let scopes = [ "read"; "write"; "api://other-api/admin"; "User.Read" ] in
  let prefixed = Azure_provider.prefix_scopes_for_azure identifier_uri scopes in
  print_s [%sexp (prefixed : string list)];
  [%expect
    {|
    (api://my-api/read
     api://my-api/write
     api://other-api/admin
     api://my-api/User.Read)
    |}];
  return ()
