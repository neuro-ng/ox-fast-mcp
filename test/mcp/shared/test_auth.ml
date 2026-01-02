(** Tests for MCP Shared Auth Types

    Tests OAuth2 authentication types and validation functions including:
    - OAuthToken type
    - OAuthClientMetadata type and validation
    - OAuthClientInformationFull type
    - OAuthMetadata type
    - ProtectedResourceMetadata type *)

open! Core
open! Expect_test_helpers_core
open Mcp_shared.Auth

(* =============================================================================
   Test: OAuth Token
   ============================================================================= *)

let%expect_test "oauth_token - create with required fields" =
  let token =
    {
      access_token = "test-access-token";
      token_type = "Bearer";
      expires_in = None;
      scope = None;
      refresh_token = None;
    }
  in
  printf "access_token: %s\n" token.access_token;
  printf "token_type: %s\n" token.token_type;
  [%expect {|
    access_token: test-access-token
    token_type: Bearer |}]

let%expect_test "oauth_token - create with all fields" =
  let token =
    {
      access_token = "access123";
      token_type = "Bearer";
      expires_in = Some 3600;
      scope = Some "read write";
      refresh_token = Some "refresh456";
    }
  in
  printf "has_expires_in: %b\n" (Option.is_some token.expires_in);
  printf "has_scope: %b\n" (Option.is_some token.scope);
  printf "has_refresh_token: %b\n" (Option.is_some token.refresh_token);
  (match token.expires_in with
  | Some exp -> printf "expires_in: %d\n" exp
  | None -> ());
  [%expect
    {|
    has_expires_in: true
    has_scope: true
    has_refresh_token: true
    expires_in: 3600 |}]

(* =============================================================================
   Test: OAuth Client Metadata
   ============================================================================= *)

let%expect_test "oauth_client_metadata - create with minimal fields" =
  let metadata =
    {
      redirect_uris = [ "http://localhost:3030/callback" ];
      token_endpoint_auth_method = `None;
      grant_types = [ `Authorization_code; `Refresh_token ];
      response_types = [ `Code ];
      scope = None;
      client_name = None;
      client_uri = None;
      logo_uri = None;
      contacts = None;
      tos_uri = None;
      policy_uri = None;
      jwks_uri = None;
      jwks = None;
      software_id = None;
      software_version = None;
    }
  in
  printf "redirect_uris_count: %d\n" (List.length metadata.redirect_uris);
  printf "grant_types_count: %d\n" (List.length metadata.grant_types);
  [%expect {|
    redirect_uris_count: 1
    grant_types_count: 2 |}]

let%expect_test "oauth_client_metadata - with multiple redirect URIs" =
  let metadata =
    {
      redirect_uris =
        [
          "http://localhost:3030/callback";
          "http://localhost:3031/callback";
          "https://example.com/oauth/callback";
        ];
      token_endpoint_auth_method = `Client_secret_post;
      grant_types = [ `Authorization_code ];
      response_types = [ `Code ];
      scope = Some "read write delete";
      client_name = Some "Test Client";
      client_uri = Some "https://example.com";
      logo_uri = None;
      contacts = Some [ "admin@example.com"; "support@example.com" ];
      tos_uri = Some "https://example.com/tos";
      policy_uri = Some "https://example.com/privacy";
      jwks_uri = None;
      jwks = None;
      software_id = Some "test-app-123";
      software_version = Some "1.0.0";
    }
  in
  printf "redirect_uris_count: %d\n" (List.length metadata.redirect_uris);
  printf "has_scope: %b\n" (Option.is_some metadata.scope);
  printf "has_client_name: %b\n" (Option.is_some metadata.client_name);
  printf "contacts_count: %d\n"
    (Option.value_map metadata.contacts ~default:0 ~f:List.length);
  [%expect
    {|
    redirect_uris_count: 3
    has_scope: true
    has_client_name: true
    contacts_count: 2 |}]

(* =============================================================================
   Test: Validate Scope
   ============================================================================= *)

let%expect_test "validate_scope - with no requested scope" =
  let metadata =
    {
      redirect_uris = [ "http://localhost:3030/callback" ];
      token_endpoint_auth_method = `None;
      grant_types = [ `Authorization_code ];
      response_types = [ `Code ];
      scope = Some "read write";
      client_name = None;
      client_uri = None;
      logo_uri = None;
      contacts = None;
      tos_uri = None;
      policy_uri = None;
      jwks_uri = None;
      jwks = None;
      software_id = None;
      software_version = None;
    }
  in
  let result = validate_scope metadata None in
  printf "result_is_none: %b\n" (Option.is_none result);
  [%expect {| result_is_none: true |}]

let%expect_test "validate_scope - with valid requested scope" =
  let metadata =
    {
      redirect_uris = [ "http://localhost:3030/callback" ];
      token_endpoint_auth_method = `None;
      grant_types = [ `Authorization_code ];
      response_types = [ `Code ];
      scope = Some "read write delete";
      client_name = None;
      client_uri = None;
      logo_uri = None;
      contacts = None;
      tos_uri = None;
      policy_uri = None;
      jwks_uri = None;
      jwks = None;
      software_id = None;
      software_version = None;
    }
  in
  let result = validate_scope metadata (Some "read write") in
  (match result with
  | Some scopes ->
    printf "scopes_count: %d\n" (List.length scopes);
    List.iter scopes ~f:(fun s -> printf "scope: %s\n" s)
  | None -> printf "no scopes\n");
  [%expect {|
    scopes_count: 2
    scope: read
    scope: write |}]

let%expect_test "validate_scope - with invalid requested scope" =
  let metadata =
    {
      redirect_uris = [ "http://localhost:3030/callback" ];
      token_endpoint_auth_method = `None;
      grant_types = [ `Authorization_code ];
      response_types = [ `Code ];
      scope = Some "read write";
      client_name = None;
      client_uri = None;
      logo_uri = None;
      contacts = None;
      tos_uri = None;
      policy_uri = None;
      jwks_uri = None;
      jwks = None;
      software_id = None;
      software_version = None;
    }
  in
  let result =
    try
      let _scopes = validate_scope metadata (Some "read write delete") in
      "should_fail"
    with Invalid_scope_error msg ->
      printf "error: %s\n" msg;
      "failed_correctly"
  in
  printf "result: %s\n" result;
  [%expect
    {|
    error: Client was not registered with scope delete
    result: failed_correctly |}]

let%expect_test "validate_scope - with no allowed scopes" =
  let metadata =
    {
      redirect_uris = [ "http://localhost:3030/callback" ];
      token_endpoint_auth_method = `None;
      grant_types = [ `Authorization_code ];
      response_types = [ `Code ];
      scope = None;
      client_name = None;
      client_uri = None;
      logo_uri = None;
      contacts = None;
      tos_uri = None;
      policy_uri = None;
      jwks_uri = None;
      jwks = None;
      software_id = None;
      software_version = None;
    }
  in
  let result =
    try
      let _scopes = validate_scope metadata (Some "read") in
      "should_fail"
    with Invalid_scope_error msg ->
      printf "error: %s\n" msg;
      "failed_correctly"
  in
  printf "result: %s\n" result;
  [%expect
    {|
    error: Client was not registered with scope read
    result: failed_correctly |}]

(* =============================================================================
   Test: Validate Redirect URI
   ============================================================================= *)

let%expect_test "validate_redirect_uri - with valid URI" =
  let metadata =
    {
      redirect_uris =
        [ "http://localhost:3030/callback"; "https://example.com/callback" ];
      token_endpoint_auth_method = `None;
      grant_types = [ `Authorization_code ];
      response_types = [ `Code ];
      scope = None;
      client_name = None;
      client_uri = None;
      logo_uri = None;
      contacts = None;
      tos_uri = None;
      policy_uri = None;
      jwks_uri = None;
      jwks = None;
      software_id = None;
      software_version = None;
    }
  in
  let result =
    validate_redirect_uri metadata (Some "http://localhost:3030/callback")
  in
  printf "result: %s\n" result;
  [%expect {| result: http://localhost:3030/callback |}]

let%expect_test "validate_redirect_uri - with invalid URI" =
  let metadata =
    {
      redirect_uris = [ "http://localhost:3030/callback" ];
      token_endpoint_auth_method = `None;
      grant_types = [ `Authorization_code ];
      response_types = [ `Code ];
      scope = None;
      client_name = None;
      client_uri = None;
      logo_uri = None;
      contacts = None;
      tos_uri = None;
      policy_uri = None;
      jwks_uri = None;
      jwks = None;
      software_id = None;
      software_version = None;
    }
  in
  let result =
    try
      let _uri =
        validate_redirect_uri metadata (Some "https://evil.com/callback")
      in
      "should_fail"
    with Invalid_redirect_uri_error msg ->
      printf "error: %s\n" msg;
      "failed_correctly"
  in
  printf "result: %s\n" result;
  [%expect
    {|
    error: Redirect URI 'https://evil.com/callback' not registered for client
    result: failed_correctly |}]

let%expect_test "validate_redirect_uri - with None and single URI" =
  let metadata =
    {
      redirect_uris = [ "http://localhost:3030/callback" ];
      token_endpoint_auth_method = `None;
      grant_types = [ `Authorization_code ];
      response_types = [ `Code ];
      scope = None;
      client_name = None;
      client_uri = None;
      logo_uri = None;
      contacts = None;
      tos_uri = None;
      policy_uri = None;
      jwks_uri = None;
      jwks = None;
      software_id = None;
      software_version = None;
    }
  in
  let result = validate_redirect_uri metadata None in
  printf "result: %s\n" result;
  [%expect {| result: http://localhost:3030/callback |}]

let%expect_test "validate_redirect_uri - with None and multiple URIs" =
  let metadata =
    {
      redirect_uris =
        [ "http://localhost:3030/callback"; "https://example.com/callback" ];
      token_endpoint_auth_method = `None;
      grant_types = [ `Authorization_code ];
      response_types = [ `Code ];
      scope = None;
      client_name = None;
      client_uri = None;
      logo_uri = None;
      contacts = None;
      tos_uri = None;
      policy_uri = None;
      jwks_uri = None;
      jwks = None;
      software_id = None;
      software_version = None;
    }
  in
  let result =
    try
      let _uri = validate_redirect_uri metadata None in
      "should_fail"
    with Invalid_redirect_uri_error msg ->
      printf "error: %s\n" msg;
      "failed_correctly"
  in
  printf "result: %s\n" result;
  [%expect
    {|
    error: redirect_uri must be specified when client has multiple registered URIs
    result: failed_correctly |}]

(* =============================================================================
   Test: OAuth Client Information
   ============================================================================= *)

let%expect_test "oauth_client_information - create" =
  let info =
    {
      client_id = "client123";
      client_secret = Some "secret456";
      client_id_issued_at = Some 1609459200;
      client_secret_expires_at = None;
    }
  in
  printf "client_id: %s\n" info.client_id;
  printf "has_secret: %b\n" (Option.is_some info.client_secret);
  printf "has_issued_at: %b\n" (Option.is_some info.client_id_issued_at);
  [%expect
    {|
    client_id: client123
    has_secret: true
    has_issued_at: true |}]

(* =============================================================================
   Test: OAuth Metadata
   ============================================================================= *)

let%expect_test "oauth_metadata - create with required fields" =
  let metadata =
    {
      issuer = "https://auth.example.com";
      authorization_endpoint = "https://auth.example.com/authorize";
      token_endpoint = "https://auth.example.com/token";
      registration_endpoint = None;
      scopes_supported = None;
      response_types_supported = [ "code" ];
      response_modes_supported = None;
      grant_types_supported = None;
      token_endpoint_auth_methods_supported = None;
      token_endpoint_auth_signing_alg_values_supported = None;
      service_documentation = None;
      ui_locales_supported = None;
      op_policy_uri = None;
      op_tos_uri = None;
      revocation_endpoint = None;
      revocation_endpoint_auth_methods_supported = None;
      revocation_endpoint_auth_signing_alg_values_supported = None;
      introspection_endpoint = None;
      introspection_endpoint_auth_methods_supported = None;
      introspection_endpoint_auth_signing_alg_values_supported = None;
      code_challenge_methods_supported = None;
      client_id_metadata_document_supported = None;
    }
  in
  printf "issuer: %s\n" metadata.issuer;
  printf "has_registration_endpoint: %b\n"
    (Option.is_some metadata.registration_endpoint);
  [%expect
    {|
    issuer: https://auth.example.com
    has_registration_endpoint: false |}]

let%expect_test "oauth_metadata - create with all fields" =
  let metadata =
    {
      issuer = "https://auth.example.com";
      authorization_endpoint = "https://auth.example.com/authorize";
      token_endpoint = "https://auth.example.com/token";
      registration_endpoint = Some "https://auth.example.com/register";
      scopes_supported = Some [ "read"; "write"; "delete" ];
      response_types_supported = [ "code"; "token" ];
      response_modes_supported = Some [ `Query; `Fragment ];
      grant_types_supported =
        Some [ "authorization_code"; "refresh_token"; "client_credentials" ];
      token_endpoint_auth_methods_supported =
        Some [ "client_secret_basic"; "client_secret_post"; "none" ];
      token_endpoint_auth_signing_alg_values_supported = None;
      service_documentation = Some "https://auth.example.com/docs";
      ui_locales_supported = Some [ "en"; "es"; "fr" ];
      op_policy_uri = Some "https://auth.example.com/policy";
      op_tos_uri = Some "https://auth.example.com/tos";
      revocation_endpoint = Some "https://auth.example.com/revoke";
      revocation_endpoint_auth_methods_supported =
        Some [ "client_secret_basic" ];
      revocation_endpoint_auth_signing_alg_values_supported = None;
      introspection_endpoint = Some "https://auth.example.com/introspect";
      introspection_endpoint_auth_methods_supported =
        Some [ "client_secret_basic" ];
      introspection_endpoint_auth_signing_alg_values_supported = None;
      code_challenge_methods_supported = Some [ "S256"; "plain" ];
      client_id_metadata_document_supported = Some true;
    }
  in
  printf "has_registration: %b\n"
    (Option.is_some metadata.registration_endpoint);
  printf "scopes_count: %d\n"
    (Option.value_map metadata.scopes_supported ~default:0 ~f:List.length);
  printf "grant_types_count: %d\n"
    (Option.value_map metadata.grant_types_supported ~default:0 ~f:List.length);
  [%expect
    {|
    has_registration: true
    scopes_count: 3
    grant_types_count: 3 |}]

(* =============================================================================
   Test: Protected Resource Metadata
   ============================================================================= *)

let%expect_test "protected_resource_metadata - create" =
  let metadata =
    {
      resource = "https://api.example.com";
      authorization_servers = [ "https://auth.example.com" ];
      scopes_supported = Some [ "read"; "write" ];
      bearer_methods_supported = [ "header" ];
      resource_documentation = Some "https://api.example.com/docs";
    }
  in
  printf "resource: %s\n" metadata.resource;
  printf "auth_servers_count: %d\n" (List.length metadata.authorization_servers);
  printf "has_scopes: %b\n" (Option.is_some metadata.scopes_supported);
  printf "bearer_methods_count: %d\n"
    (List.length metadata.bearer_methods_supported);
  [%expect
    {|
    resource: https://api.example.com
    auth_servers_count: 1
    has_scopes: true
    bearer_methods_count: 1 |}]

let%expect_test "protected_resource_metadata - multiple auth servers" =
  let metadata =
    {
      resource = "https://api.example.com";
      authorization_servers =
        [
          "https://auth1.example.com";
          "https://auth2.example.com";
          "https://auth3.example.com";
        ];
      scopes_supported = None;
      bearer_methods_supported = [ "header"; "body"; "query" ];
      resource_documentation = None;
    }
  in
  printf "auth_servers_count: %d\n" (List.length metadata.authorization_servers);
  printf "bearer_methods_count: %d\n"
    (List.length metadata.bearer_methods_supported);
  [%expect {|
    auth_servers_count: 3
    bearer_methods_count: 3 |}]
