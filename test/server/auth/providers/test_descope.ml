(** Tests for Descope OAuth provider. *)

open! Core
open! Expect_test_helpers_core
open Server_auth_providers.Descope

let valid_config_url =
  "https://api.descope.com/v1/apps/agentic/P2abc123/M123/.well-known/openid-configuration"

let config_url_no_suffix =
  "https://api.descope.com/v1/apps/agentic/P2abc123/M123"

let localhost_config_url =
  "http://localhost:8080/v1/apps/agentic/P2abc123/M123/.well-known/openid-configuration"

(* =============================================================================
   Tests for Settings
   ============================================================================= *)

let%expect_test "Settings - create with all options" =
  let settings =
    Settings.create ~config_url:valid_config_url
      ~base_url:"https://myserver.com" ~required_scopes:[ "read"; "write" ] ()
  in
  printf "config_url: %s\n" (Option.value settings.config_url ~default:"NONE");
  printf "base_url: %s\n" (Option.value settings.base_url ~default:"NONE");
  printf "required_scopes: %s\n"
    (settings.required_scopes |> Option.value ~default:[]
   |> String.concat ~sep:", ");
  [%expect
    {|
    config_url: https://api.descope.com/v1/apps/agentic/P2abc123/M123/.well-known/openid-configuration
    base_url: https://myserver.com
    required_scopes: read, write
    |}]

let%expect_test "Settings - merge prioritizes first non-None" =
  let s1 =
    Settings.create ~config_url:valid_config_url ~base_url:"https://server1.com"
      ()
  in
  let s2 =
    Settings.create ~base_url:"https://server2.com" ~project_id:"P2override" ()
  in
  let merged = Settings.merge s1 s2 in
  printf "config_url: %s\n" (Option.value merged.config_url ~default:"NONE");
  printf "base_url: %s\n" (Option.value merged.base_url ~default:"NONE");
  printf "project_id: %s\n" (Option.value merged.project_id ~default:"NONE");
  [%expect
    {|
    config_url: https://api.descope.com/v1/apps/agentic/P2abc123/M123/.well-known/openid-configuration
    base_url: https://server1.com
    project_id: P2override
    |}]

let%expect_test "Settings - validate requires config_url or \
                 project_id+descope_base_url" =
  let s1 = Settings.create ~base_url:"https://myserver.com" () in
  (match Settings.validate s1 with
  | Ok () -> printf "Valid\n"
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {| Error: Either config_url (new API) or both project_id and descope_base_url (old API) must be provided |}];
  let s2 =
    Settings.create ~config_url:valid_config_url
      ~base_url:"https://myserver.com" ()
  in
  (match Settings.validate s2 with
  | Ok () -> printf "Valid\n"
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect {| Valid |}];
  let s3 =
    Settings.create ~project_id:"P2abc123"
      ~descope_base_url:"https://api.descope.com"
      ~base_url:"https://myserver.com" ()
  in
  (match Settings.validate s3 with
  | Ok () -> printf "Valid\n"
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect {| Valid |}]

(* =============================================================================
   Tests for Descope_provider
   ============================================================================= *)

let%expect_test "Descope_provider - create with config_url" =
  let result =
    Descope_provider.create ~config_url:valid_config_url
      ~base_url:"https://myserver.com" ()
  in
  (match result with
  | Ok provider ->
    printf "project_id: %s\n" (Descope_provider.project_id provider);
    printf "descope_base_url: %s\n" (Descope_provider.descope_base_url provider);
    printf "base_url: %s\n" (Descope_provider.base_url provider);
    printf "issuer_url: %s\n" (Descope_provider.issuer_url provider);
    printf "jwks_uri: %s\n" (Descope_provider.jwks_uri provider)
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {|
    project_id: P2abc123
    descope_base_url: https://api.descope.com
    base_url: https://myserver.com
    issuer_url: https://api.descope.com/v1/apps/agentic/P2abc123/M123
    jwks_uri: https://api.descope.com/P2abc123/.well-known/jwks.json
    |}]

let%expect_test "Descope_provider - create with config_url without suffix" =
  let result =
    Descope_provider.create ~config_url:config_url_no_suffix
      ~base_url:"https://myserver.com" ()
  in
  (match result with
  | Ok provider ->
    printf "project_id: %s\n" (Descope_provider.project_id provider);
    printf "issuer_url: %s\n" (Descope_provider.issuer_url provider)
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {|
    project_id: P2abc123
    issuer_url: https://api.descope.com/v1/apps/agentic/P2abc123/M123
    |}]

let%expect_test "Descope_provider - create with localhost config_url" =
  let result =
    Descope_provider.create ~config_url:localhost_config_url
      ~base_url:"https://myserver.com" ()
  in
  (match result with
  | Ok provider ->
    printf "project_id: %s\n" (Descope_provider.project_id provider);
    printf "descope_base_url: %s\n" (Descope_provider.descope_base_url provider)
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {|
    project_id: P2abc123
    descope_base_url: http://localhost:8080
    |}]

let%expect_test "Descope_provider - backwards compatibility with project_id \
                 and descope_base_url" =
  let result =
    Descope_provider.create ~project_id:"P2abc123"
      ~descope_base_url:"https://api.descope.com"
      ~base_url:"https://myserver.com" ()
  in
  (match result with
  | Ok provider ->
    printf "project_id: %s\n" (Descope_provider.project_id provider);
    printf "descope_base_url: %s\n" (Descope_provider.descope_base_url provider);
    printf "issuer_url: %s\n" (Descope_provider.issuer_url provider);
    printf "jwks_uri: %s\n" (Descope_provider.jwks_uri provider)
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {|
    project_id: P2abc123
    descope_base_url: https://api.descope.com
    issuer_url: https://api.descope.com/v1/apps/P2abc123
    jwks_uri: https://api.descope.com/P2abc123/.well-known/jwks.json
    |}]

let%expect_test "Descope_provider - descope_base_url without scheme" =
  let result =
    Descope_provider.create ~project_id:"P2abc123"
      ~descope_base_url:"api.descope.com" ~base_url:"https://myserver.com" ()
  in
  (match result with
  | Ok provider ->
    printf "descope_base_url: %s\n" (Descope_provider.descope_base_url provider);
    printf "issuer_url: %s\n" (Descope_provider.issuer_url provider)
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {|
    descope_base_url: https://api.descope.com
    issuer_url: https://api.descope.com/v1/apps/P2abc123
    |}]

let%expect_test "Descope_provider - required_scopes" =
  let result =
    Descope_provider.create ~config_url:valid_config_url
      ~base_url:"https://myserver.com" ~required_scopes:[ "read"; "write" ] ()
  in
  (match result with
  | Ok provider ->
    printf "required_scopes: %s\n"
      (Descope_provider.required_scopes provider |> String.concat ~sep:", ")
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect {| required_scopes: read, write |}]

let%expect_test "Descope_provider - oauth_metadata_url" =
  let result =
    Descope_provider.create ~config_url:valid_config_url
      ~base_url:"https://myserver.com" ()
  in
  (match result with
  | Ok provider ->
    printf "oauth_metadata_url: %s\n"
      (Descope_provider.oauth_metadata_url provider)
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {| oauth_metadata_url: https://api.descope.com/v1/apps/P2abc123/.well-known/oauth-authorization-server |}]

let%expect_test "Descope_provider - error when neither API provided" =
  let result = Descope_provider.create ~base_url:"https://myserver.com" () in
  (match result with
  | Ok _ -> printf "Unexpected success\n"
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {| Error: Either config_url (new API) or both project_id and descope_base_url (old API) must be provided |}]

let%expect_test "Descope_provider - error when base_url missing" =
  let result = Descope_provider.create ~config_url:valid_config_url () in
  (match result with
  | Ok _ -> printf "Unexpected success\n"
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {| Error: base_url is required - set via parameter or OXFASTMCP_SERVER_AUTH_DESCOPEPROVIDER_BASE_URL |}]
