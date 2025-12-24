(** Tests for Scalekit OAuth provider. *)

open! Core
open! Expect_test_helpers_core
open Server_auth_providers.Scalekit

(* =============================================================================
   Tests for Settings
   ============================================================================= *)

let%expect_test "Settings - create with all options" =
  let settings =
    Settings.create ~environment_url:"https://my-env.scalekit.com"
      ~resource_id:"sk_resource_456" ~base_url:"https://myserver.com/"
      ~required_scopes:[ "read" ] ()
  in
  printf "environment_url: %s\n"
    (Option.value settings.environment_url ~default:"NONE");
  printf "resource_id: %s\n" (Option.value settings.resource_id ~default:"NONE");
  printf "base_url: %s\n" (Option.value settings.base_url ~default:"NONE");
  printf "required_scopes: %s\n"
    (settings.required_scopes |> Option.value ~default:[]
   |> String.concat ~sep:", ");
  [%expect
    {|
    environment_url: https://my-env.scalekit.com
    resource_id: sk_resource_456
    base_url: https://myserver.com/
    required_scopes: read
    |}]

let%expect_test "Settings - validate requires environment_url" =
  let s =
    Settings.create ~resource_id:"sk_resource_456" ~base_url:"http://x" ()
  in
  (match Settings.validate s with
  | Ok () -> printf "Valid\n"
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {| Error: environment_url is required - set via parameter or OXFASTMCP_SERVER_AUTH_SCALEKITPROVIDER_ENVIRONMENT_URL |}]

let%expect_test "Settings - validate requires resource_id" =
  let s =
    Settings.create ~environment_url:"https://my-env.scalekit.com"
      ~base_url:"http://x" ()
  in
  (match Settings.validate s with
  | Ok () -> printf "Valid\n"
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {| Error: resource_id is required - set via parameter or OXFASTMCP_SERVER_AUTH_SCALEKITPROVIDER_RESOURCE_ID |}]

let%expect_test "Settings - validate requires base_url or mcp_url" =
  let s =
    Settings.create ~environment_url:"https://my-env.scalekit.com"
      ~resource_id:"sk_resource_456" ()
  in
  (match Settings.validate s with
  | Ok () -> printf "Valid\n"
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {| Error: Either base_url or mcp_url must be provided for ScalekitProvider |}]

let%expect_test "Settings - resolve_base_url prefers base_url over mcp_url" =
  let s =
    Settings.create ~environment_url:"https://my-env.scalekit.com"
      ~resource_id:"sk_resource_456" ~base_url:"https://preferred.com"
      ~mcp_url:"https://legacy.com" ()
  in
  (match Settings.resolve_base_url s with
  | Ok url -> printf "base_url: %s\n" url
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect {| base_url: https://preferred.com |}]

let%expect_test "Settings - resolve_base_url falls back to mcp_url" =
  let s =
    Settings.create ~environment_url:"https://my-env.scalekit.com"
      ~resource_id:"sk_resource_456" ~mcp_url:"https://legacy.com" ()
  in
  (match Settings.resolve_base_url s with
  | Ok url -> printf "base_url: %s\n" url
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect {| base_url: https://legacy.com |}]

(* =============================================================================
   Tests for Scalekit_provider
   ============================================================================= *)

let%expect_test "Scalekit_provider - create with explicit params" =
  let result =
    Scalekit_provider.create ~environment_url:"https://my-env.scalekit.com"
      ~resource_id:"sk_resource_456" ~base_url:"https://myserver.com/"
      ~required_scopes:[ "read" ] ()
  in
  (match result with
  | Ok provider ->
    printf "environment_url: %s\n" (Scalekit_provider.environment_url provider);
    printf "resource_id: %s\n" (Scalekit_provider.resource_id provider);
    printf "base_url: %s\n" (Scalekit_provider.base_url provider);
    printf "required_scopes: %s\n"
      (Scalekit_provider.required_scopes provider |> String.concat ~sep:", ")
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {|
    environment_url: https://my-env.scalekit.com
    resource_id: sk_resource_456
    base_url: https://myserver.com/
    required_scopes: read
    |}]

let%expect_test "Scalekit_provider - strips trailing slash from environment_url"
    =
  let result =
    Scalekit_provider.create ~environment_url:"https://my-env.scalekit.com/"
      ~resource_id:"sk_resource_456" ~base_url:"https://myserver.com/" ()
  in
  (match result with
  | Ok provider ->
    printf "environment_url: %s\n" (Scalekit_provider.environment_url provider)
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect {| environment_url: https://my-env.scalekit.com |}]

let%expect_test "Scalekit_provider - jwks_uri configured correctly" =
  let result =
    Scalekit_provider.create ~environment_url:"https://my-env.scalekit.com"
      ~resource_id:"sk_resource_456" ~base_url:"https://myserver.com/" ()
  in
  (match result with
  | Ok provider ->
    printf "jwks_uri: %s\n" (Scalekit_provider.jwks_uri provider);
    printf "issuer: %s\n" (Scalekit_provider.issuer provider)
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {|
    jwks_uri: https://my-env.scalekit.com/keys
    issuer: https://my-env.scalekit.com
    |}]

let%expect_test "Scalekit_provider - authorization_servers configured correctly"
    =
  let result =
    Scalekit_provider.create ~environment_url:"https://my-env.scalekit.com"
      ~resource_id:"sk_resource_456" ~base_url:"https://myserver.com/" ()
  in
  (match result with
  | Ok provider ->
    printf "authorization_servers: %s\n"
      (Scalekit_provider.authorization_servers provider
      |> String.concat ~sep:", ")
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {| authorization_servers: https://my-env.scalekit.com/resources/sk_resource_456 |}]

let%expect_test "Scalekit_provider - metadata_url" =
  let result =
    Scalekit_provider.create ~environment_url:"https://test-env.scalekit.com"
      ~resource_id:"sk_resource_test_456" ~base_url:"http://localhost:4321" ()
  in
  (match result with
  | Ok provider ->
    printf "metadata_url: %s\n" (Scalekit_provider.metadata_url provider)
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {| metadata_url: https://test-env.scalekit.com/.well-known/oauth-authorization-server/resources/sk_resource_test_456 |}]

let%expect_test "Scalekit_provider - default empty required_scopes" =
  let result =
    Scalekit_provider.create ~environment_url:"https://my-env.scalekit.com"
      ~resource_id:"sk_resource_456" ~base_url:"https://myserver.com/" ()
  in
  (match result with
  | Ok provider ->
    printf "required_scopes empty: %b\n"
      (List.is_empty (Scalekit_provider.required_scopes provider))
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect {| required_scopes empty: true |}]

let%expect_test "Scalekit_provider - missing environment_url raises error" =
  let result =
    Scalekit_provider.create ~resource_id:"sk_resource_456"
      ~base_url:"https://myserver.com/" ()
  in
  (match result with
  | Ok _ -> printf "Unexpected success\n"
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {| Error: environment_url is required - set via parameter or OXFASTMCP_SERVER_AUTH_SCALEKITPROVIDER_ENVIRONMENT_URL |}]

let%expect_test "Scalekit_provider - missing resource_id raises error" =
  let result =
    Scalekit_provider.create ~environment_url:"https://my-env.scalekit.com"
      ~base_url:"https://myserver.com/" ()
  in
  (match result with
  | Ok _ -> printf "Unexpected success\n"
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {| Error: resource_id is required - set via parameter or OXFASTMCP_SERVER_AUTH_SCALEKITPROVIDER_RESOURCE_ID |}]

let%expect_test "Scalekit_provider - missing base_url raises error" =
  let result =
    Scalekit_provider.create ~environment_url:"https://my-env.scalekit.com"
      ~resource_id:"sk_resource_456" ()
  in
  (match result with
  | Ok _ -> printf "Unexpected success\n"
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {| Error: Either base_url or mcp_url must be provided for ScalekitProvider |}]
