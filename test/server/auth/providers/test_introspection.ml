(** Tests for OAuth 2.0 Token Introspection verifier (RFC 7662). *)

open! Core
open! Expect_test_helpers_core
open Server_auth_providers.Introspection

(* =============================================================================
   Tests for Settings
   ============================================================================= *)

let%expect_test "Settings - create with all options" =
  let settings =
    Settings.create ~introspection_url:"https://auth.example.com/introspect"
      ~client_id:"test-client" ~client_secret:"test-secret" ~timeout_seconds:5
      ~required_scopes:[ "read"; "write" ] ()
  in
  printf "introspection_url: %s\n"
    (Option.value settings.introspection_url ~default:"NONE");
  printf "client_id: %s\n" (Option.value settings.client_id ~default:"NONE");
  printf "timeout_seconds: %d\n" settings.timeout_seconds;
  printf "required_scopes: %s\n"
    (settings.required_scopes |> Option.value ~default:[]
   |> String.concat ~sep:", ");
  [%expect
    {|
    introspection_url: https://auth.example.com/introspect
    client_id: test-client
    timeout_seconds: 5
    required_scopes: read, write
    |}]

let%expect_test "Settings - default timeout" =
  let settings =
    Settings.create ~introspection_url:"https://auth.example.com/introspect"
      ~client_id:"test-client" ~client_secret:"test-secret" ()
  in
  printf "timeout_seconds: %d\n" settings.timeout_seconds;
  [%expect {| timeout_seconds: 10 |}]

let%expect_test "Settings - validate requires introspection_url" =
  let s =
    Settings.create ~client_id:"test-client" ~client_secret:"test-secret" ()
  in
  (match Settings.validate s with
  | Ok () -> printf "Valid\n"
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {| Error: introspection_url is required - set via parameter or OXFASTMCP_SERVER_AUTH_INTROSPECTION_INTROSPECTION_URL |}]

let%expect_test "Settings - validate requires client_id" =
  let s =
    Settings.create ~introspection_url:"https://auth.example.com/introspect"
      ~client_secret:"test-secret" ()
  in
  (match Settings.validate s with
  | Ok () -> printf "Valid\n"
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {| Error: client_id is required - set via parameter or OXFASTMCP_SERVER_AUTH_INTROSPECTION_CLIENT_ID |}]

let%expect_test "Settings - validate requires client_secret" =
  let s =
    Settings.create ~introspection_url:"https://auth.example.com/introspect"
      ~client_id:"test-client" ()
  in
  (match Settings.validate s with
  | Ok () -> printf "Valid\n"
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {| Error: client_secret is required - set via parameter or OXFASTMCP_SERVER_AUTH_INTROSPECTION_CLIENT_SECRET |}]

let%expect_test "Settings - validate success" =
  let s =
    Settings.create ~introspection_url:"https://auth.example.com/introspect"
      ~client_id:"test-client" ~client_secret:"test-secret" ()
  in
  (match Settings.validate s with
  | Ok () -> printf "Valid\n"
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect {| Valid |}]

(* =============================================================================
   Tests for Introspection_token_verifier
   ============================================================================= *)

let%expect_test "Introspection_token_verifier - create with all params" =
  let result =
    Introspection_token_verifier.create
      ~introspection_url:"https://auth.example.com/oauth/introspect"
      ~client_id:"test-client" ~client_secret:"test-secret" ~timeout_seconds:5
      ~required_scopes:[ "read"; "write" ] ()
  in
  (match result with
  | Ok verifier ->
    printf "introspection_url: %s\n"
      (Introspection_token_verifier.introspection_url verifier);
    printf "client_id: %s\n" (Introspection_token_verifier.client_id verifier);
    printf "timeout_seconds: %d\n"
      (Introspection_token_verifier.timeout_seconds verifier);
    printf "required_scopes: %s\n"
      (Introspection_token_verifier.required_scopes verifier
      |> String.concat ~sep:", ")
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {|
    introspection_url: https://auth.example.com/oauth/introspect
    client_id: test-client
    timeout_seconds: 5
    required_scopes: read, write
    |}]

let%expect_test "Introspection_token_verifier - default values" =
  let result =
    Introspection_token_verifier.create
      ~introspection_url:"https://auth.example.com/oauth/introspect"
      ~client_id:"test-client" ~client_secret:"test-secret" ()
  in
  (match result with
  | Ok verifier ->
    printf "timeout_seconds: %d\n"
      (Introspection_token_verifier.timeout_seconds verifier);
    printf "required_scopes empty: %b\n"
      (List.is_empty (Introspection_token_verifier.required_scopes verifier))
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect {|
    timeout_seconds: 10
    required_scopes empty: true
    |}]

let%expect_test "Introspection_token_verifier - missing introspection_url" =
  let result =
    Introspection_token_verifier.create ~client_id:"test-client"
      ~client_secret:"test-secret" ()
  in
  (match result with
  | Ok _ -> printf "Unexpected success\n"
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {| Error: introspection_url is required - set via parameter or OXFASTMCP_SERVER_AUTH_INTROSPECTION_INTROSPECTION_URL |}]

let%expect_test "Introspection_token_verifier - missing client_id" =
  let result =
    Introspection_token_verifier.create
      ~introspection_url:"https://auth.example.com/oauth/introspect"
      ~client_secret:"test-secret" ()
  in
  (match result with
  | Ok _ -> printf "Unexpected success\n"
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {| Error: client_id is required - set via parameter or OXFASTMCP_SERVER_AUTH_INTROSPECTION_CLIENT_ID |}]

let%expect_test "Introspection_token_verifier - missing client_secret" =
  let result =
    Introspection_token_verifier.create
      ~introspection_url:"https://auth.example.com/oauth/introspect"
      ~client_id:"test-client" ()
  in
  (match result with
  | Ok _ -> printf "Unexpected success\n"
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {| Error: client_secret is required - set via parameter or OXFASTMCP_SERVER_AUTH_INTROSPECTION_CLIENT_SECRET |}]

let%expect_test "Introspection_token_verifier - create_basic_auth_header" =
  let result =
    Introspection_token_verifier.create
      ~introspection_url:"https://auth.example.com/oauth/introspect"
      ~client_id:"test-client" ~client_secret:"test-secret" ()
  in
  (match result with
  | Ok verifier ->
    let auth_header =
      Introspection_token_verifier.create_basic_auth_header verifier
    in
    printf "auth_header starts with Basic: %b\n"
      (String.is_prefix auth_header ~prefix:"Basic ");
    (* Decode and verify *)
    let encoded = String.drop_prefix auth_header 6 in
    let decoded = Base64.decode_exn encoded in
    printf "decoded credentials: %s\n" decoded
  | Error e -> printf "Error: %s\n" (Error.to_string_hum e));
  [%expect
    {|
    auth_header starts with Basic: true
    decoded credentials: test-client:test-secret
    |}]

let%expect_test "extract_scopes - from space-separated string" =
  let json = `Assoc [ ("scope", `String "read write admin") ] in
  let scopes = Introspection_token_verifier.extract_scopes json in
  printf "scopes: %s\n" (String.concat ~sep:", " scopes);
  [%expect {| scopes: read, write, admin |}]

let%expect_test "extract_scopes - from array" =
  let json =
    `Assoc
      [ ("scope", `List [ `String "read"; `String "write"; `String "admin" ]) ]
  in
  let scopes = Introspection_token_verifier.extract_scopes json in
  printf "scopes: %s\n" (String.concat ~sep:", " scopes);
  [%expect {| scopes: read, write, admin |}]

let%expect_test "extract_scopes - missing scope field" =
  let json = `Assoc [ ("active", `Bool true) ] in
  let scopes = Introspection_token_verifier.extract_scopes json in
  printf "scopes count: %d\n" (List.length scopes);
  [%expect {| scopes count: 0 |}]

let%expect_test "extract_scopes - with extra whitespace" =
  let json = `Assoc [ ("scope", `String "  read   write  admin  ") ] in
  let scopes = Introspection_token_verifier.extract_scopes json in
  printf "scopes: %s\n" (String.concat ~sep:", " scopes);
  [%expect {| scopes: read, write, admin |}]

let%expect_test "check_required_scopes - all present" =
  let result =
    Introspection_token_verifier.check_required_scopes
      ~required:[ "read"; "write" ]
      ~token_scopes:[ "read"; "write"; "admin" ]
  in
  printf "result: %b\n" result;
  [%expect {| result: true |}]

let%expect_test "check_required_scopes - missing required" =
  let result =
    Introspection_token_verifier.check_required_scopes
      ~required:[ "read"; "write" ] ~token_scopes:[ "read" ]
  in
  printf "result: %b\n" result;
  [%expect {| result: false |}]

let%expect_test "check_required_scopes - empty required" =
  let result =
    Introspection_token_verifier.check_required_scopes ~required:[]
      ~token_scopes:[ "read" ]
  in
  printf "result: %b\n" result;
  [%expect {| result: true |}]
