(** Comprehensive tests for OIDC Proxy Provider functionality. *)

open! Core
open Server_auth.Oidc_proxy

(* =============================================================================
   Test Constants
   ============================================================================= *)

let test_issuer = "https://example.com"
let test_authorization_endpoint = "https://example.com/authorize"
let test_token_endpoint = "https://example.com/oauth/token"
let test_config_url = "https://example.com/.well-known/openid-configuration"
let test_client_id = "test-client-id"
let test_client_secret = "test-client-secret"
let test_base_url = "https://example.com:8000/"

(* =============================================================================
   Test Helper Functions
   ============================================================================= *)

(** Create a valid OIDC configuration dict for testing. *)
let valid_oidc_configuration_json () =
  `Assoc
    [
      ("issuer", `String test_issuer);
      ("authorization_endpoint", `String test_authorization_endpoint);
      ("token_endpoint", `String test_token_endpoint);
      ("jwks_uri", `String "https://example.com/.well-known/jwks.json");
      ("response_types_supported", `List [ `String "code" ]);
      ("subject_types_supported", `List [ `String "public" ]);
      ("id_token_signing_alg_values_supported", `List [ `String "RS256" ]);
    ]

(** Create an invalid OIDC configuration dict for testing (missing required
    fields). *)
let invalid_oidc_configuration_json () =
  `Assoc
    [
      ("issuer", `String test_issuer);
      ("authorization_endpoint", `String test_authorization_endpoint);
      ("token_endpoint", `String test_token_endpoint);
      ("jwks_uri", `String "https://example.com/.well-known/jwks.json");
      (* Missing: response_types_supported, subject_types_supported,
         id_token_signing_alg_values_supported *)
    ]

(** Create a valid Google OIDC configuration dict for testing. *)
let valid_google_oidc_configuration_json () =
  Yojson.Safe.from_string
    {|{
      "issuer": "https://accounts.google.com",
      "authorization_endpoint": "https://accounts.google.com/o/oauth2/v2/auth",
      "device_authorization_endpoint": "https://oauth2.googleapis.com/device/code",
      "token_endpoint": "https://oauth2.googleapis.com/token",
      "userinfo_endpoint": "https://openidconnect.googleapis.com/v1/userinfo",
      "revocation_endpoint": "https://oauth2.googleapis.com/revoke",
      "jwks_uri": "https://www.googleapis.com/oauth2/v3/certs",
      "response_types_supported": ["code", "token", "id_token", "code token", "code id_token", "token id_token", "code token id_token", "none"],
      "response_modes_supported": ["query", "fragment", "form_post"],
      "subject_types_supported": ["public"],
      "id_token_signing_alg_values_supported": ["RS256"],
      "scopes_supported": ["openid", "email", "profile"],
      "token_endpoint_auth_methods_supported": ["client_secret_post", "client_secret_basic"],
      "claims_supported": ["aud", "email", "email_verified", "exp", "family_name", "given_name", "iat", "iss", "name", "picture", "sub"],
      "code_challenge_methods_supported": ["plain", "S256"],
      "grant_types_supported": ["authorization_code", "refresh_token", "urn:ietf:params:oauth:grant-type:device_code", "urn:ietf:params:oauth:grant-type:jwt-bearer"]
    }|}

(** Create a valid Auth0 OIDC configuration dict for testing. *)
let valid_auth0_oidc_configuration_json () =
  Yojson.Safe.from_string
    {|{
      "issuer": "https://example.us.auth0.com/",
      "authorization_endpoint": "https://example.us.auth0.com/authorize",
      "token_endpoint": "https://example.us.auth0.com/oauth/token",
      "device_authorization_endpoint": "https://example.us.auth0.com/oauth/device/code",
      "userinfo_endpoint": "https://example.us.auth0.com/userinfo",
      "jwks_uri": "https://example.us.auth0.com/.well-known/jwks.json",
      "registration_endpoint": "https://example.us.auth0.com/oidc/register",
      "revocation_endpoint": "https://example.us.auth0.com/oauth/revoke",
      "scopes_supported": ["openid", "profile", "offline_access", "name", "given_name", "family_name", "nickname", "email", "email_verified", "picture", "created_at", "identities", "phone", "address"],
      "response_types_supported": ["code", "token", "id_token", "code token", "code id_token", "token id_token", "code token id_token"],
      "code_challenge_methods_supported": ["S256", "plain"],
      "response_modes_supported": ["query", "fragment", "form_post"],
      "subject_types_supported": ["public"],
      "token_endpoint_auth_methods_supported": ["client_secret_basic", "client_secret_post", "private_key_jwt", "tls_client_auth", "self_signed_tls_client_auth"],
      "token_endpoint_auth_signing_alg_values_supported": ["RS256", "RS384", "PS256"],
      "claims_supported": ["aud", "auth_time", "created_at", "email", "email_verified", "exp", "family_name", "given_name", "iat", "identities", "iss", "name", "nickname", "phone_number", "picture", "sub"],
      "request_uri_parameter_supported": false,
      "request_parameter_supported": true,
      "id_token_signing_alg_values_supported": ["HS256", "RS256", "PS256"],
      "request_object_signing_alg_values_supported": ["RS256", "RS384", "PS256"],
      "backchannel_logout_supported": true,
      "backchannel_logout_session_supported": true
    }|}

(** Validate configuration against source JSON. *)
let validate_config config source_json =
  let open Yojson.Safe.Util in
  let assoc = to_assoc source_json in
  List.iter assoc ~f:(fun (key, value) ->
      match key with
      | "issuer" -> (
        let expected = to_string value in
        match config.Oidc_configuration.issuer with
        | Some v -> assert (String.equal v expected)
        | None -> ())
      | "authorization_endpoint" -> (
        let expected = to_string value in
        match config.Oidc_configuration.authorization_endpoint with
        | Some v -> assert (String.equal v expected)
        | None -> ())
      | "token_endpoint" -> (
        let expected = to_string value in
        match config.Oidc_configuration.token_endpoint with
        | Some v -> assert (String.equal v expected)
        | None -> ())
      | "jwks_uri" -> (
        let expected = to_string value in
        match config.Oidc_configuration.jwks_uri with
        | Some v -> assert (String.equal v expected)
        | None -> ())
      | _ -> ())

(* =============================================================================
   Tests for OIDC Configuration
   ============================================================================= *)

let%expect_test "Oidc_configuration - default configuration with valid dict" =
  let json = valid_oidc_configuration_json () in
  match Oidc_configuration.of_yojson_strict json ~strict:true with
  | Ok config ->
    validate_config config json;
    printf "Configuration valid: true\n";
    printf "Issuer: %s\n" (Option.value config.issuer ~default:"NONE");
    [%expect
      {|
      Configuration valid: true
      Issuer: https://example.com
      |}]
  | Error msg ->
    printf "Error: %s\n" msg;
    [%expect.unreachable]

let%expect_test "Oidc_configuration - default configuration with issuer \
                 trailing slash" =
  let json =
    `Assoc
      [
        ("issuer", `String (test_issuer ^ "/"));
        ("authorization_endpoint", `String test_authorization_endpoint);
        ("token_endpoint", `String test_token_endpoint);
        ("jwks_uri", `String "https://example.com/.well-known/jwks.json");
        ("response_types_supported", `List [ `String "code" ]);
        ("subject_types_supported", `List [ `String "public" ]);
        ("id_token_signing_alg_values_supported", `List [ `String "RS256" ]);
      ]
  in
  match Oidc_configuration.of_yojson_strict json ~strict:true with
  | Ok config ->
    printf "Configuration valid: true\n";
    printf "Issuer: %s\n" (Option.value config.issuer ~default:"NONE");
    [%expect
      {|
      Configuration valid: true
      Issuer: https://example.com/
      |}]
  | Error msg ->
    printf "Error: %s\n" msg;
    [%expect.unreachable]

let%expect_test "Oidc_configuration - strict configuration raises error on \
                 invalid dict" =
  let json = invalid_oidc_configuration_json () in
  match Oidc_configuration.of_yojson_strict json ~strict:true with
  | Ok _ ->
    printf "Unexpectedly succeeded\n";
    [%expect.unreachable]
  | Error msg ->
    printf "Error contains 'Missing required': %b\n"
      (String.is_substring msg ~substring:"Missing required");
    [%expect {| Error contains 'Missing required': true |}]

let%expect_test "Oidc_configuration - bad URL raises error" =
  let json =
    `Assoc
      [
        ("issuer", `String "not-a-URL");
        ("authorization_endpoint", `String test_authorization_endpoint);
        ("token_endpoint", `String test_token_endpoint);
        ("jwks_uri", `String "https://example.com/.well-known/jwks.json");
        ("response_types_supported", `List [ `String "code" ]);
        ("subject_types_supported", `List [ `String "public" ]);
        ("id_token_signing_alg_values_supported", `List [ `String "RS256" ]);
      ]
  in
  match Oidc_configuration.of_yojson_strict json ~strict:true with
  | Ok _ ->
    printf "Unexpectedly succeeded\n";
    [%expect.unreachable]
  | Error msg ->
    printf "Error contains 'Invalid URL': %b\n"
      (String.is_substring msg ~substring:"Invalid URL");
    [%expect {| Error contains 'Invalid URL': true |}]

let%expect_test "Oidc_configuration - non-strict configuration with invalid \
                 dict" =
  let json = invalid_oidc_configuration_json () in
  match Oidc_configuration.of_yojson_strict json ~strict:false with
  | Ok config ->
    printf "Configuration valid: true\n";
    printf "Issuer: %s\n" (Option.value config.issuer ~default:"NONE");
    printf "Strict: %b\n" config.strict;
    [%expect
      {|
      Configuration valid: true
      Issuer: https://example.com
      Strict: false
      |}]
  | Error msg ->
    printf "Error: %s\n" msg;
    [%expect.unreachable]

let%expect_test "Oidc_configuration - non-strict configuration with empty dict"
    =
  let json = `Assoc [] in
  match Oidc_configuration.of_yojson_strict json ~strict:false with
  | Ok config ->
    printf "Strict: %b\n" config.strict;
    printf "Issuer: %s\n" (Option.value config.issuer ~default:"NONE");
    printf "Authorization endpoint: %s\n"
      (Option.value config.authorization_endpoint ~default:"NONE");
    [%expect
      {|
      Strict: false
      Issuer: NONE
      Authorization endpoint: NONE
      |}]
  | Error msg ->
    printf "Error: %s\n" msg;
    [%expect.unreachable]

let%expect_test "Oidc_configuration - non-strict configuration with bad URL" =
  let json =
    `Assoc [ ("issuer", `String "not-a-url"); ("strict", `Bool false) ]
  in
  match Oidc_configuration.of_yojson_strict json ~strict:false with
  | Ok config ->
    printf "Configuration valid: true\n";
    printf "Issuer: %s\n" (Option.value config.issuer ~default:"NONE");
    [%expect
      {|
      Configuration valid: true
      Issuer: not-a-url
      |}]
  | Error msg ->
    printf "Error: %s\n" msg;
    [%expect.unreachable]

let%expect_test "Oidc_configuration - Google configuration" =
  let json = valid_google_oidc_configuration_json () in
  match Oidc_configuration.of_yojson_strict json ~strict:true with
  | Ok config ->
    printf "Configuration valid: true\n";
    printf "Issuer: %s\n" (Option.value config.issuer ~default:"NONE");
    printf "Has revocation endpoint: %b\n"
      (Option.is_some config.revocation_endpoint);
    [%expect
      {|
      Configuration valid: true
      Issuer: https://accounts.google.com
      Has revocation endpoint: true
      |}]
  | Error msg ->
    printf "Error: %s\n" msg;
    [%expect.unreachable]

let%expect_test "Oidc_configuration - Auth0 configuration" =
  let json = valid_auth0_oidc_configuration_json () in
  match Oidc_configuration.of_yojson_strict json ~strict:true with
  | Ok config ->
    printf "Configuration valid: true\n";
    printf "Issuer: %s\n" (Option.value config.issuer ~default:"NONE");
    printf "Has registration endpoint: %b\n"
      (Option.is_some config.registration_endpoint);
    [%expect
      {|
      Configuration valid: true
      Issuer: https://example.us.auth0.com/
      Has registration endpoint: true
      |}]
  | Error msg ->
    printf "Error: %s\n" msg;
    [%expect.unreachable]

(* =============================================================================
   Tests for OIDC Proxy Initialization Validation
   ============================================================================= *)

(* Note: These tests cannot make actual HTTP requests without mocking. We test
   the validation logic that happens before HTTP calls. *)

let%expect_test "Oidc_proxy.create - missing config_url raises error" =
  let result =
    try
      let _ =
        Oidc_proxy.create ~config_url:"" ~client_id:test_client_id
          ~client_secret:test_client_secret ~base_url:test_base_url ()
      in
      "No error"
    with Failure msg -> msg
  in
  printf "Error: %s\n" result;
  [%expect {| Error: Missing required config URL |}]

let%expect_test "Oidc_proxy.create - missing client_id raises error" =
  let result =
    try
      let _ =
        Oidc_proxy.create ~config_url:test_config_url ~client_id:""
          ~client_secret:test_client_secret ~base_url:test_base_url ()
      in
      "No error"
    with Failure msg -> msg
  in
  printf "Error: %s\n" result;
  [%expect {| Error: Missing required client id |}]

let%expect_test "Oidc_proxy.create - missing client_secret raises error" =
  let result =
    try
      let _ =
        Oidc_proxy.create ~config_url:test_config_url ~client_id:test_client_id
          ~client_secret:"" ~base_url:test_base_url ()
      in
      "No error"
    with Failure msg -> msg
  in
  printf "Error: %s\n" result;
  [%expect {| Error: Missing required client secret |}]

let%expect_test "Oidc_proxy.create - missing base_url raises error" =
  let result =
    try
      let _ =
        Oidc_proxy.create ~config_url:test_config_url ~client_id:test_client_id
          ~client_secret:test_client_secret ~base_url:"" ()
      in
      "No error"
    with Failure msg -> msg
  in
  printf "Error: %s\n" result;
  [%expect {| Error: Missing required base URL |}]

let%expect_test "Oidc_proxy.create - algorithm with token_verifier raises error"
    =
  let result =
    try
      (* Create a mock token verifier module *)
      let module Mock_verifier : Server_auth.Oauth_proxy.TOKEN_VERIFIER = struct
        let required_scopes = []
        let verify_token _ = Lwt.return_none
      end in
      let _ =
        Oidc_proxy.create ~config_url:test_config_url ~client_id:test_client_id
          ~client_secret:test_client_secret ~base_url:test_base_url
          ~token_verifier:(module Mock_verifier) ~algorithm:"RS256" ()
      in
      "No error"
    with Failure msg -> msg
  in
  printf "Error contains 'algorithm': %b\n"
    (String.is_substring result ~substring:"algorithm");
  [%expect {| Error contains 'algorithm': true |}]

let%expect_test "Oidc_proxy.create - required_scopes with token_verifier \
                 raises error" =
  let result =
    try
      let module Mock_verifier : Server_auth.Oauth_proxy.TOKEN_VERIFIER = struct
        let required_scopes = []
        let verify_token _ = Lwt.return_none
      end in
      let _ =
        Oidc_proxy.create ~config_url:test_config_url ~client_id:test_client_id
          ~client_secret:test_client_secret ~base_url:test_base_url
          ~token_verifier:(module Mock_verifier)
          ~required_scopes:[ "read"; "write" ] ()
      in
      "No error"
    with Failure msg -> msg
  in
  printf "Error contains 'required_scopes': %b\n"
    (String.is_substring result ~substring:"required_scopes");
  [%expect {| Error contains 'required_scopes': true |}]

(* =============================================================================
   Tests for Sexp/Yojson Serialization
   ============================================================================= *)

let%expect_test "Oidc_configuration - sexp serialization" =
  let json = valid_oidc_configuration_json () in
  match Oidc_configuration.of_yojson_strict json ~strict:true with
  | Ok config ->
    let sexp = Oidc_configuration.sexp_of_t config in
    printf "Sexp contains issuer: %b\n"
      (String.is_substring (Sexp.to_string sexp) ~substring:test_issuer);
    [%expect {| Sexp contains issuer: true |}]
  | Error msg ->
    printf "Error: %s\n" msg;
    [%expect.unreachable]

let%expect_test "Oidc_configuration - yojson serialization roundtrip" =
  let json = valid_oidc_configuration_json () in
  match Oidc_configuration.of_yojson_strict json ~strict:true with
  | Ok config -> (
    let json' = Oidc_configuration.yojson_of_t config in
    match Oidc_configuration.of_yojson_strict json' ~strict:true with
    | Ok config' ->
      printf "Roundtrip successful: %b\n"
        (Oidc_configuration.compare config config' = 0);
      [%expect {| Roundtrip successful: true |}]
    | Error msg ->
      printf "Roundtrip error: %s\n" msg;
      [%expect.unreachable])
  | Error msg ->
    printf "Error: %s\n" msg;
    [%expect.unreachable]
