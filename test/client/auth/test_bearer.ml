(** Tests for Bearer Authentication module

    Tests BearerAuth functionality including token handling, header
    manipulation, and Secret_string integration for secure token storage. *)

open! Core
open! Expect_test_helpers_core
module Bearer = Client_auth.Bearer.BearerAuth

(* =============================================================================
   Test: BearerAuth Creation
   ============================================================================= *)

let%expect_test "BearerAuth.create - creates instance with token" =
  let _auth = Bearer.create "my-secret-token" in
  printf "auth_created: true\n";
  [%expect {| auth_created: true |}]

let%expect_test "BearerAuth.create - token is stored securely" =
  let auth = Bearer.create "my-secret-token" in
  (* Test that sexp serialization redacts the token *)
  let sexp_str = Sexp.to_string ([%sexp_of: Bearer.t] auth) in
  (* Should contain REDACTED, not the actual token *)
  let contains_redacted = String.is_substring sexp_str ~substring:"REDACTED" in
  let contains_token =
    String.is_substring sexp_str ~substring:"my-secret-token"
  in
  printf "sexp_contains_redacted: %b\n" contains_redacted;
  printf "sexp_contains_actual_token: %b\n" contains_token;
  [%expect
    {|
    sexp_contains_redacted: true
    sexp_contains_actual_token: false |}]

(* =============================================================================
   Test: BearerAuth.apply_auth
   ============================================================================= *)

let%expect_test "BearerAuth.apply_auth - adds Authorization header" =
  let auth = Bearer.create "test-token-123" in
  let headers = Cohttp.Header.init () in
  let updated_headers = Bearer.apply_auth auth headers in
  let auth_value = Cohttp.Header.get updated_headers "authorization" in
  printf "authorization_header: %s\n" (Option.value auth_value ~default:"NONE");
  [%expect {| authorization_header: Bearer test-token-123 |}]

let%expect_test "BearerAuth.apply_auth - correct Bearer format" =
  let auth = Bearer.create "abc123xyz" in
  let headers = Cohttp.Header.init () in
  let updated_headers = Bearer.apply_auth auth headers in
  match Cohttp.Header.get updated_headers "authorization" with
  | Some value ->
    let has_bearer_prefix = String.is_prefix value ~prefix:"Bearer " in
    let has_token = String.is_substring value ~substring:"abc123xyz" in
    printf "has_bearer_prefix: %b\n" has_bearer_prefix;
    printf "has_token: %b\n" has_token;
    [%expect {|
      has_bearer_prefix: true
      has_token: true |}]
  | None ->
    printf "ERROR: No authorization header\n";
    [%expect.unreachable]

let%expect_test "BearerAuth.apply_auth - preserves existing headers" =
  let auth = Bearer.create "token456" in
  let headers =
    Cohttp.Header.of_list
      [ ("content-type", "application/json"); ("x-custom", "test") ]
  in
  let updated_headers = Bearer.apply_auth auth headers in
  let content_type = Cohttp.Header.get updated_headers "content-type" in
  let custom = Cohttp.Header.get updated_headers "x-custom" in
  let auth_value = Cohttp.Header.get updated_headers "authorization" in
  printf "content_type: %s\n" (Option.value content_type ~default:"NONE");
  printf "x_custom: %s\n" (Option.value custom ~default:"NONE");
  printf "has_authorization: %b\n" (Option.is_some auth_value);
  [%expect
    {|
    content_type: application/json
    x_custom: test
    has_authorization: true |}]

let%expect_test "BearerAuth.apply_auth - multiple applications" =
  let auth = Bearer.create "persistent-token" in
  let headers1 = Cohttp.Header.init () in
  let headers2 = Cohttp.Header.init () in
  let updated1 = Bearer.apply_auth auth headers1 in
  let updated2 = Bearer.apply_auth auth headers2 in
  let auth1 = Cohttp.Header.get updated1 "authorization" in
  let auth2 = Cohttp.Header.get updated2 "authorization" in
  printf "both_have_auth: %b\n" (Option.is_some auth1 && Option.is_some auth2);
  printf "auth_values_equal: %b\n" (Option.equal String.equal auth1 auth2);
  [%expect {|
    both_have_auth: true
    auth_values_equal: true |}]

(* =============================================================================
   Test: BearerAuth.get_token
   ============================================================================= *)

let%expect_test "BearerAuth.get_token - retrieves original token" =
  let original = "my-test-token-789" in
  let auth = Bearer.create original in
  let retrieved = Bearer.get_token auth in
  printf "tokens_equal: %b\n" (String.equal original retrieved);
  printf "retrieved_token: %s\n" retrieved;
  [%expect {|
    tokens_equal: true
    retrieved_token: my-test-token-789 |}]

let%expect_test "BearerAuth.get_token - handles special characters" =
  let token_with_special = "abc-123_def.456~xyz" in
  let auth = Bearer.create token_with_special in
  let retrieved = Bearer.get_token auth in
  printf "retrieved: %s\n" retrieved;
  [%expect {| retrieved: abc-123_def.456~xyz |}]

let%expect_test "BearerAuth.get_token - handles empty token" =
  let auth = Bearer.create "" in
  let retrieved = Bearer.get_token auth in
  printf "is_empty: %b\n" (String.is_empty retrieved);
  [%expect {| is_empty: true |}]

(* =============================================================================
   Test: BearerAuth Comparison
   ============================================================================= *)

let%expect_test "BearerAuth.compare - equal tokens" =
  let auth1 = Bearer.create "same-token" in
  let auth2 = Bearer.create "same-token" in
  let comparison = [%compare: Bearer.t] auth1 auth2 in
  printf "comparison_result: %d\n" comparison;
  [%expect {| comparison_result: 0 |}]

let%expect_test "BearerAuth.compare - different tokens" =
  let auth1 = Bearer.create "token-a" in
  let auth2 = Bearer.create "token-b" in
  let comparison = [%compare: Bearer.t] auth1 auth2 in
  printf "tokens_different: %b\n" (comparison <> 0);
  [%expect {| tokens_different: true |}]

(* =============================================================================
   Test: Secret_string Integration
   ============================================================================= *)

let%expect_test "Secret_string - redacts in sexp" =
  let secret = Secret_string.create "super-secret-value" in
  let sexp_str = Sexp.to_string ([%sexp_of: Secret_string.t] secret) in
  printf "sexp_value: %s\n" sexp_str;
  [%expect {| sexp_value: <REDACTED> |}]

let%expect_test "Secret_string.get_secret_value - retrieves value" =
  let secret = Secret_string.create "my-secret" in
  let value = Secret_string.get_secret_value secret in
  printf "value: %s\n" value;
  [%expect {| value: my-secret |}]

let%expect_test "Secret_string.equal - compares correctly" =
  let secret1 = Secret_string.create "same" in
  let secret2 = Secret_string.create "same" in
  let secret3 = Secret_string.create "different" in
  printf "equal_same: %b\n" (Secret_string.equal secret1 secret2);
  printf "equal_different: %b\n" (Secret_string.equal secret1 secret3);
  [%expect {|
    equal_same: true
    equal_different: false |}]

let%expect_test "Secret_string.length - returns correct length" =
  let secret = Secret_string.create "12345" in
  let len = Secret_string.length secret in
  printf "length: %d\n" len;
  [%expect {| length: 5 |}]

let%expect_test "Secret_string.is_empty - detects empty string" =
  let empty = Secret_string.create "" in
  let non_empty = Secret_string.create "x" in
  printf "empty_is_empty: %b\n" (Secret_string.is_empty empty);
  printf "non_empty_is_empty: %b\n" (Secret_string.is_empty non_empty);
  [%expect {|
    empty_is_empty: true
    non_empty_is_empty: false |}]

let%expect_test "Secret_string.to_string - returns redacted" =
  let secret = Secret_string.create "should-not-appear" in
  let str = Secret_string.to_string secret in
  printf "to_string: %s\n" str;
  [%expect {| to_string: <REDACTED> |}]

(* =============================================================================
   Test: Edge Cases
   ============================================================================= *)

let%expect_test "BearerAuth - long token" =
  let long_token = String.make 1000 'x' in
  let auth = Bearer.create long_token in
  let retrieved = Bearer.get_token auth in
  printf "length_matches: %b\n" (String.length retrieved = 1000);
  [%expect {| length_matches: true |}]

let%expect_test "BearerAuth - token with spaces" =
  (* Some OAuth tokens might have spaces, though uncommon *)
  let token = "token with spaces" in
  let auth = Bearer.create token in
  let headers = Cohttp.Header.init () in
  let updated = Bearer.apply_auth auth headers in
  match Cohttp.Header.get updated "authorization" with
  | Some value ->
    printf "header_value: %s\n" value;
    [%expect {| header_value: Bearer token with spaces |}]
  | None -> printf "NONE\n"

let%expect_test "BearerAuth - unicode token" =
  let token = "token-世界-🔒" in
  let auth = Bearer.create token in
  let retrieved = Bearer.get_token auth in
  printf "tokens_equal: %b\n" (String.equal token retrieved);
  [%expect {| tokens_equal: true |}]
