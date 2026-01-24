(** Tests for redirect URI validation in OAuth flows. *)

open! Core
open Server_auth.Redirect_validation

(** Test wildcard pattern matching for redirect URIs. *)
let%expect_test "matches_allowed_pattern - exact match" =
  let test uri pattern =
    let result = matches_allowed_pattern ~uri ~pattern in
    printf "%s matches %s: %b\n" uri pattern result
  in
  test "http://localhost:3000/callback" "http://localhost:3000/callback";
  test "http://localhost:3000/callback" "http://localhost:3001/callback";
  [%expect
    {|
    http://localhost:3000/callback matches http://localhost:3000/callback: true
    http://localhost:3000/callback matches http://localhost:3001/callback: false
    |}]

let%expect_test "matches_allowed_pattern - port wildcard" =
  let pattern = "http://localhost:*/callback" in
  let test uri =
    let result = matches_allowed_pattern ~uri ~pattern in
    printf "%s: %b\n" uri result
  in
  test "http://localhost:3000/callback";
  test "http://localhost:54321/callback";
  test "http://example.com:3000/callback";
  [%expect
    {|
    http://localhost:3000/callback: true
    http://localhost:54321/callback: true
    http://example.com:3000/callback: false
    |}]

let%expect_test "matches_allowed_pattern - path wildcard" =
  let pattern = "http://localhost:3000/*" in
  let test uri =
    let result = matches_allowed_pattern ~uri ~pattern in
    printf "%s: %b\n" uri result
  in
  test "http://localhost:3000/callback";
  test "http://localhost:3000/auth/callback";
  test "http://localhost:3001/callback";
  [%expect
    {|
    http://localhost:3000/callback: true
    http://localhost:3000/auth/callback: true
    http://localhost:3001/callback: false
    |}]

let%expect_test "matches_allowed_pattern - subdomain wildcard" =
  let pattern = "https://*.example.com/callback" in
  let test uri =
    let result = matches_allowed_pattern ~uri ~pattern in
    printf "%s: %b\n" uri result
  in
  test "https://app.example.com/callback";
  test "https://api.example.com/callback";
  test "https://example.com/callback";
  test "http://app.example.com/callback";
  [%expect
    {|
    https://app.example.com/callback: true
    https://api.example.com/callback: true
    https://example.com/callback: false
    http://app.example.com/callback: false
    |}]

let%expect_test "matches_allowed_pattern - multiple wildcards" =
  let pattern = "https://*.example.com:*/auth/*" in
  let test uri =
    let result = matches_allowed_pattern ~uri ~pattern in
    printf "%s: %b\n" uri result
  in
  test "https://app.example.com:8080/auth/callback";
  test "https://api.example.com:3000/auth/redirect";
  test "http://app.example.com:8080/auth/callback";
  [%expect
    {|
    https://app.example.com:8080/auth/callback: true
    https://api.example.com:3000/auth/redirect: true
    http://app.example.com:8080/auth/callback: false
    |}]

(** Test redirect URI validation with pattern lists. *)
let%expect_test "validate_redirect_uri - None redirect_uri allowed" =
  let test ~redirect_uri ~allowed_patterns =
    let result = validate_redirect_uri ~redirect_uri ~allowed_patterns in
    printf "redirect_uri=None, patterns=%s: %b\n"
      (match allowed_patterns with
      | None -> "None"
      | Some [] -> "[]"
      | Some _ -> "[...]")
      result
  in
  test ~redirect_uri:None ~allowed_patterns:None;
  test ~redirect_uri:None ~allowed_patterns:(Some []);
  test ~redirect_uri:None ~allowed_patterns:(Some [ "http://localhost:*" ]);
  [%expect
    {|
    redirect_uri=None, patterns=None: true
    redirect_uri=None, patterns=[]: true
    redirect_uri=None, patterns=[...]: true
    |}]

let%expect_test "validate_redirect_uri - None patterns allows all (DCR)" =
  let test uri =
    let result =
      validate_redirect_uri ~redirect_uri:(Some uri) ~allowed_patterns:None
    in
    printf "%s: %b\n" uri result
  in
  test "http://localhost:3000";
  test "http://127.0.0.1:8080";
  test "http://example.com";
  test "https://app.example.com";
  test "https://claude.ai/api/mcp/auth_callback";
  [%expect
    {|
    http://localhost:3000: true
    http://127.0.0.1:8080: true
    http://example.com: true
    https://app.example.com: true
    https://claude.ai/api/mcp/auth_callback: true
    |}]

let%expect_test "validate_redirect_uri - empty list allows none" =
  let test uri =
    let result =
      validate_redirect_uri ~redirect_uri:(Some uri) ~allowed_patterns:(Some [])
    in
    printf "%s: %b\n" uri result
  in
  test "http://localhost:3000";
  test "http://example.com";
  test "https://anywhere.com:9999/path";
  [%expect
    {|
    http://localhost:3000: false
    http://example.com: false
    https://anywhere.com:9999/path: false
    |}]

let%expect_test "validate_redirect_uri - custom patterns" =
  let patterns =
    Some
      [
        "http://localhost:*";
        "https://app.example.com/*";
        "https://*.trusted.io/*";
      ]
  in
  let test uri =
    let result =
      validate_redirect_uri ~redirect_uri:(Some uri) ~allowed_patterns:patterns
    in
    printf "%s: %b\n" uri result
  in
  (* Allowed URIs *)
  test "http://localhost:3000";
  test "https://app.example.com/callback";
  test "https://api.trusted.io/auth";
  (* Rejected URIs *)
  test "http://127.0.0.1:3000";
  test "https://other.example.com/callback";
  test "http://app.example.com/callback";
  [%expect
    {|
    http://localhost:3000: true
    https://app.example.com/callback: true
    https://api.trusted.io/auth: true
    http://127.0.0.1:3000: false
    https://other.example.com/callback: false
    http://app.example.com/callback: false
    |}]

(** Test the default localhost patterns constant. *)
let%expect_test "default_localhost_patterns - exist and are correct" =
  printf "Number of patterns: %d\n" (List.length default_localhost_patterns);
  List.iter default_localhost_patterns ~f:(printf "Pattern: %s\n");
  printf "Contains localhost: %b\n"
    (List.mem default_localhost_patterns "http://localhost:*"
       ~equal:String.equal);
  printf "Contains 127.0.0.1: %b\n"
    (List.mem default_localhost_patterns "http://127.0.0.1:*"
       ~equal:String.equal);
  [%expect
    {|
    Number of patterns: 2
    Pattern: http://localhost:*
    Pattern: http://127.0.0.1:*
    Contains localhost: true
    Contains 127.0.0.1: true
    |}]

let%expect_test "default_localhost_patterns - restricts to localhost" =
  let patterns = Some default_localhost_patterns in
  let test uri =
    let result =
      validate_redirect_uri ~redirect_uri:(Some uri) ~allowed_patterns:patterns
    in
    printf "%s: %b\n" uri result
  in
  (* Localhost should be allowed *)
  test "http://localhost:3000";
  test "http://127.0.0.1:8080";
  (* Non-localhost should be rejected *)
  test "http://example.com";
  test "https://claude.ai/api/mcp/auth_callback";
  [%expect
    {|
    http://localhost:3000: true
    http://127.0.0.1:8080: true
    http://example.com: false
    https://claude.ai/api/mcp/auth_callback: false
    |}]
