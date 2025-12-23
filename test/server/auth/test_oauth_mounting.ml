(** Tests for OAuth .well-known routes when OxFastMCP apps are mounted in parent apps.

    This test file validates the behavior of OAuth endpoints when the MCP app is
    deployed under a path prefix (e.g., /api/mcp).

    OCaml translation of Python test_oauth_mounting.py.

    Note: Full HTTP integration tests require Starlette/ASGI equivalents which are
    not yet implemented in OCaml. This file tests the modules that ARE currently
    available and documents expected behavior via stub tests. *)

open! Core
open Server_auth.Oauth_proxy

(* ============================================================================
   Helper Functions
   ============================================================================ *)

(** Create a standard test proxy with given base_url *)
let create_test_proxy ~base_url ?issuer_url () =
  create ~upstream_authorization_endpoint:"https://auth.example.com/authorize"
    ~upstream_token_endpoint:"https://auth.example.com/token"
    ~upstream_client_id:"test-client" ~upstream_client_secret:"test-secret"
    ~base_url ?issuer_url ~redirect_path:"/auth/callback"
    ~required_scopes:[ "read" ] ()

(** Create a standard test transaction *)
let create_test_transaction ~txn_id ~client_id ~scopes () =
  {
    txn_id;
    client_id;
    client_redirect_uri = "http://localhost:3000/callback";
    client_state = "test-state";
    code_challenge = Some "test-challenge";
    code_challenge_method = "S256";
    scopes;
    created_at = 0.0;
    resource = None;
    proxy_code_verifier = None;
    csrf_token = None;
    csrf_expires_at = None;
  }

(* ============================================================================
   OAuth Proxy Configuration Tests - Tested via Observable Behavior
   ============================================================================ *)

let%expect_test "Oauth_proxy - base_url with mount prefix in redirect" =
  (* Test that OAuth proxy uses base_url (with mount prefix) in redirect_uri *)
  let proxy = create_test_proxy ~base_url:"https://api.example.com/api" () in
  let transaction =
    create_test_transaction ~txn_id:"test-txn" ~client_id:"client"
      ~scopes:[ "read" ] ()
  in
  let url = build_upstream_authorize_url proxy ~txn_id:"test-txn" ~transaction in
  (* Check that URL contains base_url path (may be encoded or raw) *)
  let has_api_in_redirect =
    String.is_substring url ~substring:"api.example.com/api" ||
    String.is_substring url ~substring:"api.example.com%2Fapi"
  in
  printf "URL contains /api mount path: %b\n" has_api_in_redirect;
  (* Print a snippet showing the redirect_uri is set correctly *)
  let uri = Uri.of_string url in
  let redirect_uri = Uri.get_query_param uri "redirect_uri" in
  printf "Redirect URI param: %s\n"
    (Option.value redirect_uri ~default:"(not found)");
  [%expect
    {|
    URL contains /api mount path: true
    Redirect URI param: https://api.example.com/api/auth/callback
    |}]

let%expect_test "Oauth_proxy - nested mount path in redirect" =
  (* Test deeply nested mount: base_url = /outer/inner *)
  let proxy =
    create_test_proxy ~base_url:"https://api.example.com/outer/inner" ()
  in
  let transaction =
    create_test_transaction ~txn_id:"test-txn" ~client_id:"client"
      ~scopes:[ "read" ] ()
  in
  let url = build_upstream_authorize_url proxy ~txn_id:"test-txn" ~transaction in
  let uri = Uri.of_string url in
  let redirect_uri = Uri.get_query_param uri "redirect_uri" in
  printf "Redirect URI has nested path: %b\n"
    (match redirect_uri with
    | Some r -> String.is_substring r ~substring:"/outer/inner"
    | None -> false);
  [%expect {| Redirect URI has nested path: true |}]

let%expect_test "Oauth_proxy - redirect_path without leading slash normalized" =
  (* Test that redirect_path is normalized to start with / *)
  let proxy =
    create ~upstream_authorization_endpoint:"https://auth.example.com/authorize"
      ~upstream_token_endpoint:"https://auth.example.com/token"
      ~upstream_client_id:"test-client" ~upstream_client_secret:"test-secret"
      ~base_url:"https://example.com" ~redirect_path:"callback" (* No leading slash *)
      ~required_scopes:[ "read" ] ()
  in
  let transaction =
    create_test_transaction ~txn_id:"txn" ~client_id:"c" ~scopes:[ "read" ] ()
  in
  let url = build_upstream_authorize_url proxy ~txn_id:"txn" ~transaction in
  let uri = Uri.of_string url in
  let redirect_uri = Uri.get_query_param uri "redirect_uri" in
  (* Should contain /callback not callback without slash *)
  printf "Redirect URI ends with /callback: %b\n"
    (match redirect_uri with
    | Some r -> String.is_suffix r ~suffix:"/callback"
    | None -> false);
  [%expect {| Redirect URI ends with /callback: true |}]

(* ============================================================================
   URL Building Tests
   ============================================================================ *)

let%expect_test "build_upstream_authorize_url - includes all required params" =
  let proxy = create_test_proxy ~base_url:"https://api.example.com" () in
  let transaction =
    create_test_transaction ~txn_id:"txn-123" ~client_id:"test-client"
      ~scopes:[ "read"; "write" ] ()
  in
  let url = build_upstream_authorize_url proxy ~txn_id:"txn-123" ~transaction in
  let uri = Uri.of_string url in
  printf "Has client_id param: %b\n"
    (Option.is_some (Uri.get_query_param uri "client_id"));
  printf "Has response_type param: %b\n"
    (Option.is_some (Uri.get_query_param uri "response_type"));
  printf "Has state param: %b\n"
    (Option.is_some (Uri.get_query_param uri "state"));
  printf "Has scope param: %b\n"
    (Option.is_some (Uri.get_query_param uri "scope"));
  printf "State equals txn_id: %b\n"
    (match Uri.get_query_param uri "state" with
    | Some s -> String.equal s "txn-123"
    | None -> false);
  [%expect
    {|
    Has client_id param: true
    Has response_type param: true
    Has state param: true
    Has scope param: true
    State equals txn_id: true
    |}]

let%expect_test "build_upstream_authorize_url - scope encoding" =
  let proxy = create_test_proxy ~base_url:"https://api.example.com" () in
  let transaction =
    create_test_transaction ~txn_id:"txn" ~client_id:"c"
      ~scopes:[ "openid"; "profile"; "email" ] ()
  in
  let url = build_upstream_authorize_url proxy ~txn_id:"txn" ~transaction in
  let uri = Uri.of_string url in
  let scope = Uri.get_query_param uri "scope" in
  printf "Scope param: %s\n" (Option.value scope ~default:"(not found)");
  [%expect {| Scope param: openid profile email |}]

(* ============================================================================
   Storage Tests - URLs with Mount Prefix
   ============================================================================ *)

let%expect_test "Storage - preserves URLs with mount prefix" =
  let store = Storage.create () in
  let txn =
    {
      txn_id = "mount-test-txn";
      client_id = "test-client";
      client_redirect_uri = "http://localhost:3000/api/callback";
      client_state = "state";
      code_challenge = Some "challenge";
      code_challenge_method = "S256";
      scopes = [ "read" ];
      created_at = 0.0;
      resource = Some "https://api.example.com/api/mcp";
      proxy_code_verifier = None;
      csrf_token = None;
      csrf_expires_at = None;
    }
  in
  Storage.put store ~key:txn.txn_id ~value:txn;
  let retrieved = Storage.get store ~key:"mount-test-txn" in
  (match retrieved with
  | None -> printf "Transaction not found\n"
  | Some t ->
    printf "Redirect URI: %s\n" t.client_redirect_uri;
    printf "Resource: %s\n" (Option.value t.resource ~default:"None"));
  [%expect
    {|
    Redirect URI: http://localhost:3000/api/callback
    Resource: https://api.example.com/api/mcp
    |}]

let%expect_test "Storage - client codes with mount path" =
  let store : client_code Storage.t = Storage.create () in
  let code =
    {
      code = "mounted-code";
      cc_client_id = "test-client";
      redirect_uri = "https://myapp.example.com/api/callback";
      cc_code_challenge = Some "challenge";
      cc_code_challenge_method = "S256";
      cc_scopes = [ "read"; "write" ];
      idp_tokens = `Assoc [];
      expires_at = 9999999999.0;
      cc_created_at = 0.0;
    }
  in
  Storage.put store ~key:code.code ~value:code;
  let retrieved = Storage.get store ~key:"mounted-code" in
  (match retrieved with
  | None -> printf "Code not found\n"
  | Some c ->
    printf "Redirect URI preserved: %b\n"
      (String.equal c.redirect_uri "https://myapp.example.com/api/callback"));
  [%expect {| Redirect URI preserved: true |}]

(* ============================================================================
   RFC 9728 Well-Known Routes - Stub Tests
   These document expected behavior for functionality not yet implemented
   ============================================================================ *)

let%expect_test "[STUB] RFC 9728 - direct deployment well-known path" =
  (* RFC 9728: If resource is at /mcp, well-known endpoint should be at
     /.well-known/oauth-protected-resource/mcp (path-scoped)

     TODO: Implement get_well_known_routes in auth.ml *)
  printf "Expected: /.well-known/oauth-protected-resource/mcp\n";
  printf "Status: NOT IMPLEMENTED - requires HTTP routing infrastructure\n";
  [%expect
    {|
    Expected: /.well-known/oauth-protected-resource/mcp
    Status: NOT IMPLEMENTED - requires HTTP routing infrastructure
    |}]

let%expect_test "[STUB] RFC 9728 - mounted app well-known path" =
  (* RFC 9728: If app mounted at /api with resource at /mcp,
     well-known endpoint should be at:
     /.well-known/oauth-protected-resource/api/mcp *)
  printf "Expected: /.well-known/oauth-protected-resource/api/mcp\n";
  printf "Requires: get_well_known_routes implementation\n";
  printf "Status: NOT IMPLEMENTED\n";
  [%expect
    {|
    Expected: /.well-known/oauth-protected-resource/api/mcp
    Requires: get_well_known_routes implementation
    Status: NOT IMPLEMENTED
    |}]

let%expect_test "[STUB] RFC 9728 - nested mounting well-known path" =
  (* RFC 9728: Deeply nested mounts /outer/inner/mcp should yield:
     /.well-known/oauth-protected-resource/outer/inner/mcp *)
  printf "Expected: /.well-known/oauth-protected-resource/outer/inner/mcp\n";
  printf "Status: NOT IMPLEMENTED\n";
  [%expect
    {|
    Expected: /.well-known/oauth-protected-resource/outer/inner/mcp
    Status: NOT IMPLEMENTED
    |}]

(* ============================================================================
   Issue #2287 - OAuth Metadata Endpoint Tests (Stubs)
   ============================================================================ *)

let%expect_test "[STUB] Issue #2287 - endpoints at base_url not issuer_url" =
  (* Fix for issue #2287: When base_url and issuer_url differ,
     operational OAuth endpoints should be declared at base_url. *)
  printf "When base_url = https://api.example.com/api\n";
  printf "And issuer_url = https://api.example.com\n";
  printf "Expected authorization_endpoint: https://api.example.com/api/authorize\n";
  printf "Expected token_endpoint: https://api.example.com/api/token\n";
  printf "Status: Metadata generation NOT IMPLEMENTED\n";
  [%expect
    {|
    When base_url = https://api.example.com/api
    And issuer_url = https://api.example.com
    Expected authorization_endpoint: https://api.example.com/api/authorize
    Expected token_endpoint: https://api.example.com/api/token
    Status: Metadata generation NOT IMPLEMENTED
    |}]

(* ============================================================================
   Cookie and CSRF Tests for Mounted Flows
   ============================================================================ *)

let%expect_test "Cookie.sign and verify - works for mounted routes" =
  let secret = "mounted-app-secret" in
  let payload = "txn-id-for-mounted-app" in
  let signed = Cookie.sign ~secret ~payload in
  let verified = Cookie.verify ~secret ~signed_value:signed in
  (match verified with
  | None -> printf "Verification failed\n"
  | Some p -> printf "Verified payload: %s\n" p);
  [%expect {| Verified payload: txn-id-for-mounted-app |}]

(* ============================================================================
   Consent HTML Generation Tests
   ============================================================================ *)

let%expect_test "create_consent_html - renders for mounted deployment" =
  let html =
    create_consent_html ~client_id:"my-client"
      ~redirect_uri:"https://myapp.example.com/api/callback"
      ~scopes:[ "read"; "write" ] ~txn_id:"mount-txn" ~csrf_token:"csrf-abc"
      ~server_name:"OxFastMCP Mounted Server" ()
  in
  printf "HTML contains form: %b\n" (String.is_substring html ~substring:"<form");
  printf "HTML contains redirect URI: %b\n"
    (String.is_substring html ~substring:"/api/callback");
  printf "HTML contains server name: %b\n"
    (String.is_substring html ~substring:"OxFastMCP Mounted Server");
  [%expect
    {|
    HTML contains form: true
    HTML contains redirect URI: true
    HTML contains server name: true
    |}]
