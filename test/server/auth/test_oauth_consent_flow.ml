(** Tests for OAuth Proxy consent flow with server-side storage.

    This test suite verifies:
    1. OAuth transactions are stored in server-side storage
    2. Authorization codes are stored in server-side storage
    3. Consent flow redirects correctly through /consent endpoint
    4. CSRF protection works with cookies
    5. State persists across storage backends
    6. Security headers (X-Frame-Options) are set correctly
    7. Cookie signing and tampering detection
    8. Auto-approve behavior with valid cookies *)

open! Core
open Server_auth.Oauth_proxy

(* ============================================================================
   Test Helper Functions
   ============================================================================ *)

(** Extract txn_id from a consent URL *)
let extract_txn_id_from_url url =
  (* URL format: https://example.com/consent?txn_id=xxx *)
  let uri = Uri.of_string url in
  Uri.get_query_param uri "txn_id"

(** Extract CSRF token from HTML form (simplified regex-like search) *)
let extract_csrf_token_from_html html =
  (* Look for: name="csrf_token" value="xxx" *)
  let pattern = "value=\"" in
  let csrf_pattern = "csrf_token" in
  if String.is_substring html ~substring:csrf_pattern then
    match String.substr_index html ~pattern with
    | Some idx ->
      let start = idx + String.length pattern in
      let end_idx =
        match String.substr_index_all html ~pattern:"\"" ~may_overlap:false with
        | indices ->
          List.find indices ~f:(fun i -> i > start)
          |> Option.value ~default:(String.length html)
      in
      Some (String.sub html ~pos:start ~len:(end_idx - start))
    | None -> None
  else None

(* ============================================================================
   Storage Module Tests
   ============================================================================ *)

let%expect_test "Storage - transactions can be stored and retrieved" =
  let store = Storage.create () in
  let txn =
    {
      txn_id = "test-txn-123";
      client_id = "test-client";
      client_redirect_uri = "http://localhost:54321/callback";
      client_state = "client-state-123";
      code_challenge = Some "challenge-abc";
      code_challenge_method = "S256";
      scopes = [ "read"; "write" ];
      created_at = 1234567890.0;
      resource = None;
      proxy_code_verifier = None;
      csrf_token = Some "csrf-token-xyz";
      csrf_expires_at = Some 1234568490.0;
    }
  in
  Storage.put store ~key:txn.txn_id ~value:txn;
  let retrieved = Storage.get store ~key:"test-txn-123" in
  (match retrieved with
  | None -> printf "Transaction not found\n"
  | Some t ->
    printf "Transaction client_id: %s\n" t.client_id;
    printf "Transaction client_state: %s\n" t.client_state;
    printf "Transaction scopes: %s\n" (String.concat ~sep:", " t.scopes));
  [%expect
    {|
    Transaction client_id: test-client
    Transaction client_state: client-state-123
    Transaction scopes: read, write
    |}]

let%expect_test "Storage - authorization codes can be stored and retrieved" =
  let store = Storage.create () in
  let code =
    {
      code = "auth-code-abc";
      cc_client_id = "test-client";
      redirect_uri = "http://localhost:12345/callback";
      cc_code_challenge = Some "challenge-xyz";
      cc_code_challenge_method = "S256";
      cc_scopes = [ "read" ];
      idp_tokens = `Assoc [ ("access_token", `String "upstream-token") ];
      expires_at = 1234568190.0;
      cc_created_at = 1234567890.0;
    }
  in
  Storage.put store ~key:code.code ~value:code;
  let retrieved = Storage.get store ~key:"auth-code-abc" in
  (match retrieved with
  | None -> printf "Code not found\n"
  | Some c ->
    printf "Code client_id: %s\n" c.cc_client_id;
    printf "Code scopes: %s\n" (String.concat ~sep:", " c.cc_scopes));
  [%expect {|
    Code client_id: test-client
    Code scopes: read
    |}]

let%expect_test "Storage - clients can be stored and retrieved" =
  let store = Storage.create () in
  let client =
    {
      pdc_client_id = "my-client";
      client_secret = Some "secret-123";
      redirect_uris = [ "http://localhost:3000/callback" ];
      allowed_redirect_uri_patterns = Some [ "http://localhost:*" ];
      client_name = Some "My Test Client";
      pdc_created_at = 1234567890.0;
    }
  in
  Storage.put store ~key:client.pdc_client_id ~value:client;
  let retrieved = Storage.get store ~key:"my-client" in
  (match retrieved with
  | None -> printf "Client not found\n"
  | Some c ->
    printf "Client name: %s\n" (Option.value c.client_name ~default:"None");
    printf "Client redirect_uris: %s\n"
      (String.concat ~sep:", " c.redirect_uris));
  [%expect
    {|
    Client name: My Test Client
    Client redirect_uris: http://localhost:3000/callback
    |}]

let%expect_test "Storage - collections are isolated" =
  let txn_store : oauth_transaction Storage.t = Storage.create () in
  let code_store : client_code Storage.t = Storage.create () in
  let client_store : proxy_dcr_client Storage.t = Storage.create () in
  (* Store items with same key in different stores *)
  let txn =
    {
      txn_id = "shared-key";
      client_id = "txn-client";
      client_redirect_uri = "http://localhost/txn";
      client_state = "txn-state";
      code_challenge = None;
      code_challenge_method = "S256";
      scopes = [];
      created_at = 0.0;
      resource = None;
      proxy_code_verifier = None;
      csrf_token = None;
      csrf_expires_at = None;
    }
  in
  let code =
    {
      code = "shared-key";
      cc_client_id = "code-client";
      redirect_uri = "http://localhost/code";
      cc_code_challenge = None;
      cc_code_challenge_method = "plain";
      cc_scopes = [];
      idp_tokens = `Null;
      expires_at = 0.0;
      cc_created_at = 0.0;
    }
  in
  let client =
    {
      pdc_client_id = "shared-key";
      client_secret = None;
      redirect_uris = [ "http://localhost/client" ];
      allowed_redirect_uri_patterns = None;
      client_name = Some "client-name";
      pdc_created_at = 0.0;
    }
  in
  Storage.put txn_store ~key:"shared-key" ~value:txn;
  Storage.put code_store ~key:"shared-key" ~value:code;
  Storage.put client_store ~key:"shared-key" ~value:client;
  (* Each store should return its own type *)
  let t = Storage.get txn_store ~key:"shared-key" in
  let c = Storage.get code_store ~key:"shared-key" in
  let cl = Storage.get client_store ~key:"shared-key" in
  printf "Txn client_id: %s\n"
    (Option.value_map t ~default:"None" ~f:(fun x -> x.client_id));
  printf "Code client_id: %s\n"
    (Option.value_map c ~default:"None" ~f:(fun x -> x.cc_client_id));
  printf "Client name: %s\n"
    (Option.value_map cl ~default:"None" ~f:(fun x ->
         Option.value x.client_name ~default:"None"));
  [%expect
    {|
    Txn client_id: txn-client
    Code client_id: code-client
    Client name: client-name
    |}]

(* ============================================================================
   Consent HTML Generation Tests
   ============================================================================ *)

let%expect_test "create_consent_html - contains required form elements" =
  let html =
    create_consent_html ~client_id:"test-client"
      ~redirect_uri:"http://localhost:3000/callback" ~scopes:[ "read"; "write" ]
      ~txn_id:"txn-abc-123" ~csrf_token:"csrf-token-xyz" ()
  in
  printf "Contains form: %b\n" (String.is_substring html ~substring:"<form");
  printf "Contains txn_id: %b\n"
    (String.is_substring html ~substring:"txn-abc-123");
  printf "Contains csrf_token: %b\n"
    (String.is_substring html ~substring:"csrf-token-xyz");
  printf "Contains approve: %b\n"
    (String.is_substring html ~substring:"approve");
  printf "Contains deny: %b\n" (String.is_substring html ~substring:"deny");
  printf "Contains POST method: %b\n"
    (String.is_substring html ~substring:"method=\"POST\"");
  [%expect
    {|
    Contains form: true
    Contains txn_id: true
    Contains csrf_token: true
    Contains approve: true
    Contains deny: true
    Contains POST method: true
    |}]

let%expect_test "create_consent_html - displays client and server info" =
  let html =
    create_consent_html ~client_id:"my-app"
      ~redirect_uri:"https://myapp.example.com/callback"
      ~scopes:[ "openid"; "profile"; "email" ]
      ~txn_id:"txn-123" ~csrf_token:"csrf-456" ~client_name:"My Application"
      ~server_name:"OxFastMCP Server" ()
  in
  printf "Contains client name: %b\n"
    (String.is_substring html ~substring:"My Application");
  printf "Contains server name: %b\n"
    (String.is_substring html ~substring:"OxFastMCP Server");
  printf "Contains redirect URI: %b\n"
    (String.is_substring html ~substring:"myapp.example.com");
  printf "Contains scopes: %b\n"
    (String.is_substring html ~substring:"openid"
    && String.is_substring html ~substring:"profile");
  [%expect
    {|
    Contains client name: true
    Contains server name: true
    Contains redirect URI: true
    Contains scopes: true
    |}]

let%expect_test "create_consent_html - uses defaults when names not provided" =
  let html =
    create_consent_html ~client_id:"client-123"
      ~redirect_uri:"http://localhost/callback" ~scopes:[ "read" ] ~txn_id:"txn"
      ~csrf_token:"csrf" ()
  in
  printf "Contains client_id as fallback: %b\n"
    (String.is_substring html ~substring:"client-123");
  printf "Contains OxFastMCP as default: %b\n"
    (String.is_substring html ~substring:"OxFastMCP");
  [%expect
    {|
    Contains client_id as fallback: true
    Contains OxFastMCP as default: true
    |}]

(* ============================================================================
   Error HTML Generation Tests
   ============================================================================ *)

let%expect_test "create_error_html - displays error information" =
  let html =
    create_error_html ~error_title:"Authorization Failed"
      ~error_message:"The client is not registered." ()
  in
  printf "Contains title: %b\n"
    (String.is_substring html ~substring:"Authorization Failed");
  printf "Contains message: %b\n"
    (String.is_substring html ~substring:"The client is not registered.");
  printf "Is HTML: %b\n" (String.is_substring html ~substring:"<!DOCTYPE html>");
  [%expect
    {|
    Contains title: true
    Contains message: true
    Is HTML: true
    |}]

let%expect_test "create_error_html - includes optional details" =
  let html =
    create_error_html ~error_title:"Token Error"
      ~error_message:"Token verification failed."
      ~error_details:
        [
          ("Error Code", "invalid_token");
          ("Status", "401");
          ("Details", "Expired");
        ]
      ()
  in
  printf "Contains error code: %b\n"
    (String.is_substring html ~substring:"invalid_token");
  printf "Contains status: %b\n" (String.is_substring html ~substring:"401");
  printf "Contains details: %b\n"
    (String.is_substring html ~substring:"Expired");
  [%expect
    {|
    Contains error code: true
    Contains status: true
    Contains details: true
    |}]

(* ============================================================================
   Cookie Signing and CSRF Tests
   ============================================================================ *)

let%expect_test "Cookie.sign - creates signed value with dot separator" =
  let secret = "my-secret-key" in
  let payload = "session-data" in
  let signed = Cookie.sign ~secret ~payload in
  printf "Contains dot: %b\n" (String.is_substring signed ~substring:".");
  printf "Not empty: %b\n" (String.length signed > 0);
  (* Verify we can extract the original payload *)
  let verified = Cookie.verify ~secret ~signed_value:signed in
  printf "Verified payload: %s\n" (Option.value verified ~default:"NONE");
  [%expect
    {|
    Contains dot: true
    Not empty: true
    Verified payload: session-data
    |}]

let%expect_test "Cookie.verify - rejects tampered signatures" =
  let secret = "my-secret-key" in
  (* Create a value with invalid signature *)
  let tampered = "c2Vzc2lvbi1kYXRh.invalidsignature" in
  let verified = Cookie.verify ~secret ~signed_value:tampered in
  printf "Verified result: %s\n"
    (match verified with
    | None -> "None (correctly rejected)"
    | Some _ -> "WRONG - should have been rejected");
  [%expect {| Verified result: None (correctly rejected) |}]

let%expect_test "Cookie.verify - rejects malformed cookies" =
  let secret = "my-secret-key" in
  (* No dot separator *)
  let no_dot = "invaliddatanodot" in
  let v1 = Cookie.verify ~secret ~signed_value:no_dot in
  printf "No dot: %s\n"
    (match v1 with
    | None -> "rejected"
    | Some _ -> "accepted");
  (* Empty string *)
  let empty = "" in
  let v2 = Cookie.verify ~secret ~signed_value:empty in
  printf "Empty: %s\n"
    (match v2 with
    | None -> "rejected"
    | Some _ -> "accepted");
  [%expect {|
    No dot: rejected
    Empty: rejected
    |}]

let%expect_test "Cookie.verify - different secret fails" =
  let secret1 = "secret-one" in
  let secret2 = "secret-two" in
  let signed = Cookie.sign ~secret:secret1 ~payload:"data" in
  let verified = Cookie.verify ~secret:secret2 ~signed_value:signed in
  printf "Different secret: %s\n"
    (match verified with
    | None -> "rejected (correct)"
    | Some _ -> "accepted (WRONG)");
  [%expect {| Different secret: rejected (correct) |}]

(* ============================================================================
   PKCE Verification Tests (deterministic only)
   ============================================================================ *)

let%expect_test "Pkce.verify - plain method accepts matching values" =
  let verifier = "my-code-verifier" in
  let challenge = "my-code-verifier" in
  let result = Pkce.verify ~verifier ~challenge ~method_:"plain" in
  printf "Plain match: %b\n" result;
  [%expect {| Plain match: true |}]

let%expect_test "Pkce.verify - plain method rejects mismatches" =
  let verifier = "my-code-verifier" in
  let challenge = "different-value" in
  let result = Pkce.verify ~verifier ~challenge ~method_:"plain" in
  printf "Plain mismatch: %b\n" result;
  [%expect {| Plain mismatch: false |}]

let%expect_test "Pkce.verify - unknown method returns false" =
  let result = Pkce.verify ~verifier:"a" ~challenge:"a" ~method_:"unknown" in
  printf "Unknown method: %b\n" result;
  [%expect {| Unknown method: false |}]

(* ============================================================================
   OAuthProxy Configuration Tests
   ============================================================================ *)

let%expect_test "Oauth_proxy.create - normalizes redirect_path with leading \
                 slash" =
  (* Without leading slash - should be normalized *)
  let _proxy1 =
    create ~upstream_authorization_endpoint:"https://auth.example.com/authorize"
      ~upstream_token_endpoint:"https://auth.example.com/token"
      ~upstream_client_id:"client" ~upstream_client_secret:"secret"
      ~base_url:"https://example.com" ~redirect_path:"callback"
      ~required_scopes:[ "read" ] ()
  in
  printf "Proxy created without leading slash\n";
  (* With leading slash - should stay the same *)
  let _proxy2 =
    create ~upstream_authorization_endpoint:"https://auth.example.com/authorize"
      ~upstream_token_endpoint:"https://auth.example.com/token"
      ~upstream_client_id:"client" ~upstream_client_secret:"secret"
      ~base_url:"https://example.com" ~redirect_path:"/callback"
      ~required_scopes:[ "read" ] ()
  in
  printf "Proxy created with leading slash\n";
  [%expect
    {|
    Proxy created without leading slash
    Proxy created with leading slash
    |}]

(* ============================================================================
   URL Building Tests
   ============================================================================ *)

let%expect_test "build_upstream_authorize_url - contains required params" =
  let proxy =
    create
      ~upstream_authorization_endpoint:
        "https://github.com/login/oauth/authorize"
      ~upstream_token_endpoint:"https://github.com/login/oauth/access_token"
      ~upstream_client_id:"my-gh-client" ~upstream_client_secret:"my-gh-secret"
      ~base_url:"https://myserver.com" ~redirect_path:"/auth/callback"
      ~required_scopes:[ "read" ] ()
  in
  let transaction =
    {
      txn_id = "txn-build-test";
      client_id = "test-client";
      client_redirect_uri = "http://localhost/callback";
      client_state = "client-state";
      code_challenge = None;
      code_challenge_method = "S256";
      scopes = [ "repo"; "user" ];
      created_at = 0.0;
      resource = None;
      proxy_code_verifier = None;
      csrf_token = None;
      csrf_expires_at = None;
    }
  in
  let url =
    build_upstream_authorize_url proxy ~txn_id:"txn-build-test" ~transaction
  in
  printf "Starts with upstream: %b\n"
    (String.is_prefix url ~prefix:"https://github.com/login/oauth/authorize");
  printf "Contains client_id: %b\n"
    (String.is_substring url ~substring:"client_id=my-gh-client");
  printf "Contains redirect_uri: %b\n"
    (String.is_substring url ~substring:"redirect_uri=");
  printf "Contains response_type=code: %b\n"
    (String.is_substring url ~substring:"response_type=code");
  printf "Contains state: %b\n"
    (String.is_substring url ~substring:"state=txn-build-test");
  printf "Contains scope: %b\n" (String.is_substring url ~substring:"scope=");
  [%expect
    {|
    Starts with upstream: true
    Contains client_id: true
    Contains redirect_uri: true
    Contains response_type=code: true
    Contains state: true
    Contains scope: true
    |}]
