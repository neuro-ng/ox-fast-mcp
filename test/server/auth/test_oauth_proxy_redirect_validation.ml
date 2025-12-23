(** Tests for OAuthProxy redirect URI validation.

    Translation of Python test_oauth_proxy_redirect_validation.py.
    
    Tests the integration between OxFastMCP OAuth Proxy and redirect URI
    validation, including:
    - ProxyDCRClient redirect URI validation
    - OAuthProxy configuration of allowed_client_redirect_uris
    - Client registration with redirect patterns *)

open! Core
open Lwt.Syntax
open Server_auth.Oauth_proxy

[@@@alert "-unsafe_multidomain"]

(* ============================================================================
   Helper Functions
   ============================================================================ *)

(** Create a test proxy with given redirect URI patterns *)
let create_test_proxy ?allowed_client_redirect_uris () =
  create ~upstream_authorization_endpoint:"https://auth.example.com/authorize"
    ~upstream_token_endpoint:"https://auth.example.com/token"
    ~upstream_client_id:"test-client" ~upstream_client_secret:"test-secret"
    ~base_url:"http://localhost:8000" ?allowed_client_redirect_uris
    ~required_scopes:[ "read" ] ()

(* ============================================================================
   TestProxyDCRClient - ProxyDCRClient redirect URI validation
   ============================================================================ *)

let%expect_test "ProxyDCRClient - default allows all (DCR compatibility)" =
  (* Test that default configuration allows all URIs for DCR compatibility.
     When allowed_redirect_uri_patterns is None, any URI should be accepted. *)
  let client =
    {
      pdc_client_id = "test";
      client_secret = Some "secret";
      redirect_uris = [ "http://localhost:3000" ];
      allowed_redirect_uri_patterns = None;  (* Default: allow all *)
      client_name = None;
      pdc_created_at = 0.0;
    }
  in
  (* With None patterns, all should be allowed *)
  let test uri =
    let result = Server_auth.Redirect_validation.validate_redirect_uri
      ~redirect_uri:(Some uri)
      ~allowed_patterns:client.allowed_redirect_uri_patterns
    in
    printf "%s: %b\n" uri result
  in
  test "http://localhost:3000";
  test "http://localhost:8080";
  test "http://127.0.0.1:3000";
  test "http://example.com";
  test "https://claude.ai/api/mcp/auth_callback";
  [%expect
    {|
    http://localhost:3000: true
    http://localhost:8080: true
    http://127.0.0.1:3000: true
    http://example.com: true
    https://claude.ai/api/mcp/auth_callback: true
    |}]

let%expect_test "ProxyDCRClient - custom patterns restrict URIs" =
  (* Test custom redirect URI patterns *)
  let client =
    {
      pdc_client_id = "test";
      client_secret = Some "secret";
      redirect_uris = [ "http://localhost:3000" ];
      allowed_redirect_uri_patterns =
        Some [ "http://localhost:*"; "https://app.example.com/*" ];
      client_name = None;
      pdc_created_at = 0.0;
    }
  in
  let test uri =
    let result = Server_auth.Redirect_validation.validate_redirect_uri
      ~redirect_uri:(Some uri)
      ~allowed_patterns:client.allowed_redirect_uri_patterns
    in
    printf "%s: %b\n" uri result
  in
  (* Allowed by patterns *)
  test "http://localhost:3000";
  test "https://app.example.com/callback";
  (* Not allowed by patterns *)
  test "http://127.0.0.1:3000";
  test "https://other.example.com/callback";
  [%expect
    {|
    http://localhost:3000: true
    https://app.example.com/callback: true
    http://127.0.0.1:3000: false
    https://other.example.com/callback: false
    |}]

let%expect_test "ProxyDCRClient - empty list allows none" =
  (* Test that empty pattern list allows no URIs *)
  let client =
    {
      pdc_client_id = "test";
      client_secret = Some "secret";
      redirect_uris = [ "http://localhost:3000" ];
      allowed_redirect_uri_patterns = Some [];  (* Empty = allow none *)
      client_name = None;
      pdc_created_at = 0.0;
    }
  in
  let test uri =
    let result = Server_auth.Redirect_validation.validate_redirect_uri
      ~redirect_uri:(Some uri)
      ~allowed_patterns:client.allowed_redirect_uri_patterns
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

let%expect_test "ProxyDCRClient - None redirect_uri uses default behavior" =
  (* Test that None redirect URI uses default behavior *)
  let valid = Server_auth.Redirect_validation.validate_redirect_uri
    ~redirect_uri:None
    ~allowed_patterns:(Some [ "http://localhost:*" ])
  in
  printf "None redirect_uri allowed: %b\n" valid;
  [%expect {| None redirect_uri allowed: true |}]

(* ============================================================================
   TestOAuthProxyRedirectValidation - OAuth proxy with redirect URI validation
   ============================================================================ *)

let%expect_test "OAuthProxy - default allowed_client_redirect_uris is None" =
  (* Test that OAuth proxy defaults to allowing all URIs for DCR compatibility.
     
     Note: OCaml Oauth_proxy.t is abstract, so we can only test behavior,
     not internal field values. We test via validate_redirect_uri behavior. *)
  let proxy = create_test_proxy () in
  (* Create a dummy client *)
  let client =
    {
      pdc_client_id = "test";
      client_secret = None;
      redirect_uris = [];
      allowed_redirect_uri_patterns = None;
      client_name = None;
      pdc_created_at = 0.0;
    }
  in
  (* With default config, any redirect should be valid *)
  let result = validate_redirect_uri proxy ~client ~redirect_uri:"http://example.com" in
  printf "Default allows any URI: %b\n" result;
  [%expect {| Default allows any URI: true |}]

let%expect_test "OAuthProxy - custom patterns restrict validation" =
  (* Test OAuth proxy with custom redirect patterns *)
  let custom_patterns = [ "http://localhost:*"; "https://*.myapp.com/*" ] in
  let proxy = create_test_proxy ~allowed_client_redirect_uris:custom_patterns () in
  let client =
    {
      pdc_client_id = "test";
      client_secret = None;
      redirect_uris = [];
      allowed_redirect_uri_patterns = None;
      client_name = None;
      pdc_created_at = 0.0;
    }
  in
  let test uri =
    let result = validate_redirect_uri proxy ~client ~redirect_uri:uri in
    printf "%s: %b\n" uri result
  in
  test "http://localhost:3000";
  test "https://api.myapp.com/callback";
  (* Should be rejected *)
  test "http://127.0.0.1:3000";
  test "http://example.com";
  [%expect
    {|
    http://localhost:3000: true
    https://api.myapp.com/callback: true
    http://127.0.0.1:3000: false
    http://example.com: false
    |}]

let%expect_test "OAuthProxy - empty list validation rejects all" =
  (* Test OAuth proxy with empty list (allow none) *)
  let proxy = create_test_proxy ~allowed_client_redirect_uris:[] () in
  let client =
    {
      pdc_client_id = "test";
      client_secret = None;
      redirect_uris = [];
      allowed_redirect_uri_patterns = None;
      client_name = None;
      pdc_created_at = 0.0;
    }
  in
  let test uri =
    let result = validate_redirect_uri proxy ~client ~redirect_uri:uri in
    printf "%s: %b\n" uri result
  in
  test "http://localhost:3000";
  test "http://example.com";
  [%expect
    {|
    http://localhost:3000: false
    http://example.com: false
    |}]

let%expect_test "OAuthProxy - register and get client" =
  Lwt_main.run
    (let proxy = create_test_proxy ~allowed_client_redirect_uris:[ "https://app.example.com/*" ] () in
     (* Register a client *)
     let* _registered =
       register_client proxy ~client_id:"new-client" ~client_secret:"secret"
         ~redirect_uris:[ "https://app.example.com/callback" ] ()
     in
     print_endline "Client registered";
     (* Get the registered client *)
     let* retrieved = get_client proxy ~client_id:"new-client" in
     (match retrieved with
     | None -> print_endline "Client not found"
     | Some client ->
       printf "Client ID: %s\n" client.pdc_client_id;
       printf "Has redirect_uris: %b\n" (not (List.is_empty client.redirect_uris)));
     Lwt.return ());
  [%expect
    {|
    Client registered
    Client ID: new-client
    Has redirect_uris: true
    |}]

let%expect_test "OAuthProxy - unregistered client returns None" =
  Lwt_main.run
    (let proxy = create_test_proxy () in
     let* client = get_client proxy ~client_id:"unknown-client" in
     (match client with
     | None -> print_endline "Unregistered client returns None"
     | Some _ -> print_endline "ERROR: Found unregistered client");
     Lwt.return ());
  [%expect {| Unregistered client returns None |}]
