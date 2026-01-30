(** Tests for HTTP Auth Middleware integration.

    Translated from Python test_http_auth_middleware.py to OCaml. Tests that
    create_streamable_http_app properly integrates with auth middleware and
    RequireAuthMiddleware configuration. *)

open! Core
open! Expect_test_helpers_core
module Http = Ox_fast_mcp_server__Http
module Middleware = Server_auth.Middleware
module Jwt = Server_auth_providers.Jwt

(* =============================================================================
   Helper Functions
   ============================================================================= *)

(** Create a mock JWT verifier for testing (using static token verifier) *)
let create_mock_verifier ~_base_url () =
  (* Use Static_token_verifier as a stand-in since RSA key gen requires RNG *)
  let token_data =
    Jwt.Static_token_verifier.make_token_data ~client_id:"test-client"
      ~scopes:[ "read"; "write" ] ()
  in
  Jwt.Static_token_verifier.create
    ~tokens:[ ("test-token", token_data) ]
    ~required_scopes:[] ()

(** Create a require auth config for testing *)
let create_auth_config ?(required_scopes = []) ?resource_metadata_url () :
    Middleware.require_auth_config =
  { required_scopes; resource_metadata_url }

(* =============================================================================
   Test: Require Auth Config Creation
   ============================================================================= *)

let%expect_test "require_auth_config - create with default values" =
  let config = create_auth_config () in
  printf "required_scopes_count: %d\n" (List.length config.required_scopes);
  printf "has_resource_url: %b\n" (Option.is_some config.resource_metadata_url);
  [%expect {|
    required_scopes_count: 0
    has_resource_url: false
    |}]

let%expect_test "require_auth_config - create with scopes" =
  let config = create_auth_config ~required_scopes:[ "read"; "write" ] () in
  printf "required_scopes_count: %d\n" (List.length config.required_scopes);
  printf "first_scope: %s\n" (List.hd_exn config.required_scopes);
  [%expect {|
    required_scopes_count: 2
    first_scope: read
    |}]

let%expect_test "require_auth_config - create with resource_metadata_url" =
  let config =
    create_auth_config
      ~resource_metadata_url:
        "https://resource.example.com/.well-known/oauth-protected-resource"
      ()
  in
  printf "has_resource_url: %b\n" (Option.is_some config.resource_metadata_url);
  printf "resource_url: %s\n"
    (Option.value config.resource_metadata_url ~default:"none");
  [%expect
    {|
    has_resource_url: true
    resource_url: https://resource.example.com/.well-known/oauth-protected-resource
    |}]

(* =============================================================================
   Test: Require Auth Middleware Create
   ============================================================================= *)

let%expect_test "Require_auth_middleware.create - passes through config" =
  let config =
    create_auth_config ~required_scopes:[ "api.read" ]
      ~resource_metadata_url:"https://example.com/meta" ()
  in
  let result = Middleware.Require_auth_middleware.create config in
  printf "scopes_equal: %b\n"
    (List.equal String.equal result.required_scopes config.required_scopes);
  printf "url_equal: %b\n"
    (Option.equal String.equal result.resource_metadata_url
       config.resource_metadata_url);
  [%expect {|
    scopes_equal: true
    url_equal: true
    |}]

(* =============================================================================
   Test: Send Auth Error (401 Unauthorized)
   ============================================================================= *)

let%expect_test "send_auth_error - 401 returns unauthorized response" =
  let result =
    Lwt_main.run
      (Middleware.Require_auth_middleware.send_auth_error ~status:`Unauthorized
         ~error:"invalid_token" ~description:"Token expired"
         ~resource_metadata_url:None)
  in
  let response, body = result in
  let status_code =
    Cohttp.Response.status response |> Cohttp.Code.code_of_status
  in
  let body_str = Cohttp.Body.to_string body in
  printf "status: %d\n" status_code;
  printf "has_body: %b\n" (String.length body_str > 0);
  printf "has_error_in_body: %b\n"
    (String.is_substring body_str ~substring:"invalid_token");
  (* Check for WWW-Authenticate header *)
  let headers = Cohttp.Response.headers response in
  let www_auth = Cohttp.Header.get headers "www-authenticate" in
  printf "has_www_authenticate: %b\n" (Option.is_some www_auth);
  printf "www_auth_has_bearer: %b\n"
    (Option.value_map www_auth ~default:false ~f:(fun h ->
         String.is_prefix h ~prefix:"Bearer"));
  [%expect
    {|
    status: 401
    has_body: true
    has_error_in_body: true
    has_www_authenticate: true
    www_auth_has_bearer: true
    |}]

(* =============================================================================
   Test: Send Auth Error (403 Forbidden - Scope Error)
   ============================================================================= *)

let%expect_test "send_auth_error - 403 returns forbidden response" =
  let result =
    Lwt_main.run
      (Middleware.Require_auth_middleware.send_auth_error ~status:`Forbidden
         ~error:"insufficient_scope" ~description:"Required scope: admin"
         ~resource_metadata_url:None)
  in
  let response, body = result in
  let status_code =
    Cohttp.Response.status response |> Cohttp.Code.code_of_status
  in
  let body_str = Cohttp.Body.to_string body in
  printf "status: %d\n" status_code;
  printf "has_scope_error: %b\n"
    (String.is_substring body_str ~substring:"insufficient_scope");
  [%expect {|
    status: 403
    has_scope_error: true
    |}]

(* =============================================================================
   Test: Send Auth Error with Resource Metadata URL
   ============================================================================= *)

let%expect_test "send_auth_error - includes resource_metadata in header" =
  let result =
    Lwt_main.run
      (Middleware.Require_auth_middleware.send_auth_error ~status:`Unauthorized
         ~error:"invalid_token" ~description:"Auth required"
         ~resource_metadata_url:
           (Some "https://res.example.com/.well-known/oauth"))
  in
  let response, _body = result in
  let headers = Cohttp.Response.headers response in
  let www_auth = Cohttp.Header.get headers "www-authenticate" in
  printf "has_resource_metadata: %b\n"
    (Option.value_map www_auth ~default:false ~f:(fun h ->
         String.is_substring h ~substring:"resource_metadata="));
  [%expect {| has_resource_metadata: true |}]

(* =============================================================================
   Test: Enhanced Error Messages
   ============================================================================= *)

let%expect_test "send_auth_error - enhances invalid_token description" =
  let result =
    Lwt_main.run
      (Middleware.Require_auth_middleware.send_auth_error ~status:`Unauthorized
         ~error:"invalid_token" ~description:"Authentication required"
         ~resource_metadata_url:None)
  in
  let _response, body = result in
  let body_str = Cohttp.Body.to_string body in
  (* Enhanced message should mention "Authentication failed" and token issues *)
  printf "has_enhanced_message: %b\n"
    (String.is_substring body_str ~substring:"Authentication failed");
  printf "mentions_tokens: %b\n"
    (String.is_substring body_str ~substring:"token");
  [%expect {|
    has_enhanced_message: true
    mentions_tokens: true
    |}]

(* =============================================================================
   Test: HTTP App Creation with Auth Config
   ============================================================================= *)

let%expect_test "create_streamable_http_app - without auth creates routes" =
  let server = `Assoc [ ("name", `String "TestServer") ] in
  let app =
    Http.create_streamable_http_app ~server ~streamable_http_path:"/mcp" ()
  in
  printf "routes_count: %d\n" (List.length app.routes);
  printf "has_mcp_route: %b\n"
    (List.exists app.routes ~f:(fun r -> String.equal r.path "/mcp"));
  [%expect {|
    routes_count: 1
    has_mcp_route: true
    |}]

let%expect_test "create_streamable_http_app - route has correct methods" =
  let server = `Assoc [ ("name", `String "TestServer") ] in
  let app =
    Http.create_streamable_http_app ~server ~streamable_http_path:"/mcp" ()
  in
  let mcp_route =
    List.find_exn app.routes ~f:(fun r -> String.equal r.path "/mcp")
  in
  let methods =
    List.map mcp_route.methods ~f:Http.Http_method.to_string
    |> String.concat ~sep:", "
  in
  printf "methods: %s\n" methods;
  (* Should include GET, POST, DELETE for streamable-http *)
  printf "has_get: %b\n"
    (List.mem mcp_route.methods Http.Http_method.GET
       ~equal:Http.Http_method.equal);
  printf "has_post: %b\n"
    (List.mem mcp_route.methods Http.Http_method.POST
       ~equal:Http.Http_method.equal);
  printf "has_delete: %b\n"
    (List.mem mcp_route.methods Http.Http_method.DELETE
       ~equal:Http.Http_method.equal);
  [%expect
    {|
    methods: GET, POST, DELETE
    has_get: true
    has_post: true
    has_delete: true
    |}]

(* =============================================================================
   Test: Static Token Verifier (Mock JWT Verifier)
   ============================================================================= *)

let%expect_test "Static_token_verifier - create and lookup token" =
  let token_data =
    Jwt.Static_token_verifier.make_token_data ~client_id:"client123"
      ~scopes:[ "read"; "write" ] ()
  in
  let verifier =
    Jwt.Static_token_verifier.create
      ~tokens:[ ("valid-token", token_data) ]
      ~required_scopes:[ "read" ] ()
  in
  let found = Jwt.Static_token_verifier.get_token_data verifier "valid-token" in
  printf "token_found: %b\n" (Option.is_some found);
  printf "client_id: %s\n"
    (Option.value_map found ~default:"none" ~f:(fun d -> d.client_id));
  printf "scopes_count: %d\n"
    (Option.value_map found ~default:0 ~f:(fun d -> List.length d.scopes));
  [%expect
    {|
    token_found: true
    client_id: client123
    scopes_count: 2
    |}]

let%expect_test "Static_token_verifier - unknown token returns None" =
  let token_data =
    Jwt.Static_token_verifier.make_token_data ~client_id:"client" ()
  in
  let verifier =
    Jwt.Static_token_verifier.create ~tokens:[ ("valid", token_data) ] ()
  in
  let found =
    Jwt.Static_token_verifier.get_token_data verifier "invalid-token"
  in
  printf "token_found: %b\n" (Option.is_some found);
  [%expect {| token_found: false |}]

let%expect_test "Static_token_verifier - check_scopes with matching scopes" =
  let token_data =
    Jwt.Static_token_verifier.make_token_data ~client_id:"client"
      ~scopes:[ "read"; "write" ] ()
  in
  let verifier =
    Jwt.Static_token_verifier.create
      ~tokens:[ ("token", token_data) ]
      ~required_scopes:[ "read" ] ()
  in
  let has_scopes = Jwt.Static_token_verifier.check_scopes verifier token_data in
  printf "has_required_scopes: %b\n" has_scopes;
  [%expect {| has_required_scopes: true |}]

let%expect_test "Static_token_verifier - check_scopes with missing scopes" =
  let token_data =
    Jwt.Static_token_verifier.make_token_data ~client_id:"client"
      ~scopes:[ "read" ] ()
  in
  let verifier =
    Jwt.Static_token_verifier.create
      ~tokens:[ ("token", token_data) ]
      ~required_scopes:[ "admin" ] ()
  in
  let has_scopes = Jwt.Static_token_verifier.check_scopes verifier token_data in
  printf "has_required_scopes: %b\n" has_scopes;
  [%expect {| has_required_scopes: false |}]
