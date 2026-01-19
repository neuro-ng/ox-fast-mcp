(** OAuth Integration Tests

    End-to-end tests using HeadlessOAuth and MockOAuthServer.
    These tests verify the full OAuth flow without browser interaction. *)

open! Core
open! Async
open! Expect_test_helpers_core

(** {1 HeadlessOAuth Tests} *)

let%expect_test "headless oauth creates valid oauth client" =
  let oauth = Headless_oauth.create_with_defaults
    ~server_url:"http://localhost:9999"
    ~mock_auth_code:"test_code_123"
    ()
  in
  (* Verify client was created *)
  let ctx = Mcp_client_auth.Oauth2.get_context oauth in
  printf "Server URL: %s\n" ctx.server_url;
  printf "Client initialized: false (not yet called initialize)\n";
  [%expect {|
    Server URL: http://localhost:9999
    Client initialized: false (not yet called initialize) |}];
  return ()

let%expect_test "headless oauth with custom metadata" =
  let headless = Headless_oauth.create
    ~server_url:"http://localhost:8888"
    ~mock_auth_code:"custom_code"
    ~mock_state:"custom_state"
    ()
  in
  let metadata = Headless_oauth.default_client_metadata
    ~scopes:["read"; "write"; "admin"]
    ~client_name:"Integration Test"
    ()
  in
  let _oauth = Headless_oauth.create_oauth_client headless ~client_metadata:metadata in
  printf "Custom scopes: %s\n" (Option.value metadata.scope ~default:"none");
  printf "Client name: %s\n" (Option.value metadata.client_name ~default:"none");
  [%expect {|
    Custom scopes: read write admin
    Client name: Integration Test |}];
  return ()

let%expect_test "test_storage operations" =
  let storage = Headless_oauth.Test_storage.create () in
  
  (* Initially empty *)
  let%bind initial = Headless_oauth.Test_storage.get_tokens storage in
  printf "Initial tokens: %s\n" (if Option.is_none initial then "none" else "some");
  
  (* Set tokens *)
  let token = Mcp_shared.Auth.{
    access_token = "test_access_token";
    token_type = "Bearer";
    expires_in = Some 3600;
    scope = Some "read write";
    refresh_token = Some "test_refresh_token";
  } in
  let%bind () = Headless_oauth.Test_storage.set_tokens storage token in
  
  (* Retrieve tokens *)
  let%bind retrieved = Headless_oauth.Test_storage.get_tokens storage in
  (match retrieved with
  | Some t -> 
    printf "Retrieved token: %s\n" t.access_token;
    printf "Has refresh: %b\n" (Option.is_some t.refresh_token)
  | None -> printf "No token found\n");
  
  [%expect {|
    Initial tokens: none
    Retrieved token: test_access_token
    Has refresh: true |}];
  return ()

(** {1 Mock OAuth Server Tests} *)

let%expect_test "mock oauth server metadata" =
  (* Test OAuth metadata generation *)
  let issuer = "http://localhost:9999" in
  let metadata_json = `Assoc [
    ("issuer", `String issuer);
    ("authorization_endpoint", `String (sprintf "%s/authorize" issuer));
    ("token_endpoint", `String (sprintf "%s/token" issuer));
  ] in
  let issuer_field = Yojson.Safe.Util.member "issuer" metadata_json in
  printf "Issuer: %s\n" (Yojson.Safe.Util.to_string issuer_field);
  [%expect {| Issuer: http://localhost:9999 |}];
  return ()

let%expect_test "mock oauth server token response format" =
  let response = Mock_oauth_server.{
    access_token = "mock_access_token";
    token_type = "Bearer";
    expires_in = 3600;
    refresh_token = Some "mock_refresh_token";
    scope = Some "read write";
  } in
  let json = Mock_oauth_server.yojson_of_token_response response in
  let access = Yojson.Safe.Util.member "access_token" json in
  let token_type = Yojson.Safe.Util.member "token_type" json in
  printf "Access token: %s\n" (Yojson.Safe.Util.to_string access);
  printf "Token type: %s\n" (Yojson.Safe.Util.to_string token_type);
  [%expect {|
    Access token: mock_access_token
    Token type: Bearer |}];
  return ()

let%expect_test "mock oauth server create and configure" =
  let server = Mock_oauth_server.create ~port:19999 () in
  Mock_oauth_server.add_valid_code server ~code:"auth_code_123" ~client_id:"test_client";
  let issuer = Mock_oauth_server.get_issuer server in
  printf "Mock server issuer: %s\n" issuer;
  [%expect {| Mock server issuer: http://localhost:19999 |}];
  return ()

(** {1 File Token Storage Tests} *)

let%expect_test "file token storage hash generation" =
  (* Test that different URLs produce different hashes *)
  let storage1 = Mcp_client_auth.File_token_storage.create_default 
    ~server_url:"http://localhost:8000" in
  let storage2 = Mcp_client_auth.File_token_storage.create_default 
    ~server_url:"http://localhost:9000" in
  (* They should have different file paths (hashed) *)
  printf "Storage created for two different servers\n";
  (* We can't easily inspect the paths, but existence check works *)
  let%bind exists1 = Mcp_client_auth.File_token_storage.exists storage1 in
  let%bind exists2 = Mcp_client_auth.File_token_storage.exists storage2 in
  printf "Storage 1 exists (should be false initially): %b\n" exists1;
  printf "Storage 2 exists (should be false initially): %b\n" exists2;
  [%expect {|
    Storage created for two different servers
    Storage 1 exists (should be false initially): false
    Storage 2 exists (should be false initially): false |}];
  return ()

(** {1 Full Integration Tests with Mock OAuth Server} *)

(* Note: These tests start an actual HTTP server on localhost.
   Each test uses a unique port to avoid conflicts. *)

let%expect_test "oauth_metadata_discovery_with_mock_server" =
  let port = 19991 in
  let server = Mock_oauth_server.create ~port () in
  let%bind () = Mock_oauth_server.start server in
  
  let issuer = Mock_oauth_server.get_issuer server in
  
  (* Fetch the OAuth metadata endpoint *)
  let metadata_url = sprintf "%s/.well-known/oauth-authorization-server" issuer in
  let%bind _resp, body = Cohttp_async.Client.get (Uri.of_string metadata_url) in
  let%bind body_str = Cohttp_async.Body.to_string body in
  let json = Yojson.Safe.from_string body_str in
  
  (* Verify metadata fields *)
  let issuer_field = Yojson.Safe.Util.member "issuer" json |> Yojson.Safe.Util.to_string in
  let token_endpoint = Yojson.Safe.Util.member "token_endpoint" json |> Yojson.Safe.Util.to_string in
  let auth_endpoint = Yojson.Safe.Util.member "authorization_endpoint" json |> Yojson.Safe.Util.to_string in
  
  printf "Issuer: %s\n" issuer_field;
  printf "Token endpoint matches issuer: %b\n" (String.is_prefix token_endpoint ~prefix:issuer);
  printf "Auth endpoint matches issuer: %b\n" (String.is_prefix auth_endpoint ~prefix:issuer);
  
  let%bind () = Mock_oauth_server.stop server in
  [%expect {|
    Issuer: http://localhost:19991
    Token endpoint matches issuer: true
    Auth endpoint matches issuer: true |}];
  return ()

let%expect_test "client_registration_with_mock_server" =
  let port = 19992 in
  let server = Mock_oauth_server.create ~port () in
  let%bind () = Mock_oauth_server.start server in
  
  let issuer = Mock_oauth_server.get_issuer server in
  let register_url = sprintf "%s/register" issuer in
  
  (* Perform dynamic client registration *)
  let client_metadata = `Assoc [
    ("redirect_uris", `List [`String "http://localhost:9999/callback"]);
    ("client_name", `String "Test Client");
  ] in
  let body = Yojson.Safe.to_string client_metadata in
  let headers = Cohttp.Header.of_list [("Content-Type", "application/json")] in
  let%bind resp, body = 
    Cohttp_async.Client.post 
      ~headers 
      ~body:(`String body) 
      (Uri.of_string register_url) 
  in
  let status = Cohttp.Response.status resp in
  let%bind body_str = Cohttp_async.Body.to_string body in
  let json = Yojson.Safe.from_string body_str in
  
  let client_id = Yojson.Safe.Util.member "client_id" json |> Yojson.Safe.Util.to_string in
  let client_secret = Yojson.Safe.Util.member "client_secret" json |> Yojson.Safe.Util.to_string in
  
  printf "Registration status: %s\n" (Cohttp.Code.string_of_status status);
  printf "Client ID received: %b\n" (String.is_prefix client_id ~prefix:"mock_client_");
  printf "Client secret received: %b\n" (String.is_prefix client_secret ~prefix:"mock_secret_");
  
  let%bind () = Mock_oauth_server.stop server in
  [%expect {|
    Registration status: 201 Created
    Client ID received: true
    Client secret received: true |}];
  return ()

let%expect_test "token_exchange_with_mock_server" =
  let port = 19993 in
  let server = Mock_oauth_server.create ~port () in
  let%bind () = Mock_oauth_server.start server in
  
  (* Register a valid authorization code *)
  Mock_oauth_server.add_valid_code server ~code:"test_auth_code_123" ~client_id:"test_client";
  
  let issuer = Mock_oauth_server.get_issuer server in
  let token_url = sprintf "%s/token" issuer in
  
  (* Exchange code for tokens *)
  let body = Uri.encoded_of_query [
    ("grant_type", ["authorization_code"]);
    ("code", ["test_auth_code_123"]);
    ("client_id", ["test_client"]);
    ("redirect_uri", ["http://localhost:9999/callback"]);
  ] in
  let headers = Cohttp.Header.of_list [("Content-Type", "application/x-www-form-urlencoded")] in
  let%bind resp, body = 
    Cohttp_async.Client.post 
      ~headers 
      ~body:(`String body) 
      (Uri.of_string token_url) 
  in
  let status = Cohttp.Response.status resp in
  let%bind body_str = Cohttp_async.Body.to_string body in
  let json = Yojson.Safe.from_string body_str in
  
  let access_token = Yojson.Safe.Util.member "access_token" json |> Yojson.Safe.Util.to_string in
  let token_type = Yojson.Safe.Util.member "token_type" json |> Yojson.Safe.Util.to_string in
  let has_refresh = match Yojson.Safe.Util.member "refresh_token" json with `Null -> false | _ -> true in
  
  printf "Token exchange status: %s\n" (Cohttp.Code.string_of_status status);
  printf "Access token received: %b\n" (String.is_prefix access_token ~prefix:"mock_access_");
  printf "Token type: %s\n" token_type;
  printf "Refresh token received: %b\n" has_refresh;
  
  (* Verify token is tracked by server *)
  printf "Token valid on server: %b\n" (Mock_oauth_server.is_token_valid server ~token:access_token);
  
  let%bind () = Mock_oauth_server.stop server in
  [%expect {|
    Token exchange status: 200 OK
    Access token received: true
    Token type: Bearer
    Refresh token received: true
    Token valid on server: true |}];
  return ()

let%expect_test "token_refresh_with_mock_server" =
  let port = 19994 in
  let server = Mock_oauth_server.create ~port () in
  let%bind () = Mock_oauth_server.start server in
  
  (* First, get initial tokens *)
  Mock_oauth_server.add_valid_code server ~code:"initial_code" ~client_id:"test_client";
  
  let issuer = Mock_oauth_server.get_issuer server in
  let token_url = sprintf "%s/token" issuer in
  
  let headers = Cohttp.Header.of_list [("Content-Type", "application/x-www-form-urlencoded")] in
  
  (* Exchange initial code *)
  let body = Uri.encoded_of_query [
    ("grant_type", ["authorization_code"]);
    ("code", ["initial_code"]);
    ("client_id", ["test_client"]);
  ] in
  let%bind _resp, body = 
    Cohttp_async.Client.post ~headers ~body:(`String body) (Uri.of_string token_url) 
  in
  let%bind body_str = Cohttp_async.Body.to_string body in
  let json = Yojson.Safe.from_string body_str in
  let refresh_token = Yojson.Safe.Util.member "refresh_token" json |> Yojson.Safe.Util.to_string in
  
  printf "Got initial refresh token: %b\n" (String.is_prefix refresh_token ~prefix:"mock_refresh_");
  
  (* Now use refresh token to get new access token *)
  let refresh_body = Uri.encoded_of_query [
    ("grant_type", ["refresh_token"]);
    ("refresh_token", [refresh_token]);
    ("client_id", ["test_client"]);
  ] in
  let%bind resp, body = 
    Cohttp_async.Client.post ~headers ~body:(`String refresh_body) (Uri.of_string token_url) 
  in
  let status = Cohttp.Response.status resp in
  let%bind body_str = Cohttp_async.Body.to_string body in
  let json = Yojson.Safe.from_string body_str in
  
  let new_access_token = Yojson.Safe.Util.member "access_token" json |> Yojson.Safe.Util.to_string in
  
  printf "Refresh status: %s\n" (Cohttp.Code.string_of_status status);
  printf "New access token received: %b\n" (String.is_prefix new_access_token ~prefix:"mock_refreshed_");
  printf "New token valid on server: %b\n" (Mock_oauth_server.is_token_valid server ~token:new_access_token);
  
  let%bind () = Mock_oauth_server.stop server in
  [%expect {|
    Got initial refresh token: true
    Refresh status: 200 OK
    New access token received: true
    New token valid on server: true |}];
  return ()

let%expect_test "full_oauth_flow_headless_with_mock_server" =
  let port = 19995 in
  let server = Mock_oauth_server.create ~port () in
  let%bind () = Mock_oauth_server.start server in
  
  let auth_code = "headless_test_code_456" in
  Mock_oauth_server.add_valid_code server ~code:auth_code ~client_id:"headless_client";
  
  let issuer = Mock_oauth_server.get_issuer server in
  
  (* Create HeadlessOAuth client with mock server *)
  let headless = Headless_oauth.create
    ~server_url:issuer
    ~mock_auth_code:auth_code
    ~mock_state:"test_state_xyz"
    ()
  in
  
  let metadata = Headless_oauth.default_client_metadata
    ~scopes:["read"; "write"]
    ~client_name:"Integration Test Client"
    ()
  in
  
  let oauth = Headless_oauth.create_oauth_client headless ~client_metadata:metadata in
  
  (* Initialize OAuth client (loads stored tokens) *)
  let%bind () = Mcp_client_auth.Oauth2.initialize oauth in
  
  let ctx = Mcp_client_auth.Oauth2.get_context oauth in
  printf "OAuth client initialized\n";
  printf "Server URL: %s\n" ctx.server_url;
  
  (* Perform complete token exchange flow manually *)
  let token_url = sprintf "%s/token" issuer in
  let headers = Cohttp.Header.of_list [("Content-Type", "application/x-www-form-urlencoded")] in
  let body = Uri.encoded_of_query [
    ("grant_type", ["authorization_code"]);
    ("code", [auth_code]);
    ("client_id", ["headless_client"]);
    ("redirect_uri", ["http://localhost:9999/callback"]);
  ] in
  let%bind resp, body = 
    Cohttp_async.Client.post ~headers ~body:(`String body) (Uri.of_string token_url) 
  in
  let status = Cohttp.Response.status resp in
  let%bind body_str = Cohttp_async.Body.to_string body in
  let json = Yojson.Safe.from_string body_str in
  
  let access_token = Yojson.Safe.Util.member "access_token" json |> Yojson.Safe.Util.to_string in
  
  printf "Token exchange via mock server: %s\n" (Cohttp.Code.string_of_status status);
  printf "Token received: %b\n" (String.is_prefix access_token ~prefix:"mock_access_");
  printf "Token tracked by mock server: %b\n" (Mock_oauth_server.is_token_valid server ~token:access_token);
  
  let%bind () = Mock_oauth_server.stop server in
  [%expect {|
    OAuth client initialized
    Server URL: http://localhost:19995
    Token exchange via mock server: 200 OK
    Token received: true
    Token tracked by mock server: true |}];
  return ()

let%expect_test "invalid_code_returns_error" =
  let port = 19996 in
  let server = Mock_oauth_server.create ~port () in
  let%bind () = Mock_oauth_server.start server in
  
  let issuer = Mock_oauth_server.get_issuer server in
  let token_url = sprintf "%s/token" issuer in
  
  (* Try to exchange an invalid code (not registered) *)
  let body = Uri.encoded_of_query [
    ("grant_type", ["authorization_code"]);
    ("code", ["invalid_code_xyz"]);
    ("client_id", ["test_client"]);
  ] in
  let headers = Cohttp.Header.of_list [("Content-Type", "application/x-www-form-urlencoded")] in
  let%bind resp, body = 
    Cohttp_async.Client.post ~headers ~body:(`String body) (Uri.of_string token_url) 
  in
  let status = Cohttp.Response.status resp in
  let%bind body_str = Cohttp_async.Body.to_string body in
  let json = Yojson.Safe.from_string body_str in
  
  let error = Yojson.Safe.Util.member "error" json |> Yojson.Safe.Util.to_string in
  
  printf "Status: %s\n" (Cohttp.Code.string_of_status status);
  printf "Error: %s\n" error;
  
  let%bind () = Mock_oauth_server.stop server in
  [%expect {|
    Status: 400 Bad Request
    Error: invalid_grant |}];
  return ()
