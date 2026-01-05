(** OAuth Client Tests

    Minimal test suite for OAuth client functionality. Many tests are stubbed
    pending implementation of missing components. See src/client/auth/oauth.todo
    for details on missing functionality. **)

open! Core
open! Async
open! Expect_test_helpers_core

(** {1 Basic Functionality Tests} *)

let%expect_test "create oauth client with defaults" =
  let _oauth =
    Client_auth.Oauth.create ~mcp_url:"http://localhost:3000/mcp/sse/" ()
  in
  (* Just verify it creates without error *)
  print_endline "OAuth client created successfully";
  [%expect {| OAuth client created successfully |}];
  return ()

let%expect_test "create oauth client with custom scopes" =
  let _oauth =
    Client_auth.Oauth.create ~mcp_url:"http://localhost:3000/mcp/sse/"
      ~scopes:[ "read"; "write" ] ~client_name:"Test Client" ()
  in
  print_endline "OAuth client with scopes created";
  [%expect {| OAuth client with scopes created |}];
  return ()

let%expect_test "create oauth client with callback port" =
  let _oauth =
    Client_auth.Oauth.create ~mcp_url:"http://localhost:3000/mcp/sse/"
      ~callback_port:(Some 8080) ()
  in
  print_endline "OAuth client with fixed port created";
  [%expect {| OAuth client with fixed port created |}];
  return ()

(** {1 Token Storage Tests} *)

let%expect_test "token storage cache keys" =
  let storage =
    Client_auth.Oauth.Token_storage_adapter.create
      ~server_url:"http://localhost:3000"
  in
  let token_key =
    Client_auth.Oauth.Token_storage_adapter.get_token_cache_key storage
  in
  let client_key =
    Client_auth.Oauth.Token_storage_adapter.get_client_info_cache_key storage
  in
  print_endline token_key;
  print_endline client_key;
  [%expect
    {|
    http://localhost:3000/tokens
    http://localhost:3000/client_info |}];
  return ()

let%expect_test "token storage set and get" =
  let storage =
    Client_auth.Oauth.Token_storage_adapter.create
      ~server_url:"http://localhost:3000"
  in
  let token =
    Mcp_shared.Auth.
      {
        access_token = "test-token";
        token_type = "Bearer";
        expires_in = Some 3600;
        scope = Some "read write";
        refresh_token = Some "refresh-token";
      }
  in
  let%bind () =
    Client_auth.Oauth.Token_storage_adapter.set_tokens storage token
  in
  let%bind retrieved =
    Client_auth.Oauth.Token_storage_adapter.get_tokens storage
  in
  (match retrieved with
  | Some t ->
    printf "Token retrieved: %s\n" t.access_token;
    printf "Has refresh: %b\n" (Option.is_some t.refresh_token)
  | None -> print_endline "No token found");
  [%expect {|
    Token retrieved: test-token
    Has refresh: true |}];
  return ()

let%expect_test "token storage clear" =
  let storage =
    Client_auth.Oauth.Token_storage_adapter.create
      ~server_url:"http://localhost:3000"
  in
  let token =
    Mcp_shared.Auth.
      {
        access_token = "test-token";
        token_type = "Bearer";
        expires_in = None;
        scope = None;
        refresh_token = None;
      }
  in
  let%bind () =
    Client_auth.Oauth.Token_storage_adapter.set_tokens storage token
  in
  let%bind () = Client_auth.Oauth.Token_storage_adapter.clear storage in
  let%bind retrieved =
    Client_auth.Oauth.Token_storage_adapter.get_tokens storage
  in
  (match retrieved with
  | Some _ -> print_endline "Token still exists"
  | None -> print_endline "Token cleared");
  [%expect {| Token cleared |}];
  return ()

(** {1 Helper Function Tests} *)

let%expect_test "find_available_port" =
  let port = Client_auth.Oauth.find_available_port () in
  let is_valid = port >= 1024 && port <= 65535 in
  printf "Port in valid range (1024-65535): %b\n" is_valid;
  [%expect {| Port in valid range (1024-65535): true |}];
  return ()

(** {1 Stubbed/Missing Tests} *)

(* The following tests cannot be implemented yet due to missing components:

   - test_unauthorized: Needs full MCP client integration + auth - test_ping:
   Needs MCP client with OAuth provider integration - test_list_tools: Needs MCP
   client integration - test_call_tool: Needs MCP client integration -
   test_list_resources: Needs MCP client integration - test_read_resource: Needs
   MCP client integration - test_oauth_server_metadata_discovery: Needs OAuth
   server implementation

   These tests are documented in src/client/auth/oauth.todo and will be
   implemented once the following are available:

   1. OAuth callback server (oauth_callback.ml) 2. MCP client auth base classes
   (Mcp.Client.Auth) 3. HeadlessOAuth test utility 4. Test server infrastructure
   5. OAuth provider implementation *)

(** NOTE: The check_if_auth_required test is skipped because it makes actual
    HTTP requests which may not work in all test environments *)

(* Placeholder for future integration tests *)
let () =
  Logs.info (fun m -> m "OAuth client tests - basic functionality only");
  Logs.info (fun m ->
      m
        "Integration tests require: oauth_callback.ml, MCP auth base, test \
         utilities")
