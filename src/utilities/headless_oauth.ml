(** HeadlessOAuth - OAuth Client for Automated Testing

    Provides OAuth client that bypasses browser interaction for testing.
    Returns mock authorization codes directly. *)

open Core
open Async

(** {1 Types} *)

type t = {
  server_url : string;
  mock_auth_code : string;
  mock_state : string;
  storage : Mcp_client_auth.Oauth2.storage_wrapper;
}

(** {1 In-Memory Storage for Tests} *)

module Test_storage = struct
  type t = {
    mutable tokens : Mcp_shared.Auth.oauth_token option;
    mutable client_info : Mcp_shared.Auth.oauth_client_information_full option;
  }

  let create () = { tokens = None; client_info = None }
  let get_tokens t = return t.tokens
  let set_tokens t tokens = t.tokens <- Some tokens; return ()
  let get_client_info t = return t.client_info
  let set_client_info t info = t.client_info <- Some info; return ()
end

(** {1 HeadlessOAuth Implementation} *)

let create ~server_url ~mock_auth_code ?(mock_state = "test_state_12345") () =
  let storage_instance = Test_storage.create () in
  let storage = Mcp_client_auth.Oauth2.Storage ((module Test_storage : Mcp_client_auth.Oauth2.Token_storage with type t = Test_storage.t), storage_instance) in
  { server_url; mock_auth_code; mock_state; storage }

let redirect_handler _t _url =
  (* No-op - don't open browser in headless mode *)
  Logs.info (fun m -> m "HeadlessOAuth: Skipping browser redirect");
  return ()

let callback_handler t () =
  (* Immediately return mock auth code and state *)
  Logs.info (fun m -> m "HeadlessOAuth: Returning mock auth code");
  return (t.mock_auth_code, Some t.mock_state)

let create_oauth_client t ~client_metadata =
  Mcp_client_auth.Oauth2.create
    ~server_url:t.server_url
    ~client_metadata
    ~storage:t.storage
    ~redirect_handler:(Some (redirect_handler t))
    ~callback_handler:(Some (callback_handler t))
    ()

(** {1 Convenience Functions} *)

let default_client_metadata ?(scopes = []) ?(client_name = "HeadlessOAuth Test") () =
  Mcp_shared.Auth.{
    redirect_uris = ["http://localhost:9999/callback"];
    token_endpoint_auth_method = `None;
    grant_types = [`Authorization_code; `Refresh_token];
    response_types = [`Code];
    scope = (if List.is_empty scopes then None else Some (String.concat ~sep:" " scopes));
    client_name = Some client_name;
    client_uri = None;
    logo_uri = None;
    contacts = None;
    tos_uri = None;
    policy_uri = None;
    jwks_uri = None;
    jwks = None;
    software_id = None;
    software_version = None;
  }

let create_with_defaults ~server_url ~mock_auth_code () =
  let t = create ~server_url ~mock_auth_code () in
  let client_metadata = default_client_metadata () in
  create_oauth_client t ~client_metadata
