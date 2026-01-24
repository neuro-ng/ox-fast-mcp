(** OAuth Client for OxFastMCP

    Provides OAuth2 authentication for MCP servers with browser-based auth flow.
    This is a partial implementation - full browser interaction and callback
    server are stubbed for future implementation.

    See oauth.todo for missing functionality. **)

open Core
open Async

(** {1 Exceptions} *)

exception Client_not_found_error of string
(** Raised when OAuth client credentials are not found on server *)

(** {1 Helper Functions} *)

let check_if_auth_required ?(httpx_kwargs = []) mcp_url =
  Monitor.try_with (fun () ->
      Cohttp_async.Client.get
        ~headers:(Cohttp.Header.of_list httpx_kwargs)
        (Uri.of_string mcp_url)
      >>| fun (response, _body) ->
      let status = Cohttp.Response.status response in
      let headers = Cohttp.Response.headers response in

      (* Check for 401/403 status codes *)
      if
        Cohttp.Code.code_of_status status = 401
        || Cohttp.Code.code_of_status status = 403
      then true (* Check for WWW-Authenticate header *)
      else if Cohttp.Header.get headers "WWW-Authenticate" |> Option.is_some
      then true
      else false)
  >>| function
  | Ok result -> result
  | Error _ -> true (* If connection fails, assume auth might be required *)

let find_available_port () =
  (* Simplified port finder - binds to port 0 to get OS-assigned port *)
  try
    let sock =
      Core_unix.socket ~domain:PF_INET ~kind:SOCK_STREAM ~protocol:0 ()
    in
    let addr =
      Core_unix.ADDR_INET (Core_unix.Inet_addr.of_string "127.0.0.1", 0)
    in
    Core_unix.bind sock ~addr;
    let actual_addr = Core_unix.getsockname sock in
    Core_unix.close sock;
    match actual_addr with
    | Core_unix.ADDR_INET (_, port) -> port
    | _ -> 8080 (* Fallback *)
  with _ -> 8080 (* Fallback on any error *)

let open_browser url =
  (* Platform-specific browser opening *)
  let cmd =
    match Sys.os_type with
    | "Unix" | "Cygwin" ->
      sprintf "xdg-open '%s' 2>/dev/null || open '%s' 2>/dev/null &" url url
    | "Win32" -> sprintf "start %s" url
    | _ -> sprintf "open %s" url
  in
  let _result = Core_unix.system cmd in
  Logs.info (fun m -> m "Opening browser for OAuth: %s" url)

(** {1 Token Storage Adapter} *)

module Token_storage_adapter = struct
  type t = {
    server_url : string;
    mutable token_cache : Mcp_shared.Auth.oauth_token option;
    mutable client_info_cache :
      Mcp_shared.Auth.oauth_client_information_full option;
    mutable token_expiry : Time_ns.t option;
  }

  let create ~server_url =
    {
      server_url;
      token_cache = None;
      client_info_cache = None;
      token_expiry = None;
    }

  let get_token_cache_key t = sprintf "%s/tokens" t.server_url
  let get_client_info_cache_key t = sprintf "%s/client_info" t.server_url

  let clear t =
    t.token_cache <- None;
    t.client_info_cache <- None;
    t.token_expiry <- None;
    return ()

  let get_tokens t =
    (* Check if token is expired *)
    match t.token_expiry with
    | Some expiry when Time_ns.( > ) (Time_ns.now ()) expiry ->
      t.token_cache <- None;
      return None
    | _ -> return t.token_cache

  let set_tokens t tokens =
    t.token_cache <- Some tokens;
    (* Set expiry time if expires_in is present *)
    (match tokens.expires_in with
    | Some ttl ->
      let expiry = Time_ns.add (Time_ns.now ()) (Time_ns.Span.of_int_sec ttl) in
      t.token_expiry <- Some expiry
    | None -> ());
    return ()

  let get_client_info t = return t.client_info_cache

  let set_client_info t client_info =
    t.client_info_cache <- Some client_info;
    (* Set expiry if client_secret_expires_at is present *)
    (match client_info.info.client_secret_expires_at with
    | Some expires_at ->
      let expiry = Time_ns.of_int63_ns_since_epoch (Int63.of_int expires_at) in
      t.token_expiry <- Some expiry
    | None -> ());
    return ()
end

(** {1 OAuth Client} *)

type t = {
  mcp_url : string;
  server_base_url : string;
  scopes : string list;
  client_name : string;
  redirect_port : int;
  redirect_uri : string;
  client_metadata : Mcp_shared.Auth.oauth_client_metadata;
  token_storage : Token_storage_adapter.t;
  mutable initialized : bool;
}

[@@@warning "-27"]
(* Suppress unused variable warnings for reserved parameters *)

let create ~mcp_url ?(scopes = []) ?(client_name = "OxFastMCP Client")
    ?(callback_port = None) ?(additional_client_metadata = []) () =
  (* Parse URL to get base URL *)
  let uri = Uri.of_string mcp_url in
  let scheme = Uri.scheme uri |> Option.value ~default:"http" in
  let host = Uri.host uri |> Option.value ~default:"localhost" in
  let port_opt = Uri.port uri in

  let server_base_url =
    match port_opt with
    | Some p -> sprintf "%s://%s:%d" scheme host p
    | None -> sprintf "%s://%s" scheme host
  in

  (* Setup OAuth client *)
  let redirect_port =
    Option.value callback_port ~default:(find_available_port ())
  in
  let redirect_uri = sprintf "http://localhost:%d/callback" redirect_port in

  let scopes_str = String.concat ~sep:" " scopes in

  let client_metadata =
    Mcp_shared.Auth.
      {
        redirect_uris = [ redirect_uri ];
        token_endpoint_auth_method = `None;
        grant_types = [ `Authorization_code; `Refresh_token ];
        response_types = [ `Code ];
        scope = (if String.is_empty scopes_str then None else Some scopes_str);
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
  in

  (* Log warning about in-memory storage *)
  Logs.warn (fun m ->
      m
        "Using in-memory token storage - tokens will be lost when client \
         restarts");

  let token_storage =
    Token_storage_adapter.create ~server_url:server_base_url
  in

  {
    mcp_url;
    server_base_url;
    scopes;
    client_name;
    redirect_port;
    redirect_uri;
    client_metadata;
    token_storage;
    initialized = false;
  }

let initialize t =
  if t.initialized then return ()
  else (
    t.initialized <- true;
    (* Load stored tokens and client info *)
    let%bind tokens = Token_storage_adapter.get_tokens t.token_storage in
    let%bind _client_info =
      Token_storage_adapter.get_client_info t.token_storage
    in

    (* If tokens were loaded, they're already set *)
    match tokens with
    | Some _ ->
      Logs.info (fun m -> m "Loaded cached OAuth tokens");
      return ()
    | None ->
      Logs.info (fun m -> m "No cached tokens found");
      return ())

let redirect_handler _t authorization_url =
  (* Pre-flight check to detect invalid client_id before opening browser *)
  let%bind response, _body =
    Cohttp_async.Client.get ~headers:(Cohttp.Header.init ())
      (Uri.of_string authorization_url)
  in

  let status_code =
    Cohttp.Response.status response |> Cohttp.Code.code_of_status
  in

  (* Check for client not found error *)
  if status_code = 400 then
    raise
      (Client_not_found_error
         "OAuth client not found - cached credentials may be stale")
  else if
    not (List.mem [ 200; 302; 303; 307; 308 ] status_code ~equal:Int.equal)
  then
    raise
      (Failure (sprintf "Unexpected authorization response: %d" status_code))
  else (
    Logs.info (fun m -> m "OAuth authorization URL: %s" authorization_url);
    open_browser authorization_url;
    return ())

let callback_handler t =
  (* Start OAuth callback HTTP server and wait for browser redirect *)
  Logs.info (fun m ->
      m "Starting OAuth callback server on http://localhost:%d" t.redirect_port);

  let timeout = Time_ns.Span.of_min 5.0 in
  let%bind result =
    Oauth_callback.start_callback_server ~port:t.redirect_port ~timeout
  in

  match result.Oauth_callback.code with
  | Some code ->
    Logs.info (fun m -> m "OAuth authorization code received");
    return (code, result.state)
  | None -> (
    match result.error with
    | Some err ->
      let desc =
        Option.value result.error_description ~default:"Unknown error"
      in
      failwith (sprintf "OAuth authorization failed: %s - %s" err desc)
    | None -> failwith "No authorization code received from OAuth callback")

let clear_cache t = Token_storage_adapter.clear t.token_storage
