(** OAuth2 Authentication Implementation for MCP Client

    Implements authorization code flow with PKCE and automatic token refresh.
    Translation from Python mcp/client/auth/oauth2.py *)

open Core
open Async

(** {1 PKCE Parameters} *)

module Pkce_parameters = struct
  type t = { code_verifier : string; code_challenge : string }
  [@@deriving fields]

  let generate () =
    (* Generate 128-character code verifier from allowed characters *)
    let allowed_chars =
      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~"
    in
    let len = String.length allowed_chars in
    let code_verifier =
      String.init 128 ~f:(fun _ -> allowed_chars.[Random.int len])
    in
    (* Calculate SHA256 challenge with base64url encoding *)
    let digest = Digestif.SHA256.digest_string code_verifier in
    let digest_bytes = Digestif.SHA256.to_raw_string digest in
    (* Base64 URL-safe encoding without padding per RFC 7636 *)
    let code_challenge =
      Base64.encode_exn ~pad:false ~alphabet:Base64.uri_safe_alphabet
        digest_bytes
    in
    { code_verifier; code_challenge }
end

(** {1 Token Storage} *)

module type Token_storage = sig
  type t

  val get_tokens : t -> Mcp_shared.Auth.oauth_token option Deferred.t
  (** Get stored tokens *)

  val set_tokens : t -> Mcp_shared.Auth.oauth_token -> unit Deferred.t
  (** Store tokens *)

  val get_client_info :
    t -> Mcp_shared.Auth.oauth_client_information_full option Deferred.t
  (** Get stored client information *)

  val set_client_info :
    t -> Mcp_shared.Auth.oauth_client_information_full -> unit Deferred.t
  (** Store client information *)
end

(** {1 OAuth Context} *)

(** Storage wrapped to hide type parameter *)
type storage_wrapper =
  | Storage : (module Token_storage with type t = 'a) * 'a -> storage_wrapper

type oauth_context = {
  server_url : string;
  mutable client_metadata : Mcp_shared.Auth.oauth_client_metadata;
  storage : storage_wrapper;
  redirect_handler : (string -> unit Deferred.t) option;
  callback_handler : (unit -> (string * string option) Deferred.t) option;
  timeout : float;
  client_metadata_url : string option;
  (* Discovered metadata *)
  mutable protected_resource_metadata :
    Mcp_shared.Auth.protected_resource_metadata option;
  mutable oauth_metadata : Mcp_shared.Auth.oauth_metadata option;
  mutable auth_server_url : string option;
  mutable protocol_version : string option;
  (* Client registration *)
  mutable client_info : Mcp_shared.Auth.oauth_client_information_full option;
  (* Token management *)
  mutable current_tokens : Mcp_shared.Auth.oauth_token option;
  mutable token_expiry_time : float option;
  (* Concurrency *)
  lock : unit Ivar.t;
}

let get_authorization_base_url server_url =
  let uri = Uri.of_string server_url in
  let scheme = Uri.scheme uri |> Option.value ~default:"https" in
  let host = Uri.host uri |> Option.value ~default:"localhost" in
  sprintf "%s://%s" scheme host

let calculate_token_expiry expires_in_opt =
  match expires_in_opt with
  | Some expires_in ->
    let now = Time_float.now () in
    let expiry =
      Time_float.add now (Time_float.Span.of_int_sec (expires_in - 5))
    in
    Some (Time_float.to_span_since_epoch expiry |> Time_float.Span.to_sec)
  | None -> None

let update_token_expiry context token =
  context.token_expiry_time <-
    calculate_token_expiry token.Mcp_shared.Auth.expires_in

let is_token_valid context =
  match context.current_tokens with
  | None -> false
  | Some tokens when String.is_empty tokens.access_token -> false
  | Some _ -> (
    match context.token_expiry_time with
    | None -> true
    | Some expiry_time ->
      let now =
        Time_float.now () |> Time_float.to_span_since_epoch
        |> Time_float.Span.to_sec
      in
      Float.(now <= expiry_time))

let can_refresh_token context =
  match (context.current_tokens, context.client_info) with
  | Some tokens, Some _client_info -> Option.is_some tokens.refresh_token
  | _ -> false

let clear_tokens context =
  context.current_tokens <- None;
  context.token_expiry_time <- None

let resource_url_from_server_url server_url =
  (* Extract resource URL by removing path *)
  let uri = Uri.of_string server_url in
  let scheme = Uri.scheme uri |> Option.value ~default:"https" in
  let host = Uri.host uri |> Option.value ~default:"localhost" in
  let port = Uri.port uri in
  match port with
  | Some p -> sprintf "%s://%s:%d" scheme host p
  | None -> sprintf "%s://%s" scheme host

let check_resource_allowed ~requested_resource ~configured_resource =
  (* Simple prefix check for resource validation *)
  String.is_prefix requested_resource ~prefix:configured_resource

let get_resource_url context =
  let resource = resource_url_from_server_url context.server_url in
  match context.protected_resource_metadata with
  | Some prm when not (String.is_empty prm.resource) ->
    let prm_resource = prm.resource in
    if
      check_resource_allowed ~requested_resource:resource
        ~configured_resource:prm_resource
    then prm_resource
    else resource
  | _ -> resource

let should_include_resource_param context protocol_version_opt =
  match context.protected_resource_metadata with
  | Some _ -> true
  | None -> (
    match protocol_version_opt with
    | None -> false
    | Some version -> String.(version >= "2025-06-18"))

let prepare_token_auth context data headers_opt =
  let headers = Option.value headers_opt ~default:[] in
  match context.client_info with
  | None -> (data, headers)
  | Some client_info -> (
    let auth_method = client_info.metadata.token_endpoint_auth_method in
    match (auth_method, client_info.info.client_secret) with
    | `Client_secret_post, Some secret ->
      (* Include client_secret in body *)
      let data_with_secret = ("client_secret", secret) :: data in
      (data_with_secret, headers)
    | `None, _ | `Client_secret_post, None ->
      (* No authentication or no secret available *)
      (data, headers))

(** {1 OAuth Client Provider} *)

type t = { context : oauth_context; mutable initialized : bool }

let create ~server_url ~client_metadata ~(storage : storage_wrapper)
    ?(redirect_handler = None) ?(callback_handler = None) ?(timeout = 300.0)
    ?(client_metadata_url = None) () =
  (* Validate client_metadata_url if provided *)
  (match client_metadata_url with
  | Some url when not (Utils.is_valid_client_metadata_url (Some url)) ->
    raise
      (Invalid_argument
         (sprintf
            "client_metadata_url must be a valid HTTPS URL with a non-root \
             pathname, got: %s"
            url))
  | _ -> ());

  let context =
    {
      server_url;
      client_metadata;
      storage;
      redirect_handler;
      callback_handler;
      timeout;
      client_metadata_url;
      protected_resource_metadata = None;
      oauth_metadata = None;
      auth_server_url = None;
      protocol_version = None;
      client_info = None;
      current_tokens = None;
      token_expiry_time = None;
      lock = Ivar.create ();
    }
  in

  { context; initialized = false }

let initialize t =
  if t.initialized then return ()
  else
    let (Storage (module_storage, storage_impl)) = t.context.storage in
    let module Storage = (val module_storage : Token_storage with type t = _) in
    let%bind tokens = Storage.get_tokens storage_impl in
    let%bind client_info = Storage.get_client_info storage_impl in
    t.context.current_tokens <- tokens;
    t.context.client_info <- client_info;
    t.initialized <- true;
    return ()

let add_auth_header t request_headers =
  match t.context.current_tokens with
  | Some tokens when not (String.is_empty tokens.access_token) ->
    ("Authorization", sprintf "Bearer %s" tokens.access_token)
    :: request_headers
  | _ -> request_headers

(** Get token endpoint URL *)
let get_token_endpoint context =
  match context.oauth_metadata with
  | Some metadata when not (String.is_empty metadata.token_endpoint) ->
    metadata.token_endpoint
  | _ ->
    let auth_base = get_authorization_base_url context.server_url in
    Uri.with_path (Uri.of_string auth_base) "/token" |> Uri.to_string

(** Get the OAuth context from the provider *)
let get_context t = t.context

(** {1 OAuth Flow Helper Functions} *)

(** Discover protected resource metadata with fallback URLs *)
let discover_protected_resource_metadata www_auth_url server_url =
  let urls =
    Utils.build_protected_resource_metadata_discovery_urls www_auth_url
      server_url
  in
  (* Try each URL in priority order until we get a valid response *)
  let rec try_urls urls_remaining =
    match urls_remaining with
    | [] -> return None
    | url :: rest -> (
      let request = Utils.create_oauth_metadata_request url in
      let uri = Cohttp.Request.uri request in
      (* Make HTTP GET request *)
      let%bind resp, body = Cohttp_async.Client.get uri in
      let%bind result = Utils.handle_protected_resource_response resp body in
      match result with
      | Some prm -> return (Some prm)
      | None -> try_urls rest)
  in
  try_urls urls

(** Discover OAuth authorization server metadata with fallback URLs *)
let discover_oauth_authorization_server_metadata auth_server_url server_url =
  let urls =
    Utils.build_oauth_authorization_server_metadata_discovery_urls
      auth_server_url server_url
  in
  (* Try each URL in priority order until we get a valid response *)
  let rec try_urls urls_remaining =
    match urls_remaining with
    | [] -> return None
    | url :: rest -> (
      let request = Utils.create_oauth_metadata_request url in
      let uri = Cohttp.Request.uri request in
      (* Make HTTP GET request *)
      let%bind resp, body = Cohttp_async.Client.get uri in
      let%bind should_continue, result =
        Utils.handle_auth_metadata_response resp body
      in
      match (should_continue, result) with
      | false, _ ->
        (* Server error, stop trying *)
        return None
      | true, Some asm -> return (Some asm)
      | true, None -> try_urls rest)
  in
  try_urls urls

(** Register client with authorization server or use CIMD *)
let register_client context =
  (* Check if we should use CIMD instead of DCR *)
  let use_cimd =
    Utils.should_use_client_metadata_url ~oauth_metadata:context.oauth_metadata
      ~client_metadata_url:context.client_metadata_url
  in
  if use_cimd then
    (* Use Client ID Metadata Document instead of dynamic registration *)
    match context.client_metadata_url with
    | Some url ->
      let client_info =
        Utils.create_client_info_from_metadata_url ~client_metadata_url:url
          ~redirect_uris:
            (List.map context.client_metadata.redirect_uris ~f:Uri.of_string)
          ()
      in
      return (Some client_info)
    | None -> return None
  else
    (* Check if already registered *)
    match context.client_info with
    | Some info -> return (Some info)
    | None ->
      (* Perform dynamic client registration *)
      let auth_base_url = get_authorization_base_url context.server_url in
      let request =
        Utils.create_client_registration_request
          ~auth_server_metadata:context.oauth_metadata
          ~client_metadata:context.client_metadata ~auth_base_url
      in
      let uri = Cohttp.Request.uri request in
      let headers = Cohttp.Request.headers request in
      let json =
        Mcp_shared.Auth.yojson_of_oauth_client_metadata context.client_metadata
      in
      let body_str = Yojson.Safe.to_string json in
      let%bind resp, body =
        Cohttp_async.Client.post ~body:(`String body_str) ~headers uri
      in
      let%bind client_info = Utils.handle_registration_response resp body in
      return (Some client_info)

(** Perform authorization code grant with PKCE *)
let perform_authorization_code_grant context =
  (* Generate PKCE parameters *)
  let pkce = Pkce_parameters.generate () in
  (* Generate state token *)
  let state = Crypto_utils.generate_state_token () in
  (* Build authorization URL *)
  match context.oauth_metadata with
  | None -> return (Error "Missing OAuth metadata for authorization")
  | Some metadata -> (
    match context.client_info with
    | None -> return (Error "Missing client info for authorization")
    | Some client_info -> (
      let auth_endpoint = metadata.authorization_endpoint in
      let client_id = client_info.info.client_id in
      let redirect_uri =
        List.hd context.client_metadata.redirect_uris
        |> Option.value ~default:""
      in
      let scope =
        Utils.get_client_metadata_scopes ~www_authenticate_scope:None
          ~protected_resource_metadata:context.protected_resource_metadata
          ~authorization_server_metadata:context.oauth_metadata ()
      in
      (* Build authorization URL with parameters *)
      let params =
        [
          ("response_type", "code");
          ("client_id", client_id);
          ("redirect_uri", redirect_uri);
          ("state", state);
          ("code_challenge", pkce.code_challenge);
          ("code_challenge_method", "S256");
        ]
        @
        match scope with
        | Some s -> [ ("scope", s) ]
        | None -> []
      in
      let auth_url = Uri.of_string auth_endpoint in
      let auth_url_with_params = Uri.add_query_params' auth_url params in
      (* Call redirect handler to open browser *)
      let%bind () =
        match context.redirect_handler with
        | Some handler -> handler (Uri.to_string auth_url_with_params)
        | None ->
          Logs.warn (fun m ->
              m "No redirect handler - cannot open authorization URL");
          return ()
      in
      (* Call callback handler to get authorization code *)
      match context.callback_handler with
      | Some handler -> (
        let%bind auth_code, returned_state = handler () in
        (* Validate state matches *)
        match returned_state with
        | Some s when Crypto_utils.constant_time_compare s state ->
          return (Ok (auth_code, pkce.code_verifier))
        | Some _ -> return (Error "State validation failed")
        | None -> return (Error "No state returned from callback"))
      | None -> return (Error "No callback handler configured")))

(** Exchange authorization code for tokens *)
let exchange_authorization_code context auth_code code_verifier =
  match (context.oauth_metadata, context.client_info) with
  | None, _ -> return (Error "Missing OAuth metadata")
  | _, None -> return (Error "Missing client info")
  | Some _metadata, Some client_info ->
    let token_url = get_token_endpoint context in
    let redirect_uri =
      List.hd context.client_metadata.redirect_uris |> Option.value ~default:""
    in
    let data =
      [
        ("grant_type", "authorization_code");
        ("code", auth_code);
        ("redirect_uri", redirect_uri);
        ("client_id", client_info.info.client_id);
        ("code_verifier", code_verifier);
      ]
    in
    (* Add resource parameter if needed *)
    let data =
      if should_include_resource_param context context.protocol_version then
        ("resource", get_resource_url context) :: data
      else data
    in
    (* Prepare authentication *)
    let data, headers = prepare_token_auth context data None in
    (* Make token exchange request *)
    let uri = Uri.of_string token_url in
    let body_str =
      String.concat ~sep:"&"
        (List.map data ~f:(fun (k, v) -> sprintf "%s=%s" k v))
    in
    let headers_cohttp =
      List.fold headers ~init:(Cohttp.Header.init ()) ~f:(fun h (k, v) ->
          Cohttp.Header.add h k v)
      |> fun h ->
      Cohttp.Header.add h "Content-Type" "application/x-www-form-urlencoded"
    in
    let%bind resp, body =
      Cohttp_async.Client.post ~body:(`String body_str) ~headers:headers_cohttp
        uri
    in
    let%bind token = Utils.handle_token_response_scopes resp body in
    return (Ok token)

(** Refresh access token using refresh token *)
let refresh_access_token context =
  match
    (context.oauth_metadata, context.client_info, context.current_tokens)
  with
  | None, _, _ -> return (Error "Missing OAuth metadata")
  | _, None, _ -> return (Error "Missing client info")
  | _, _, None -> return (Error "No tokens to refresh")
  | Some _metadata, Some client_info, Some tokens -> (
    match tokens.refresh_token with
    | None -> return (Error "No refresh token available")
    | Some refresh_token -> (
      let token_url = get_token_endpoint context in
      let data =
        [
          ("grant_type", "refresh_token");
          ("refresh_token", refresh_token);
          ("client_id", client_info.info.client_id);
        ]
      in
      (* Add resource parameter if needed *)
      let data =
        if should_include_resource_param context context.protocol_version then
          ("resource", get_resource_url context) :: data
        else data
      in
      (* Prepare authentication *)
      let data, headers = prepare_token_auth context data None in
      (* Make token refresh request *)
      let uri = Uri.of_string token_url in
      let body_str =
        String.concat ~sep:"&"
          (List.map data ~f:(fun (k, v) -> sprintf "%s=%s" k v))
      in
      let headers_cohttp =
        List.fold headers ~init:(Cohttp.Header.init ()) ~f:(fun h (k, v) ->
            Cohttp.Header.add h k v)
        |> fun h ->
        Cohttp.Header.add h "Content-Type" "application/x-www-form-urlencoded"
      in
      let%bind resp, body =
        Cohttp_async.Client.post ~body:(`String body_str)
          ~headers:headers_cohttp uri
      in
      Monitor.try_with (fun () -> Utils.handle_token_response_scopes resp body)
      >>= function
      | Ok token -> return (Ok token)
      | Error exn ->
        (* Clear tokens on refresh failure *)
        clear_tokens context;
        return (Error (sprintf "Token refresh failed: %s" (Exn.to_string exn))))
    )

(** Handle 401 Unauthorized response - Full OAuth flow coordination *)
let handle_unauthorized_response t response =
  let context = t.context in

  (* 1. Extract resource metadata URL from WWW-Authenticate *)
  let www_auth_url_opt =
    Utils.extract_resource_metadata_from_www_auth response
  in

  (* 2. Discover protected resource metadata *)
  let%bind prm_opt =
    discover_protected_resource_metadata www_auth_url_opt context.server_url
  in

  match prm_opt with
  | None -> return (Error "Failed to discover protected resource metadata")
  | Some prm -> (
    context.protected_resource_metadata <- Some prm;

    (* 3. Discover OAuth authorization server metadata *)
    match prm.authorization_servers with
    | [] -> return (Error "No authorization servers found")
    | auth_server :: _ -> (
      let%bind oasm_opt =
        discover_oauth_authorization_server_metadata (Some auth_server)
          context.server_url
      in

      match oasm_opt with
      | None -> return (Error "Failed to discover OAuth metadata")
      | Some oasm -> (
        context.oauth_metadata <- Some oasm;

        (* 4. Register client or use CIMD *)
        let%bind client_info_opt = register_client context in
        match client_info_opt with
        | None -> return (Error "Failed to register client")
        | Some client_info -> (
          context.client_info <- Some client_info;

          (* 5. Perform authorization code grant *)
          let%bind auth_result = perform_authorization_code_grant context in

          match auth_result with
          | Error err -> return (Error err)
          | Ok (auth_code, code_verifier) -> (
            (* 6. Exchange authorization code for tokens *)
            let%bind token_result =
              exchange_authorization_code context auth_code code_verifier
            in

            match token_result with
            | Error err -> return (Error err)
            | Ok tokens ->
              (* Store tokens *)
              context.current_tokens <- Some tokens;
              update_token_expiry context tokens;
              let (Storage (module_storage, storage_impl)) = context.storage in
              let module Storage =
                (val module_storage : Token_storage with type t = _)
              in
              let%bind () = Storage.set_tokens storage_impl tokens in
              return (Ok ()))))))

(** Handle 403 Forbidden with insufficient_scope *)
let handle_insufficient_scope_response t response =
  let context = t.context in

  (* Extract required scope from WWW-Authenticate *)
  match Utils.extract_scope_from_www_auth response with
  | None -> return (Error "No scope in 403 response")
  | Some new_scope -> (
    (* Update client metadata with new scope *)
    context.client_metadata <-
      { context.client_metadata with scope = Some new_scope };

    (* Re-authorize with new scope *)
    let%bind auth_result = perform_authorization_code_grant context in

    match auth_result with
    | Error err -> return (Error err)
    | Ok (auth_code, code_verifier) -> (
      let%bind token_result =
        exchange_authorization_code context auth_code code_verifier
      in

      match token_result with
      | Error err -> return (Error err)
      | Ok tokens ->
        context.current_tokens <- Some tokens;
        update_token_expiry context tokens;
        let (Storage (module_storage, storage_impl)) = context.storage in
        let module Storage =
          (val module_storage : Token_storage with type t = _)
        in
        let%bind () = Storage.set_tokens storage_impl tokens in
        return (Ok ())))
