(** OAuth Client Utility Functions *)

open Core
open Async

(* {1 Constants} *)

let mcp_protocol_version_header = "MCP-Protocol-Version"

(* {1 WWW-Authenticate Header Parsing} *)

let extract_field_from_www_auth response field_name =
  let headers = Cohttp.Response.headers response in
  match Cohttp.Header.get headers "WWW-Authenticate" with
  | None -> None
  | Some www_auth_header -> (
    (* Pattern matches: field_name="value" or field_name=value (unquoted) *)
    (* Manually escape special regex characters in field_name *)
    let escaped_field_name =
      String.concat_map field_name ~f:(fun c ->
          match c with
          | '.'
          | '*'
          | '+'
          | '?'
          | '['
          | ']'
          | '('
          | ')'
          | '{'
          | '}'
          | '|'
          | '^'
          | '$'
          | '\\' -> "\\" ^ String.of_char c
          | _ -> String.of_char c)
    in
    let pattern_str =
      Printf.sprintf {|%s=(?:"([^"]+)"|([^\s,]+))|} escaped_field_name
    in
    let pattern = Re.Perl.compile_pat pattern_str in
    match Re.exec_opt pattern www_auth_header with
    | None -> None
    | Some groups -> (
      (* Return quoted value if present (group 1), otherwise unquoted value
         (group 2) *)
      try Some (Re.Group.get groups 1)
      with _ -> ( try Some (Re.Group.get groups 2) with _ -> None)))

let extract_scope_from_www_auth response =
  extract_field_from_www_auth response "scope"

let extract_resource_metadata_from_www_auth response =
  match Cohttp.Response.status response with
  | `Unauthorized -> extract_field_from_www_auth response "resource_metadata"
  | _ -> None

(* {1 URL Building for Discovery} *)

let build_protected_resource_metadata_discovery_urls www_auth_url server_url =
  let urls = ref [] in
  (* Priority 1: WWW-Authenticate header with resource_metadata parameter *)
  (match www_auth_url with
  | Some url -> urls := url :: !urls
  | None -> ());
  (* Priority 2-3: Well-known URIs (RFC 9728) *)
  let parsed = Uri.of_string server_url in
  let scheme = Uri.scheme parsed |> Option.value ~default:"https" in
  let host = Uri.host parsed |> Option.value ~default:"" in
  let port = Uri.port parsed in
  let base_url =
    match port with
    | Some p -> Printf.sprintf "%s://%s:%d" scheme host p
    | None -> Printf.sprintf "%s://%s" scheme host
  in
  let path = Uri.path parsed in
  (* Priority 2: Path-based well-known URI (if server has a path component) *)
  (match path with
  | "" | "/" -> ()
  | p ->
    let path_based_url =
      Uri.with_path (Uri.of_string base_url)
        ("/.well-known/oauth-protected-resource" ^ p)
      |> Uri.to_string
    in
    urls := path_based_url :: !urls);
  (* Priority 3: Root-based well-known URI *)
  let root_based_url =
    Uri.with_path (Uri.of_string base_url)
      "/.well-known/oauth-protected-resource"
    |> Uri.to_string
  in
  urls := root_based_url :: !urls;
  List.rev !urls

let build_oauth_authorization_server_metadata_discovery_urls auth_server_url
    server_url =
  match auth_server_url with
  | None ->
    (* Legacy path using the 2025-03-26 spec *)
    let parsed = Uri.of_string server_url in
    let scheme = Uri.scheme parsed |> Option.value ~default:"https" in
    let host = Uri.host parsed |> Option.value ~default:"" in
    let port = Uri.port parsed in
    let base_url =
      match port with
      | Some p -> Printf.sprintf "%s://%s:%d" scheme host p
      | None -> Printf.sprintf "%s://%s" scheme host
    in
    [
      Uri.with_path (Uri.of_string base_url)
        "/.well-known/oauth-authorization-server"
      |> Uri.to_string;
    ]
  | Some auth_url ->
    let urls = ref [] in
    let parsed = Uri.of_string auth_url in
    let scheme = Uri.scheme parsed |> Option.value ~default:"https" in
    let host = Uri.host parsed |> Option.value ~default:"" in
    let port = Uri.port parsed in
    let base_url =
      match port with
      | Some p -> Printf.sprintf "%s://%s:%d" scheme host p
      | None -> Printf.sprintf "%s://%s" scheme host
    in
    let path = Uri.path parsed in
    (* RFC 8414: Path-aware OAuth discovery *)
    (match path with
    | "" | "/" ->
      (* Root: OAuth root, OIDC root *)
      urls :=
        (Uri.with_path (Uri.of_string base_url)
           "/.well-known/oauth-authorization-server"
        |> Uri.to_string)
        :: !urls;
      urls :=
        (Uri.with_path (Uri.of_string base_url)
           "/.well-known/openid-configuration"
        |> Uri.to_string)
        :: !urls
    | p ->
      let p_stripped = String.rstrip ~drop:(Char.( = ) '/') p in
      (* Path-aware: OAuth path-aware, OIDC path-aware, OIDC suffix paths *)
      urls :=
        (Uri.with_path (Uri.of_string base_url)
           ("/.well-known/oauth-authorization-server" ^ p_stripped)
        |> Uri.to_string)
        :: !urls;
      urls :=
        (Uri.with_path (Uri.of_string base_url)
           ("/.well-known/openid-configuration" ^ p_stripped)
        |> Uri.to_string)
        :: !urls;
      urls :=
        (Uri.with_path (Uri.of_string base_url)
           (p_stripped ^ "/.well-known/openid-configuration")
        |> Uri.to_string)
        :: !urls);
    List.rev !urls

(* {1 Scope Selection} *)

let get_client_metadata_scopes ~www_authenticate_scope
    ~protected_resource_metadata
    ?(authorization_server_metadata : Mcp_shared.Auth.oauth_metadata option =
      None) () =
  match www_authenticate_scope with
  | Some scope -> Some scope (* Priority 1: WWW-Authenticate header scope *)
  | None -> (
    match protected_resource_metadata with
    | Some prm -> (
      match prm.Mcp_shared.Auth.scopes_supported with
      | Some scopes ->
        Some (String.concat ~sep:" " scopes)
        (* Priority 2: PRM scopes_supported *)
      | None -> (
        match authorization_server_metadata with
        | Some asm -> (
          match asm.Mcp_shared.Auth.scopes_supported with
          | Some scopes -> Some (String.concat ~sep:" " scopes)
          (* Priority 3: OASM scopes_supported *)
          | None -> None)
        | None -> None))
    | None -> (
      match authorization_server_metadata with
      | Some asm -> (
        match asm.Mcp_shared.Auth.scopes_supported with
        | Some scopes -> Some (String.concat ~sep:" " scopes)
        | None -> None)
      | None -> None))

(* {1 CIMD (Client ID Metadata Document) Support} *)

let is_valid_client_metadata_url url =
  match url with
  | None -> false
  | Some u -> (
    try
      let parsed = Uri.of_string u in
      let scheme = Uri.scheme parsed |> Option.value ~default:"" in
      let path = Uri.path parsed in
      String.( = ) scheme "https"
      && not (String.is_empty path || String.( = ) path "/")
    with _ -> false)

let should_use_client_metadata_url ~oauth_metadata ~client_metadata_url =
  match (client_metadata_url, oauth_metadata) with
  | None, _ | _, None -> false
  | Some _, Some (metadata : Mcp_shared.Auth.oauth_metadata) -> (
    match metadata.client_id_metadata_document_supported with
    | Some true -> true
    | _ -> false)

let create_client_info_from_metadata_url ~client_metadata_url
    ?(redirect_uris = []) () =
  let redirect_uris_str = List.map redirect_uris ~f:Uri.to_string in
  {
    Mcp_shared.Auth.metadata =
      {
        redirect_uris = redirect_uris_str;
        token_endpoint_auth_method = `None;
        grant_types = [ `Authorization_code; `Refresh_token ];
        response_types = [ `Code ];
        scope = None;
        client_name = None;
        client_uri = None;
        logo_uri = None;
        contacts = None;
        tos_uri = None;
        policy_uri = None;
        jwks_uri = None;
        jwks = None;
        software_id = None;
        software_version = None;
      };
    info =
      {
        client_id = client_metadata_url;
        client_secret = None;
        client_id_issued_at = None;
        client_secret_expires_at = None;
      };
  }

(* {1 Async Response Handlers} *)

let handle_protected_resource_response response body =
  match Cohttp.Response.status response with
  | `OK -> (
    Cohttp_async.Body.to_string body >>| fun content ->
    try
      let json = Yojson.Safe.from_string content in
      let metadata =
        Mcp_shared.Auth.protected_resource_metadata_of_yojson json
      in
      Some metadata
    with _ -> None)
  | _ -> return None

let handle_auth_metadata_response response body =
  let code = Cohttp.Response.status response |> Cohttp.Code.code_of_status in
  match code with
  | 200 -> (
    Cohttp_async.Body.to_string body >>| fun content ->
    try
      let json = Yojson.Safe.from_string content in
      let asm = Mcp_shared.Auth.oauth_metadata_of_yojson json in
      (true, Some asm)
    with _ -> (true, None))
  | c when c < 400 || c >= 500 ->
    return (false, None) (* Non-4XX error, stop trying *)
  | _ -> return (true, None)

let handle_registration_response response body =
  let code = Cohttp.Response.status response |> Cohttp.Code.code_of_status in
  if code = 200 || code = 201 then
    Cohttp_async.Body.to_string body >>= fun content ->
    try
      let json = Yojson.Safe.from_string content in
      let client_info =
        Mcp_shared.Auth.oauth_client_information_full_of_yojson json
      in
      return client_info
    with exn ->
      raise (Exceptions.Oauth_registration_error (Exn.to_string exn))
  else
    Cohttp_async.Body.to_string body >>= fun text ->
    raise
      (Exceptions.Oauth_registration_error
         (Printf.sprintf "Registration failed: %d %s" code text))

let handle_token_response_scopes _response body =
  Cohttp_async.Body.to_string body >>= fun content ->
  try
    let json = Yojson.Safe.from_string content in
    let token_response = Mcp_shared.Auth.oauth_token_of_yojson json in
    return token_response
  with exn -> raise (Exceptions.Oauth_token_error (Exn.to_string exn))

(* {1 Request Builders} *)

let create_oauth_metadata_request url =
  let uri = Uri.of_string url in
  let headers =
    Cohttp.Header.init () |> fun h ->
    Cohttp.Header.add h mcp_protocol_version_header
      Mcp.Types.latest_protocol_version
  in
  Cohttp.Request.make ~meth:`GET ~headers uri

let create_client_registration_request ~auth_server_metadata ~client_metadata
    ~auth_base_url =
  let registration_url =
    match auth_server_metadata with
    | Some asm -> (
      match asm.Mcp_shared.Auth.registration_endpoint with
      | Some endpoint -> endpoint
      | None ->
        Uri.with_path (Uri.of_string auth_base_url) "/register" |> Uri.to_string
      )
    | None ->
      Uri.with_path (Uri.of_string auth_base_url) "/register" |> Uri.to_string
  in
  let json = Mcp_shared.Auth.yojson_of_oauth_client_metadata client_metadata in
  let json_string = Yojson.Safe.to_string json in
  let uri = Uri.of_string registration_url in
  let headers =
    Cohttp.Header.init () |> fun h ->
    Cohttp.Header.add h "Content-Type" "application/json"
  in
  let _body = Cohttp_async.Body.of_string json_string in
  (* Note: Cohttp.Request.make doesn't directly support body, but we return it
     for the caller to use *)
  Cohttp.Request.make ~meth:`POST ~headers uri
