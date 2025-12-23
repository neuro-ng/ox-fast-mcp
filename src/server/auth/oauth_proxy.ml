(** OAuth Proxy Provider for OxFastMCP.

    This provider acts as a transparent proxy to an upstream OAuth Authorization
    Server, handling Dynamic Client Registration locally while forwarding all
    other OAuth flows. This enables authentication with upstream providers that
    don't support DCR or have restricted client registration policies.

    Key features:
    - Proxies authorization and token endpoints to upstream server
    - Implements local Dynamic Client Registration with fixed upstream
      credentials
    - Validates tokens using upstream JWKS
    - Maintains minimal local state for bookkeeping *)

open! Core
open Cohttp
open Lwt.Syntax
open! Logging
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* -------------------------------------------------------------------------
   JSON type for Yojson.Safe.t with yojson/compare/sexp derivers
   ------------------------------------------------------------------------- *)

type json = Yojson.Safe.t

let json_of_yojson (j : Yojson.Safe.t) : json = j
let yojson_of_json (j : json) : Yojson.Safe.t = j

let compare_json (a : json) (b : json) : int =
  String.compare (Yojson.Safe.to_string a) (Yojson.Safe.to_string b)

let json_of_sexp (s : Sexp.t) : json =
  Yojson.Safe.from_string (Sexp.to_string_hum s)

let sexp_of_json (j : json) : Sexp.t =
  Parsexp.Single.parse_string_exn (Yojson.Safe.to_string j)

(* -------------------------------------------------------------------------
   Constants
   ------------------------------------------------------------------------- *)

(** Default token expiration times *)
let default_access_token_expiry_seconds = 60 * 60 (* 1 hour *)

let default_auth_code_expiry_seconds = 5 * 60 (* 5 minutes *)

(** HTTP client timeout *)
let http_timeout_seconds = 30

(** Initialize logger for OAuth Proxy *)
let logger = Logger.get_logger "OAuthProxy"

(* -------------------------------------------------------------------------
   Data Models  
   ------------------------------------------------------------------------- *)

(** OAuth transaction state for consent flow.

    Stored server-side to track active authorization flows with client context.
    Includes CSRF tokens for consent protection per MCP security best
    practices. *)
type oauth_transaction = {
  txn_id : string;
  client_id : string;
  client_redirect_uri : string;
  client_state : string;
  code_challenge : string option;
  code_challenge_method : string;
  scopes : string list;
  created_at : float;
  resource : string option;
  proxy_code_verifier : string option;
  csrf_token : string option;
  csrf_expires_at : float option;
}
[@@deriving yojson, compare, sexp]

(** Client authorization code with PKCE and upstream tokens.

    Stored server-side after upstream IdP callback. Contains the upstream
    tokens bound to the client's PKCE challenge for secure token exchange. *)
type client_code = {
  code : string;
  cc_client_id : string; [@key "client_id"]
  redirect_uri : string;
  cc_code_challenge : string option; [@key "code_challenge"]
  cc_code_challenge_method : string; [@key "code_challenge_method"]
  cc_scopes : string list; [@key "scopes"]
  idp_tokens : json;
  expires_at : float;
  cc_created_at : float; [@key "created_at"]
}
[@@deriving yojson, compare, sexp]

(** Stored upstream OAuth tokens from identity provider.

    These tokens are obtained from the upstream provider (Google, GitHub, etc.)
    and stored in plaintext within this model. Encryption is handled
    transparently at the storage layer. Tokens are never exposed to MCP
    clients. *)
type upstream_token_set = {
  upstream_token_id : string;
  access_token : string;
  refresh_token : string option;
  refresh_token_expires_at : float option;
  uts_expires_at : float; [@key "expires_at"]
  token_type : string;
  scope : string;
  uts_client_id : string; [@key "client_id"]
  uts_created_at : float; [@key "created_at"]
  raw_token_data : json;
}
[@@deriving yojson, compare, sexp]

(** Maps OxFastMCP token JTI to upstream token ID.

    This allows stateless JWT validation while still being able to look up the
    corresponding upstream token when tools need to access upstream APIs. *)
type jti_mapping = {
  jti : string;
  upstream_token_id : string;
  jm_created_at : float; [@key "created_at"]
}
[@@deriving yojson, compare, sexp]

(** Proxy DCR client with configurable redirect URI validation.

    This client class is critical for the OAuth proxy to work correctly with
    Dynamic Client Registration (DCR). *)
type proxy_dcr_client = {
  pdc_client_id : string; [@key "client_id"]
  client_secret : string option;
  redirect_uris : string list;
  allowed_redirect_uri_patterns : string list option;
  client_name : string option;
  pdc_created_at : float; [@key "created_at"]
}
[@@deriving yojson, compare, sexp]

(* -------------------------------------------------------------------------
   In-Memory Storage (simplified from Python's key_value library)
   ------------------------------------------------------------------------- *)

module Storage = struct
  type 'a t = { mutable data : (string, 'a) Hashtbl.t }

  let create () = { data = Hashtbl.create (module String) }
  let get t ~key = Hashtbl.find t.data key
  let put t ~key ~value = Hashtbl.set t.data ~key ~data:value

  let remove t ~key =
    Hashtbl.remove t.data key;
    ()

  let clear t = Hashtbl.clear t.data
end

(* -------------------------------------------------------------------------
   PKCE Utilities
   ------------------------------------------------------------------------- *)

module Pkce = struct
  (** Generate PKCE code verifier and challenge pair using S256 method *)
  let generate_pair () =
    let verifier =
      Base64.encode_exn ~alphabet:Base64.uri_safe_alphabet
        (Mirage_crypto_rng.generate 32 |> Cstruct.to_string)
    in
    let challenge =
      Mirage_crypto.Hash.SHA256.digest (Cstruct.of_string verifier)
      |> Cstruct.to_string
      |> Base64.encode_exn ~alphabet:Base64.uri_safe_alphabet
    in
    (verifier, challenge)

  (** Verify PKCE code verifier against challenge *)
  let verify ~verifier ~challenge ~method_ =
    match method_ with
    | "S256" ->
      let computed =
        Mirage_crypto.Hash.SHA256.digest (Cstruct.of_string verifier)
        |> Cstruct.to_string
        |> Base64.encode_exn ~alphabet:Base64.uri_safe_alphabet
      in
      String.equal computed challenge
    | "plain" -> String.equal verifier challenge
    | _ -> false
end

(* -------------------------------------------------------------------------
   Cookie Utilities
   ------------------------------------------------------------------------- *)

module Cookie = struct
  (** Sign a cookie payload with HMAC-SHA256 *)
  let sign ~secret ~payload =
    let signature =
      Mirage_crypto.Hash.SHA256.hmac ~key:(Cstruct.of_string secret)
        (Cstruct.of_string payload)
      |> Cstruct.to_string |> Base64.encode_exn
    in
    Base64.encode_exn payload ^ "." ^ signature

  (** Verify and extract payload from signed cookie *)
  let verify ~secret ~signed_value =
    match String.lsplit2 signed_value ~on:'.' with
    | None -> None
    | Some (payload_b64, signature) -> (
      match Base64.decode payload_b64 with
      | Error _ -> None
      | Ok payload ->
        let expected =
          Mirage_crypto.Hash.SHA256.hmac ~key:(Cstruct.of_string secret)
            (Cstruct.of_string payload)
          |> Cstruct.to_string |> Base64.encode_exn
        in
        if String.equal expected signature then Some payload else None)
end

(* -------------------------------------------------------------------------
   Token Verifier Interface 
   ------------------------------------------------------------------------- *)

module type TOKEN_VERIFIER = sig
  val required_scopes : string list

  val verify_token :
    string -> Mcp_server_auth.Provider.access_token option Lwt.t
end

(* -------------------------------------------------------------------------
   OAuth Proxy Configuration
   ------------------------------------------------------------------------- *)

type config = {
  upstream_authorization_endpoint : string;
  upstream_token_endpoint : string;
  upstream_client_id : string;
  upstream_client_secret : string;
  upstream_revocation_endpoint : string option;
  base_url : string;
  redirect_path : string;
  issuer_url : string option;
  service_documentation_url : string option;
  allowed_client_redirect_uris : string list option;
  valid_scopes : string list option;
  forward_pkce : bool;
  token_endpoint_auth_method : string option;
  extra_authorize_params : (string * string) list option;
  extra_token_params : (string * string) list option;
  jwt_signing_key : string option;
  require_authorization_consent : bool;
  consent_csp_policy : string option;
  required_scopes : string list;
}
[@@deriving compare, sexp]

(* -------------------------------------------------------------------------
   OAuth Proxy State
   ------------------------------------------------------------------------- *)

type t = {
  config : config;
  transaction_store : oauth_transaction Storage.t;
  code_store : client_code Storage.t;
  upstream_token_store : upstream_token_set Storage.t;
  jti_mapping_store : jti_mapping Storage.t;
  client_store : proxy_dcr_client Storage.t;
  mutable token_verifier : (module TOKEN_VERIFIER) option;
}

(* -------------------------------------------------------------------------
   Creation
   ------------------------------------------------------------------------- *)

(** Create a new OAuth proxy provider *)
let create ~upstream_authorization_endpoint ~upstream_token_endpoint
    ~upstream_client_id ~upstream_client_secret ?upstream_revocation_endpoint
    ~base_url ?(redirect_path = "/auth/callback") ?issuer_url
    ?service_documentation_url ?allowed_client_redirect_uris ?valid_scopes
    ?(forward_pkce = true) ?token_endpoint_auth_method ?extra_authorize_params
    ?extra_token_params ?jwt_signing_key ?(require_authorization_consent = true)
    ?consent_csp_policy ~required_scopes () =
  let redirect_path =
    if String.is_prefix redirect_path ~prefix:"/" then redirect_path
    else "/" ^ redirect_path
  in
  let () =
    match allowed_client_redirect_uris with
    | Some [] ->
      Logger.warning logger
        "allowed_client_redirect_uris is empty list; no redirect URIs will be \
         accepted."
    | _ -> ()
  in
  let () =
    if not require_authorization_consent then
      Logger.warning logger
        "Authorization consent is disabled. This is not recommended for \
         production."
  in
  {
    config =
      {
        upstream_authorization_endpoint;
        upstream_token_endpoint;
        upstream_client_id;
        upstream_client_secret;
        upstream_revocation_endpoint;
        base_url;
        redirect_path;
        issuer_url;
        service_documentation_url;
        allowed_client_redirect_uris;
        valid_scopes;
        forward_pkce;
        token_endpoint_auth_method;
        extra_authorize_params;
        extra_token_params;
        jwt_signing_key;
        require_authorization_consent;
        consent_csp_policy;
        required_scopes;
      };
    transaction_store = Storage.create ();
    code_store = Storage.create ();
    upstream_token_store = Storage.create ();
    jti_mapping_store = Storage.create ();
    client_store = Storage.create ();
    token_verifier = None;
  }

(** Set the token verifier for the proxy *)
let set_token_verifier t verifier = t.token_verifier <- Some verifier

(* -------------------------------------------------------------------------
   Client Registration (DCR)
   ------------------------------------------------------------------------- *)

(** Get client information by ID *)
let get_client t ~client_id = Lwt.return (Storage.get t.client_store ~key:client_id)

(** Register a client locally with DCR *)
let register_client t ~client_id ?client_secret ~redirect_uris ?client_name () =
  let client =
    {
      pdc_client_id = client_id;
      client_secret;
      redirect_uris;
      allowed_redirect_uri_patterns = t.config.allowed_client_redirect_uris;
      client_name;
      pdc_created_at = Core_unix.gettimeofday ();
    }
  in
  Storage.put t.client_store ~key:client_id ~value:client;
  let () =
    Logger.info logger (Printf.sprintf "Registered DCR client: %s" client_id)
  in
  Lwt.return client

(** Validate redirect URI for a client *)
let validate_redirect_uri t ~client:_ ~redirect_uri =
  Redirect_validation.validate_redirect_uri ~redirect_uri:(Some redirect_uri)
    ~allowed_patterns:t.config.allowed_client_redirect_uris

(* -------------------------------------------------------------------------
   Authorization Flow
   ------------------------------------------------------------------------- *)

(** Generate a secure transaction ID *)
let generate_txn_id () =
  Mirage_crypto_rng.generate 32
  |> Cstruct.to_string
  |> Base64.encode_exn ~alphabet:Base64.uri_safe_alphabet

(** Generate a CSRF token *)
let generate_csrf_token () =
  Mirage_crypto_rng.generate 32
  |> Cstruct.to_string
  |> Base64.encode_exn ~alphabet:Base64.uri_safe_alphabet

(** Start OAuth transaction - returns consent page URL *)
let authorize t ~client_id ~redirect_uri ~state ~code_challenge
    ?code_challenge_method ~scopes ?resource () =
  let code_challenge_method =
    Option.value code_challenge_method ~default:"S256"
  in
  let txn_id = generate_txn_id () in
  let csrf_token = generate_csrf_token () in
  let now = Core_unix.gettimeofday () in
  let proxy_code_verifier =
    if t.config.forward_pkce then
      let verifier, _ = Pkce.generate_pair () in
      Some verifier
    else None
  in
  let transaction =
    {
      txn_id;
      client_id;
      client_redirect_uri = redirect_uri;
      client_state = state;
      code_challenge;
      code_challenge_method;
      scopes;
      created_at = now;
      resource;
      proxy_code_verifier;
      csrf_token = Some csrf_token;
      csrf_expires_at = Some (now +. 600.0);
    }
  in
  Storage.put t.transaction_store ~key:txn_id ~value:transaction;
  let () =
    Logger.debug logger
      (Printf.sprintf "Created OAuth transaction: %s for client %s" txn_id
         client_id)
  in
  (* Return consent page URL *)
  let consent_url =
    Printf.sprintf "%s/consent?txn_id=%s" t.config.base_url txn_id
  in
  Lwt.return consent_url

(* -------------------------------------------------------------------------
   Authorization Code Exchange
   ------------------------------------------------------------------------- *)

(** Load authorization code for validation *)
let load_authorization_code t ~code =
  let* () = Lwt.return () in
  match Storage.get t.code_store ~key:code with
  | None ->
    let () =
      Logger.debug logger (Printf.sprintf "Authorization code not found: %s" code)
    in
    Lwt.return None
  | Some client_code ->
    let now = Core_unix.gettimeofday () in
    if Float.(client_code.expires_at < now) then (
      Storage.remove t.code_store ~key:code;
      let () =
        Logger.debug logger
          (Printf.sprintf "Authorization code expired: %s" code)
      in
      Lwt.return None)
    else Lwt.return (Some client_code)

(** Exchange authorization code for tokens *)
let exchange_authorization_code t ~client_id ~code ~code_verifier =
  let* client_code_opt = load_authorization_code t ~code in
  match client_code_opt with
  | None -> Lwt.return (Error "Invalid authorization code")
  | Some client_code ->
    (* Verify client *)
    if not (String.equal client_code.cc_client_id client_id) then
      Lwt.return (Error "Client ID mismatch")
    else
      (* Verify PKCE if present *)
      let pkce_valid =
        match client_code.cc_code_challenge with
        | None -> true
        | Some challenge ->
          Pkce.verify ~verifier:code_verifier ~challenge
            ~method_:client_code.cc_code_challenge_method
      in
      if not pkce_valid then Lwt.return (Error "PKCE verification failed")
      else (
        (* Remove used code *)
        Storage.remove t.code_store ~key:code;
        (* Return the stored IDP tokens *)
        let () =
          Logger.info logger
            (Printf.sprintf "Exchanged authorization code for client: %s"
               client_id)
        in
        Lwt.return (Ok client_code.idp_tokens))

(* -------------------------------------------------------------------------
   Refresh Token
   ------------------------------------------------------------------------- *)

(** Load refresh token from local storage *)
let load_refresh_token t ~token =
  let* () = Lwt.return () in
  (* Look up the JTI mapping to find upstream token *)
  match Storage.get t.jti_mapping_store ~key:token with
  | None ->
    let () =
      Logger.debug logger (Printf.sprintf "Refresh token not found: %s" token)
    in
    Lwt.return None
  | Some mapping -> (
    match Storage.get t.upstream_token_store ~key:mapping.upstream_token_id with
    | None -> Lwt.return None
    | Some upstream_token -> Lwt.return (Some upstream_token))

(** Exchange refresh token for new access token.

    Note: This is a stub - actual upstream token refresh would require HTTP
    client implementation *)
let exchange_refresh_token t ~client_id ~refresh_token ~scopes:_ =
  let* upstream_opt = load_refresh_token t ~token:refresh_token in
  match upstream_opt with
  | None -> Lwt.return (Error "Invalid refresh token")
  | Some upstream ->
    if not (String.equal upstream.uts_client_id client_id) then
      Lwt.return (Error "Client ID mismatch")
    else (
      (* TODO: Actually refresh with upstream provider *)
      let () =
        Logger.info logger
          (Printf.sprintf "Refresh token exchange for client: %s" client_id)
      in
      (* Return existing upstream token for now *)
      Lwt.return (Ok (yojson_of_upstream_token_set upstream)))

(* -------------------------------------------------------------------------
   Access Token Validation
   ------------------------------------------------------------------------- *)

(** Validate access token *)
let load_access_token t ~token =
  let* () = Lwt.return () in
  match t.token_verifier with
  | None ->
    let () =
      Logger.warning logger "Token verifier not set, rejecting access token"
    in
    Lwt.return None
  | Some (module V : TOKEN_VERIFIER) -> V.verify_token token

(* -------------------------------------------------------------------------
   Token Revocation
   ------------------------------------------------------------------------- *)

(** Revoke token locally and with upstream server if supported *)
let revoke_token t ~token =
  let* () = Lwt.return () in
  (* Remove from JTI mapping *)
  Storage.remove t.jti_mapping_store ~key:token;
  let () =
    Logger.info logger (Printf.sprintf "Revoked token: %s..." (String.prefix token 10))
  in
  (* TODO: Forward revocation to upstream if endpoint configured *)
  Lwt.return ()

(* -------------------------------------------------------------------------
   HTTP Route Handlers
   ------------------------------------------------------------------------- *)

(** Build upstream authorization URL from transaction *)
let build_upstream_authorize_url t ~txn_id ~transaction =
  let base_params =
    [
      ("client_id", t.config.upstream_client_id);
      ("redirect_uri", t.config.base_url ^ t.config.redirect_path);
      ("response_type", "code");
      ("state", txn_id);
      ("scope", String.concat ~sep:" " transaction.scopes);
    ]
  in
  let pkce_params =
    match transaction.proxy_code_verifier with
    | None -> []
    | Some verifier ->
      let _, challenge = Pkce.generate_pair () in
      ignore verifier;
      [ ("code_challenge", challenge); ("code_challenge_method", "S256") ]
  in
  let extra_params = Option.value t.config.extra_authorize_params ~default:[] in
  let all_params = base_params @ pkce_params @ extra_params in
  let query = Uri.encoded_of_query (List.map all_params ~f:(fun (k, v) -> (k, [v]))) in
  t.config.upstream_authorization_endpoint ^ "?" ^ query

(** Create consent page HTML *)
let create_consent_html ~client_id ~redirect_uri ~scopes ~txn_id ~csrf_token
    ?client_name ?server_name () =
  let client_display = Option.value client_name ~default:client_id in
  let server_display = Option.value server_name ~default:"OxFastMCP" in
  let scopes_str = String.concat ~sep:", " scopes in
  Printf.sprintf
    {|<!DOCTYPE html>
<html>
<head>
  <meta charset="UTF-8">
  <title>Application Access Request</title>
  <style>
    body { font-family: system-ui, sans-serif; max-width: 600px; margin: 50px auto; padding: 20px; }
    .container { background: #f9f9f9; border-radius: 8px; padding: 30px; }
    h1 { color: #333; margin-bottom: 20px; }
    .info-box { background: #e8f4f8; padding: 15px; border-radius: 4px; margin: 20px 0; }
    .redirect-section { background: #fff3cd; padding: 15px; border-radius: 4px; margin: 20px 0; text-align: center; }
    .button-group { display: flex; gap: 10px; margin-top: 20px; }
    .btn-approve { background: #22c55e; color: white; padding: 12px 24px; border: none; border-radius: 6px; cursor: pointer; }
    .btn-deny { background: #ef4444; color: white; padding: 12px 24px; border: none; border-radius: 6px; cursor: pointer; }
  </style>
</head>
<body>
  <div class="container">
    <h1>Application Access Request</h1>
    <div class="info-box">
      <p>The application <strong>%s</strong> wants to access the MCP server <strong>%s</strong>.</p>
    </div>
    <div class="redirect-section">
      <span>Credentials will be sent to:</span>
      <div><strong>%s</strong></div>
    </div>
    <details>
      <summary>Advanced Details</summary>
      <p>Scopes: %s</p>
    </details>
    <form method="POST" action="">
      <input type="hidden" name="txn_id" value="%s" />
      <input type="hidden" name="csrf_token" value="%s" />
      <div class="button-group">
        <button type="submit" name="action" value="approve" class="btn-approve">Allow Access</button>
        <button type="submit" name="action" value="deny" class="btn-deny">Deny</button>
      </div>
    </form>
  </div>
</body>
</html>|}
    client_display server_display redirect_uri scopes_str txn_id csrf_token

(** Create error page HTML *)
let create_error_html ~error_title ~error_message ?error_details () =
  let details_html =
    match error_details with
    | None -> ""
    | Some details ->
      let rows =
        List.map details ~f:(fun (k, v) ->
            Printf.sprintf "<p><strong>%s:</strong> %s</p>" k v)
      in
      "<div class=\"details\">" ^ String.concat ~sep:"\n" rows ^ "</div>"
  in
  Printf.sprintf
    {|<!DOCTYPE html>
<html>
<head>
  <meta charset="UTF-8">
  <title>%s</title>
  <style>
    body { font-family: system-ui, sans-serif; max-width: 600px; margin: 50px auto; padding: 20px; }
    .container { background: #fef2f2; border-radius: 8px; padding: 30px; }
    h1 { color: #991b1b; }
    .details { margin-top: 20px; color: #666; }
  </style>
</head>
<body>
  <div class="container">
    <h1>%s</h1>
    <p>%s</p>
    %s
  </div>
</body>
</html>|}
    error_title error_title error_message details_html

(** Handle consent page GET request *)
let handle_consent_get t ~txn_id =
  match Storage.get t.transaction_store ~key:txn_id with
  | None ->
    let html =
      create_error_html ~error_title:"Invalid Request"
        ~error_message:"Transaction not found or expired." ()
    in
    let headers = Header.of_list [ ("Content-Type", "text/html") ] in
    let response = Response.make ~status:`Bad_request ~headers () in
    Lwt.return (response, Body.of_string html)
  | Some transaction ->
    let csrf_token = Option.value transaction.csrf_token ~default:"" in
    let html =
      create_consent_html ~client_id:transaction.client_id
        ~redirect_uri:transaction.client_redirect_uri ~scopes:transaction.scopes
        ~txn_id ~csrf_token ()
    in
    let headers = Header.of_list [ ("Content-Type", "text/html") ] in
    let response = Response.make ~status:`OK ~headers () in
    Lwt.return (response, Body.of_string html)

(** Handle consent page POST request *)
let handle_consent_post t ~txn_id ~csrf_token ~action =
  let* () = Lwt.return () in
  match Storage.get t.transaction_store ~key:txn_id with
  | None ->
    let html =
      create_error_html ~error_title:"Invalid Request"
        ~error_message:"Transaction not found or expired." ()
    in
    let headers = Header.of_list [ ("Content-Type", "text/html") ] in
    let response = Response.make ~status:`Bad_request ~headers () in
    Lwt.return (response, Body.of_string html)
  | Some transaction -> (
    (* Verify CSRF token *)
    let csrf_valid =
      match transaction.csrf_token with
      | None -> false
      | Some expected -> String.equal expected csrf_token
    in
    if not csrf_valid then (
      let html =
        create_error_html ~error_title:"Invalid Request"
          ~error_message:"CSRF token validation failed." ()
      in
      let headers = Header.of_list [ ("Content-Type", "text/html") ] in
      let response = Response.make ~status:`Bad_request ~headers () in
      Lwt.return (response, Body.of_string html))
    else
      match action with
      | "deny" ->
        (* Remove transaction and redirect with error *)
        Storage.remove t.transaction_store ~key:txn_id;
        let redirect_uri =
          Printf.sprintf "%s?error=access_denied&state=%s"
            transaction.client_redirect_uri transaction.client_state
        in
        let headers = Header.of_list [ ("Location", redirect_uri) ] in
        let response = Response.make ~status:`Found ~headers () in
        Lwt.return (response, Body.empty)
      | "approve" ->
        (* Build upstream URL and redirect *)
        let upstream_url = build_upstream_authorize_url t ~txn_id ~transaction in
        let headers = Header.of_list [ ("Location", upstream_url) ] in
        let response = Response.make ~status:`Found ~headers () in
        Lwt.return (response, Body.empty)
      | _ ->
        let html =
          create_error_html ~error_title:"Invalid Request"
            ~error_message:"Unknown action." ()
        in
        let headers = Header.of_list [ ("Content-Type", "text/html") ] in
        let response = Response.make ~status:`Bad_request ~headers () in
        Lwt.return (response, Body.of_string html))

(** Handle IDP callback and forward to client *)
let handle_idp_callback t ~code ~state =
  let txn_id = state in
  let* () = Lwt.return () in
  match Storage.get t.transaction_store ~key:txn_id with
  | None ->
    let () =
      Logger.warning logger
        (Printf.sprintf "IDP callback: transaction not found: %s" txn_id)
    in
    let html =
      create_error_html ~error_title:"Authorization Failed"
        ~error_message:"Transaction not found or expired." ()
    in
    let headers = Header.of_list [ ("Content-Type", "text/html") ] in
    let response = Response.make ~status:`Bad_request ~headers () in
    Lwt.return (response, Body.of_string html)
  | Some transaction ->
    (* Store the code for later exchange *)
    let client_code_value = generate_txn_id () in
    let now = Core_unix.gettimeofday () in
    let client_code =
      {
        code = client_code_value;
        cc_client_id = transaction.client_id;
        redirect_uri = transaction.client_redirect_uri;
        cc_code_challenge = transaction.code_challenge;
        cc_code_challenge_method = transaction.code_challenge_method;
        cc_scopes = transaction.scopes;
        idp_tokens = `Assoc [ ("upstream_code", `String code) ];
        expires_at =
          now +. Float.of_int default_auth_code_expiry_seconds;
        cc_created_at = now;
      }
    in
    Storage.put t.code_store ~key:client_code_value ~value:client_code;
    (* Remove used transaction *)
    Storage.remove t.transaction_store ~key:txn_id;
    (* Redirect to client with our code *)
    let redirect_uri =
      Printf.sprintf "%s?code=%s&state=%s" transaction.client_redirect_uri
        client_code_value transaction.client_state
    in
    let () =
      Logger.info logger
        (Printf.sprintf "IDP callback: redirecting to client %s"
           transaction.client_id)
    in
    let headers = Header.of_list [ ("Location", redirect_uri) ] in
    let response = Response.make ~status:`Found ~headers () in
    Lwt.return (response, Body.empty)

(** Route handler type *)
type route = {
  path : string;
  methods : string list;
  handler : Request.t -> (Response.t * Body.t) Lwt.t;
}

(** Get OAuth routes for this proxy *)
let get_routes t =
  [
    { path = "/consent"; methods = [ "GET"; "POST" ]; handler = (fun _req ->
        (* TODO: Parse request to get txn_id, csrf_token, action *)
        handle_consent_get t ~txn_id:"" (* placeholder *)
      )
    };
    { path = t.config.redirect_path; methods = [ "GET" ]; handler = (fun _req ->
        (* TODO: Parse request to get code and state *)
        handle_idp_callback t ~code:"" ~state:"" (* placeholder *)
      )
    };
  ]
