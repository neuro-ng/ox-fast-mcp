(** WorkOS authentication providers for OxFastMCP.

    This module provides two WorkOS authentication strategies:
    1. WorkOSProvider - OAuth proxy for WorkOS Connect applications (non-DCR)
    2. AuthKitProvider - DCR-compliant provider for WorkOS AuthKit

    Example:
    {[
      let auth =
        Workos.Workos_provider.create ~client_id:"client_123"
          ~client_secret:"sk_test_456"
          ~authkit_domain:"https://your-app.authkit.app"
          ~base_url:"http://localhost:8000" ()
      in
      (* Use auth with your OxFastMCP server *)
    ]} *)

open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

let logger =
  Logging.Logger.get_logger "ox-fast-mcp.server.auth.providers.workos"

(* =============================================================================
   WorkOS OAuth Provider Settings
   ============================================================================= *)

module Workos_settings = struct
  type t = {
    client_id : string option;
    client_secret : string option;
    authkit_domain : string option;
    base_url : string option;
    issuer_url : string option;
    redirect_path : string option;
    required_scopes : string list option;
    timeout_seconds : int option;
    allowed_client_redirect_uris : string list option;
    jwt_signing_key : string option;
  }
  [@@deriving sexp, yojson, compare]

  let create ?client_id ?client_secret ?authkit_domain ?base_url ?issuer_url
      ?redirect_path ?required_scopes ?timeout_seconds
      ?allowed_client_redirect_uris ?jwt_signing_key () =
    {
      client_id;
      client_secret;
      authkit_domain;
      base_url;
      issuer_url;
      redirect_path;
      required_scopes;
      timeout_seconds;
      allowed_client_redirect_uris;
      jwt_signing_key;
    }

  (** Parse comma-separated scopes string into list *)
  let parse_scopes = function
    | None -> None
    | Some s ->
      Some
        (String.split s ~on:',' |> List.map ~f:String.strip
        |> List.filter ~f:(fun s -> not (String.is_empty s)))

  let load_from_env () =
    let get key = Sys.getenv key in
    let client_id = get "OXFASTMCP_SERVER_AUTH_WORKOS_CLIENT_ID" in
    let client_secret = get "OXFASTMCP_SERVER_AUTH_WORKOS_CLIENT_SECRET" in
    let authkit_domain = get "OXFASTMCP_SERVER_AUTH_WORKOS_AUTHKIT_DOMAIN" in
    let base_url = get "OXFASTMCP_SERVER_AUTH_WORKOS_BASE_URL" in
    let issuer_url = get "OXFASTMCP_SERVER_AUTH_WORKOS_ISSUER_URL" in
    let redirect_path = get "OXFASTMCP_SERVER_AUTH_WORKOS_REDIRECT_PATH" in
    let required_scopes =
      parse_scopes (get "OXFASTMCP_SERVER_AUTH_WORKOS_REQUIRED_SCOPES")
    in
    let timeout_seconds =
      Option.bind
        (get "OXFASTMCP_SERVER_AUTH_WORKOS_TIMEOUT_SECONDS")
        ~f:Int.of_string_opt
    in
    let allowed_client_redirect_uris =
      parse_scopes
        (get "OXFASTMCP_SERVER_AUTH_WORKOS_ALLOWED_CLIENT_REDIRECT_URIS")
    in
    let jwt_signing_key = get "OXFASTMCP_SERVER_AUTH_WORKOS_JWT_SIGNING_KEY" in
    {
      client_id;
      client_secret;
      authkit_domain;
      base_url;
      issuer_url;
      redirect_path;
      required_scopes;
      timeout_seconds;
      allowed_client_redirect_uris;
      jwt_signing_key;
    }

  let merge (t1 : t) (t2 : t) =
    {
      client_id = Option.first_some t1.client_id t2.client_id;
      client_secret = Option.first_some t1.client_secret t2.client_secret;
      authkit_domain = Option.first_some t1.authkit_domain t2.authkit_domain;
      base_url = Option.first_some t1.base_url t2.base_url;
      issuer_url = Option.first_some t1.issuer_url t2.issuer_url;
      redirect_path = Option.first_some t1.redirect_path t2.redirect_path;
      required_scopes = Option.first_some t1.required_scopes t2.required_scopes;
      timeout_seconds = Option.first_some t1.timeout_seconds t2.timeout_seconds;
      allowed_client_redirect_uris =
        Option.first_some t1.allowed_client_redirect_uris
          t2.allowed_client_redirect_uris;
      jwt_signing_key = Option.first_some t1.jwt_signing_key t2.jwt_signing_key;
    }

  let validate_client_id (t : t) =
    match t.client_id with
    | Some _ -> Ok ()
    | None ->
      Error
        (Error.of_string
           "client_id is required - set via parameter or \
            OXFASTMCP_SERVER_AUTH_WORKOS_CLIENT_ID")

  let validate_client_secret (t : t) =
    match t.client_secret with
    | Some _ -> Ok ()
    | None ->
      Error
        (Error.of_string
           "client_secret is required - set via parameter or \
            OXFASTMCP_SERVER_AUTH_WORKOS_CLIENT_SECRET")

  let validate_authkit_domain (t : t) =
    match t.authkit_domain with
    | Some _ -> Ok ()
    | None ->
      Error
        (Error.of_string
           "authkit_domain is required - set via parameter or \
            OXFASTMCP_SERVER_AUTH_WORKOS_AUTHKIT_DOMAIN")

  let validate (t : t) =
    match validate_client_id t with
    | Error e -> Error e
    | Ok () -> (
      match validate_client_secret t with
      | Error e -> Error e
      | Ok () -> validate_authkit_domain t)
end

(* =============================================================================
   WorkOS Token Verifier
   ============================================================================= *)

(** Token verifier for WorkOS OAuth tokens. WorkOS AuthKit tokens are opaque, so
    we verify them by calling the /oauth2/userinfo endpoint to check validity
    and get user info. *)
module Workos_token_verifier = struct
  type t = {
    authkit_domain : string;
    required_scopes : string list;
    timeout_seconds : int;
  }
  [@@deriving sexp, compare]

  let create ~authkit_domain ?(required_scopes = []) ?(timeout_seconds = 10) ()
      =
    let domain =
      if String.is_suffix authkit_domain ~suffix:"/" then
        String.chop_suffix_exn authkit_domain ~suffix:"/"
      else authkit_domain
    in
    { authkit_domain = domain; required_scopes; timeout_seconds }

  (** Get the authkit domain *)
  let authkit_domain t = t.authkit_domain

  (** Get required scopes *)
  let required_scopes t = t.required_scopes

  (** Get timeout in seconds *)
  let timeout_seconds t = t.timeout_seconds

  (** Build the userinfo endpoint URL *)
  let userinfo_url t = sprintf "%s/oauth2/userinfo" t.authkit_domain
end

(* =============================================================================
   WorkOS OAuth Provider
   ============================================================================= *)

(** Complete WorkOS OAuth provider for OxFastMCP.

    This provider implements WorkOS AuthKit OAuth using the OAuth Proxy pattern.
    It provides OAuth2 authentication for users through WorkOS Connect
    applications.

    Features:
    - Transparent OAuth proxy to WorkOS AuthKit
    - Automatic token validation via userinfo endpoint
    - User information extraction from ID tokens
    - Support for standard OAuth scopes (openid, profile, email) *)
module Workos_provider = struct
  type t = {
    settings : Workos_settings.t;
    client_id : string;
    client_secret : string;
    authkit_domain : string;
    base_url : string option;
    issuer_url : string option;
    redirect_path : string;
    required_scopes : string list;
    timeout_seconds : int;
    upstream_authorization_endpoint : string;
    upstream_token_endpoint : string;
    token_verifier : Workos_token_verifier.t;
  }
  [@@deriving sexp, compare]

  (** Ensure domain has https:// prefix *)
  let ensure_https domain =
    if
      String.is_prefix domain ~prefix:"http://"
      || String.is_prefix domain ~prefix:"https://"
    then domain
    else sprintf "https://%s" domain

  (** Strip trailing slash *)
  let strip_trailing_slash url =
    if String.is_suffix url ~suffix:"/" then
      String.chop_suffix_exn url ~suffix:"/"
    else url

  let create ?client_id ?client_secret ?authkit_domain ?base_url ?issuer_url
      ?redirect_path ?required_scopes ?timeout_seconds
      ?allowed_client_redirect_uris:_ ?jwt_signing_key:_ () =
    let explicit_settings =
      Workos_settings.create ?client_id ?client_secret ?authkit_domain ?base_url
        ?issuer_url ?redirect_path ?required_scopes ?timeout_seconds ()
    in
    let env_settings = Workos_settings.load_from_env () in
    let settings = Workos_settings.merge explicit_settings env_settings in

    match Workos_settings.validate settings with
    | Error e -> Error e
    | Ok () ->
      let client_id = Option.value_exn settings.client_id in
      let client_secret = Option.value_exn settings.client_secret in

      (* Ensure authkit_domain has proper https prefix and no trailing slash *)
      let authkit_domain =
        settings.authkit_domain |> Option.value_exn |> ensure_https
        |> strip_trailing_slash
      in

      let base_url = settings.base_url in
      let issuer_url = settings.issuer_url in
      let redirect_path =
        Option.value settings.redirect_path ~default:"/auth/callback"
      in
      let required_scopes = Option.value settings.required_scopes ~default:[] in
      let timeout_seconds = Option.value settings.timeout_seconds ~default:10 in

      (* Build OAuth endpoints *)
      let upstream_authorization_endpoint =
        sprintf "%s/oauth2/authorize" authkit_domain
      in
      let upstream_token_endpoint = sprintf "%s/oauth2/token" authkit_domain in

      (* Create token verifier *)
      let token_verifier =
        Workos_token_verifier.create ~authkit_domain ~required_scopes
          ~timeout_seconds ()
      in

      Logging.Logger.debug logger
        (sprintf
           "Initialized WorkOS OAuth provider for client %s with AuthKit \
            domain %s"
           client_id authkit_domain);

      Ok
        {
          settings;
          client_id;
          client_secret;
          authkit_domain;
          base_url;
          issuer_url;
          redirect_path;
          required_scopes;
          timeout_seconds;
          upstream_authorization_endpoint;
          upstream_token_endpoint;
          token_verifier;
        }

  (** Get the client ID *)
  let client_id t = t.client_id

  (** Get the client secret *)
  let client_secret t = t.client_secret

  (** Get the authkit domain *)
  let authkit_domain t = t.authkit_domain

  (** Get the base URL *)
  let base_url t = t.base_url

  (** Get the issuer URL *)
  let issuer_url t = t.issuer_url

  (** Get the redirect path *)
  let redirect_path t = t.redirect_path

  (** Get required scopes *)
  let required_scopes t = t.required_scopes

  (** Get timeout in seconds *)
  let timeout_seconds t = t.timeout_seconds

  (** Get the upstream authorization endpoint *)
  let upstream_authorization_endpoint t = t.upstream_authorization_endpoint

  (** Get the upstream token endpoint *)
  let upstream_token_endpoint t = t.upstream_token_endpoint

  (** Get the token verifier *)
  let token_verifier t = t.token_verifier
end

(* =============================================================================
   AuthKit Provider Settings
   ============================================================================= *)

module Authkit_settings = struct
  type t = {
    authkit_domain : string option;
    base_url : string option;
    required_scopes : string list option;
  }
  [@@deriving sexp, yojson, compare]

  let create ?authkit_domain ?base_url ?required_scopes () =
    { authkit_domain; base_url; required_scopes }

  (** Parse comma-separated scopes string into list *)
  let parse_scopes = function
    | None -> None
    | Some s ->
      Some
        (String.split s ~on:',' |> List.map ~f:String.strip
        |> List.filter ~f:(fun s -> not (String.is_empty s)))

  let load_from_env () =
    let get key = Sys.getenv key in
    let authkit_domain =
      get "OXFASTMCP_SERVER_AUTH_AUTHKITPROVIDER_AUTHKIT_DOMAIN"
    in
    let base_url = get "OXFASTMCP_SERVER_AUTH_AUTHKITPROVIDER_BASE_URL" in
    let required_scopes =
      parse_scopes (get "OXFASTMCP_SERVER_AUTH_AUTHKITPROVIDER_REQUIRED_SCOPES")
    in
    { authkit_domain; base_url; required_scopes }

  let merge (t1 : t) (t2 : t) =
    {
      authkit_domain = Option.first_some t1.authkit_domain t2.authkit_domain;
      base_url = Option.first_some t1.base_url t2.base_url;
      required_scopes = Option.first_some t1.required_scopes t2.required_scopes;
    }

  let validate_authkit_domain (t : t) =
    match t.authkit_domain with
    | Some _ -> Ok ()
    | None ->
      Error
        (Error.of_string
           "authkit_domain is required - set via parameter or \
            OXFASTMCP_SERVER_AUTH_AUTHKITPROVIDER_AUTHKIT_DOMAIN")

  let validate_base_url (t : t) =
    match t.base_url with
    | Some _ -> Ok ()
    | None ->
      Error
        (Error.of_string
           "base_url is required - set via parameter or \
            OXFASTMCP_SERVER_AUTH_AUTHKITPROVIDER_BASE_URL")

  let validate (t : t) =
    match validate_authkit_domain t with
    | Error e -> Error e
    | Ok () -> validate_base_url t
end

(* =============================================================================
   AuthKit Provider (DCR-compliant)
   ============================================================================= *)

(** AuthKit metadata provider for DCR (Dynamic Client Registration).

    This provider implements AuthKit integration using metadata forwarding
    instead of OAuth proxying. This is the recommended approach for WorkOS DCR
    as it allows WorkOS to handle the OAuth flow directly while OxFastMCP acts
    as a resource server. *)
module Authkit_provider = struct
  type t = {
    settings : Authkit_settings.t;
    authkit_domain : string;
    base_url : string;
    required_scopes : string list;
    jwks_uri : string;
    issuer : string;
    authorization_server : string;
  }
  [@@deriving sexp, compare]

  (** Strip trailing slash *)
  let strip_trailing_slash url =
    if String.is_suffix url ~suffix:"/" then
      String.chop_suffix_exn url ~suffix:"/"
    else url

  let create ?authkit_domain ?base_url ?required_scopes () =
    let explicit_settings =
      Authkit_settings.create ?authkit_domain ?base_url ?required_scopes ()
    in
    let env_settings = Authkit_settings.load_from_env () in
    let settings = Authkit_settings.merge explicit_settings env_settings in

    match Authkit_settings.validate settings with
    | Error e -> Error e
    | Ok () ->
      let authkit_domain =
        strip_trailing_slash (Option.value_exn settings.authkit_domain)
      in
      let base_url =
        strip_trailing_slash (Option.value_exn settings.base_url)
      in
      let required_scopes = Option.value settings.required_scopes ~default:[] in

      (* Build JWT/JWKS endpoints *)
      let jwks_uri = sprintf "%s/oauth2/jwks" authkit_domain in
      let issuer = authkit_domain in
      let authorization_server = authkit_domain in

      Logging.Logger.debug logger
        (sprintf "Initialized AuthKit provider: authkit_domain=%s base_url=%s"
           authkit_domain base_url);

      Ok
        {
          settings;
          authkit_domain;
          base_url;
          required_scopes;
          jwks_uri;
          issuer;
          authorization_server;
        }

  (** Get the authkit domain *)
  let authkit_domain t = t.authkit_domain

  (** Get the base URL *)
  let base_url t = t.base_url

  (** Get required scopes *)
  let required_scopes t = t.required_scopes

  (** Get the JWKS URI *)
  let jwks_uri t = t.jwks_uri

  (** Get the issuer *)
  let issuer t = t.issuer

  (** Get authorization servers list *)
  let authorization_servers t = [ t.authorization_server ]

  (** Get the authorization server URL *)
  let authorization_server t = t.authorization_server

  (** Build the OAuth authorization server metadata URL *)
  let metadata_url t =
    sprintf "%s/.well-known/oauth-authorization-server" t.authkit_domain
end
