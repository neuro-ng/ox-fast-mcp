(** OCI OIDC provider for OxFastMCP.

    This module provides OIDC Implementation to integrate MCP servers with OCI
    (Oracle Cloud Infrastructure). You only need OCI Identity Domain's discovery
    URL, client ID, client secret, and base URL.

    Post Authentication, you get OCI IAM domain access token. That is not
    authorized to invoke OCI control plane. You need to exchange the IAM domain
    access token for OCI UPST token to invoke OCI control plane APIs.

    Example:
    {[
      let auth =
        Oci.Oci_provider.create ~config_url:"https://oci.example.com/.well-known/openid-configuration"
          ~client_id:"your-client-id" ~client_secret:"your-client-secret"
          ~base_url:"http://localhost:8000"
          ~required_scopes:[ "openid"; "profile"; "email" ] ()
      in
      (* Use auth with your OxFastMCP server *)
    ]} *)

open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

let logger = Logging.Logger.get_logger "ox-fast-mcp.server.auth.providers.oci"

module Settings = struct
  type t = {
    config_url : string option;
    client_id : string option;
    client_secret : string option;
    audience : string option;
    base_url : string option;
    issuer_url : string option;
    redirect_path : string option;
    required_scopes : string list option;
    allowed_client_redirect_uris : string list option;
    jwt_signing_key : string option;
  }
  [@@deriving sexp, yojson, compare]

  let create ?config_url ?client_id ?client_secret ?audience ?base_url
      ?issuer_url ?redirect_path ?required_scopes ?allowed_client_redirect_uris
      ?jwt_signing_key () =
    {
      config_url;
      client_id;
      client_secret;
      audience;
      base_url;
      issuer_url;
      redirect_path;
      required_scopes;
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
    let config_url = get "OXFASTMCP_SERVER_AUTH_OCI_CONFIG_URL" in
    let client_id = get "OXFASTMCP_SERVER_AUTH_OCI_CLIENT_ID" in
    let client_secret = get "OXFASTMCP_SERVER_AUTH_OCI_CLIENT_SECRET" in
    let audience = get "OXFASTMCP_SERVER_AUTH_OCI_AUDIENCE" in
    let base_url = get "OXFASTMCP_SERVER_AUTH_OCI_BASE_URL" in
    let issuer_url = get "OXFASTMCP_SERVER_AUTH_OCI_ISSUER_URL" in
    let redirect_path = get "OXFASTMCP_SERVER_AUTH_OCI_REDIRECT_PATH" in
    let required_scopes =
      parse_scopes (get "OXFASTMCP_SERVER_AUTH_OCI_REQUIRED_SCOPES")
    in
    let allowed_client_redirect_uris =
      parse_scopes
        (get "OXFASTMCP_SERVER_AUTH_OCI_ALLOWED_CLIENT_REDIRECT_URIS")
    in
    let jwt_signing_key = get "OXFASTMCP_SERVER_AUTH_OCI_JWT_SIGNING_KEY" in
    {
      config_url;
      client_id;
      client_secret;
      audience;
      base_url;
      issuer_url;
      redirect_path;
      required_scopes;
      allowed_client_redirect_uris;
      jwt_signing_key;
    }

  let merge (t1 : t) (t2 : t) =
    {
      config_url = Option.first_some t1.config_url t2.config_url;
      client_id = Option.first_some t1.client_id t2.client_id;
      client_secret = Option.first_some t1.client_secret t2.client_secret;
      audience = Option.first_some t1.audience t2.audience;
      base_url = Option.first_some t1.base_url t2.base_url;
      issuer_url = Option.first_some t1.issuer_url t2.issuer_url;
      redirect_path = Option.first_some t1.redirect_path t2.redirect_path;
      required_scopes = Option.first_some t1.required_scopes t2.required_scopes;
      allowed_client_redirect_uris =
        Option.first_some t1.allowed_client_redirect_uris
          t2.allowed_client_redirect_uris;
      jwt_signing_key = Option.first_some t1.jwt_signing_key t2.jwt_signing_key;
    }

  let validate_config_url (t : t) =
    match t.config_url with
    | Some _ -> Ok ()
    | None ->
      Error
        (Error.of_string
           "config_url is required - set via parameter or \
            OXFASTMCP_SERVER_AUTH_OCI_CONFIG_URL")

  let validate_client_id (t : t) =
    match t.client_id with
    | Some _ -> Ok ()
    | None ->
      Error
        (Error.of_string
           "client_id is required - set via parameter or \
            OXFASTMCP_SERVER_AUTH_OCI_CLIENT_ID")

  let validate_client_secret (t : t) =
    match t.client_secret with
    | Some _ -> Ok ()
    | None ->
      Error
        (Error.of_string
           "client_secret is required - set via parameter or \
            OXFASTMCP_SERVER_AUTH_OCI_CLIENT_SECRET")

  let validate_base_url (t : t) =
    match t.base_url with
    | Some _ -> Ok ()
    | None ->
      Error
        (Error.of_string
           "base_url is required - set via parameter or \
            OXFASTMCP_SERVER_AUTH_OCI_BASE_URL")

  let validate (t : t) =
    match validate_config_url t with
    | Error e -> Error e
    | Ok () -> (
      match validate_client_id t with
      | Error e -> Error e
      | Ok () -> (
        match validate_client_secret t with
        | Error e -> Error e
        | Ok () -> validate_base_url t))
end

(** An OCI IAM Domain provider implementation for OxFastMCP.

    This provider is a complete OCI integration that's ready to use with just
    the configuration URL, client ID, client secret, and base URL. *)
module Oci_provider = struct
  type t = {
    settings : Settings.t;
    config_url : string;
    client_id : string;
    client_secret : string;
    audience : string option;
    base_url : string;
    issuer_url : string option;
    redirect_path : string option;
    required_scopes : string list;
    allowed_client_redirect_uris : string list option;
    jwt_signing_key : string option;
  }
  [@@deriving sexp, compare]

  let create ?config_url ?client_id ?client_secret ?audience ?base_url
      ?issuer_url ?redirect_path ?required_scopes ?allowed_client_redirect_uris
      ?jwt_signing_key ?_require_authorization_consent () =
    let explicit_settings =
      Settings.create ?config_url ?client_id ?client_secret ?audience ?base_url
        ?issuer_url ?redirect_path ?required_scopes
        ?allowed_client_redirect_uris ?jwt_signing_key ()
    in
    let env_settings = Settings.load_from_env () in
    let settings = Settings.merge explicit_settings env_settings in

    match Settings.validate settings with
    | Error e -> Error e
    | Ok () ->
      let config_url = Option.value_exn settings.config_url in
      let client_id = Option.value_exn settings.client_id in
      let client_secret = Option.value_exn settings.client_secret in
      let base_url = Option.value_exn settings.base_url in
      let audience = settings.audience in
      let issuer_url = settings.issuer_url in
      let redirect_path = settings.redirect_path in
      let required_scopes =
        Option.value settings.required_scopes ~default:[ "openid" ]
      in
      let allowed_client_redirect_uris =
        settings.allowed_client_redirect_uris
      in
      let jwt_signing_key = settings.jwt_signing_key in

      Logging.Logger.debug logger
        (sprintf "Initialized OCI OAuth provider for client %s with scopes: %s"
           client_id
           (String.concat ~sep:", " required_scopes));

      Ok
        {
          settings;
          config_url;
          client_id;
          client_secret;
          audience;
          base_url;
          issuer_url;
          redirect_path;
          required_scopes;
          allowed_client_redirect_uris;
          jwt_signing_key;
        }

  (** Get the OIDC config URL *)
  let config_url t = t.config_url

  (** Get the client ID *)
  let client_id t = t.client_id

  (** Get the client secret *)
  let client_secret t = t.client_secret

  (** Get the audience *)
  let audience t = t.audience

  (** Get the base URL *)
  let base_url t = t.base_url

  (** Get the issuer URL *)
  let issuer_url t = t.issuer_url

  (** Get the redirect path *)
  let redirect_path t = t.redirect_path

  (** Get required scopes *)
  let required_scopes t = t.required_scopes

  (** Get allowed client redirect URIs *)
  let allowed_client_redirect_uris t = t.allowed_client_redirect_uris

  (** Get JWT signing key *)
  let jwt_signing_key t = t.jwt_signing_key
end
