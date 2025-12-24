(** Scalekit authentication provider for OxFastMCP.

    This module provides ScalekitProvider - a complete authentication solution
    that integrates with Scalekit's OAuth 2.1 and OpenID Connect services,
    supporting Resource Server authentication for seamless MCP client
    authentication.

    Example:
    {[
      let auth =
        Scalekit.Scalekit_provider.create
          ~environment_url:"https://your-env.scalekit.com"
          ~resource_id:"sk_resource_..."
          ~base_url:"https://your-fastmcp-server.com" ()
      in
      (* Use auth with your OxFastMCP server *)
    ]} *)

open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

let logger =
  Logging.Logger.get_logger "ox-fast-mcp.server.auth.providers.scalekit"

module Settings = struct
  type t = {
    environment_url : string option;
    resource_id : string option;
    base_url : string option;
    mcp_url : string option; (* deprecated, use base_url *)
    required_scopes : string list option;
  }
  [@@deriving sexp, yojson, compare]

  let create ?environment_url ?resource_id ?base_url ?mcp_url ?required_scopes
      () =
    { environment_url; resource_id; base_url; mcp_url; required_scopes }

  (** Parse comma-separated scopes string into list *)
  let parse_scopes = function
    | None -> None
    | Some s ->
      Some
        (String.split s ~on:',' |> List.map ~f:String.strip
        |> List.filter ~f:(fun s -> not (String.is_empty s)))

  let load_from_env () =
    let get key = Sys.getenv key in
    let environment_url =
      get "OXFASTMCP_SERVER_AUTH_SCALEKITPROVIDER_ENVIRONMENT_URL"
    in
    let resource_id =
      get "OXFASTMCP_SERVER_AUTH_SCALEKITPROVIDER_RESOURCE_ID"
    in
    let base_url = get "OXFASTMCP_SERVER_AUTH_SCALEKITPROVIDER_BASE_URL" in
    let mcp_url = get "OXFASTMCP_SERVER_AUTH_SCALEKITPROVIDER_MCP_URL" in
    let required_scopes =
      parse_scopes
        (get "OXFASTMCP_SERVER_AUTH_SCALEKITPROVIDER_REQUIRED_SCOPES")
    in
    { environment_url; resource_id; base_url; mcp_url; required_scopes }

  let merge (t1 : t) (t2 : t) =
    {
      environment_url = Option.first_some t1.environment_url t2.environment_url;
      resource_id = Option.first_some t1.resource_id t2.resource_id;
      base_url = Option.first_some t1.base_url t2.base_url;
      mcp_url = Option.first_some t1.mcp_url t2.mcp_url;
      required_scopes = Option.first_some t1.required_scopes t2.required_scopes;
    }

  let validate_environment_url (t : t) =
    match t.environment_url with
    | Some _ -> Ok ()
    | None ->
      Error
        (Error.of_string
           "environment_url is required - set via parameter or \
            OXFASTMCP_SERVER_AUTH_SCALEKITPROVIDER_ENVIRONMENT_URL")

  let validate_resource_id (t : t) =
    match t.resource_id with
    | Some _ -> Ok ()
    | None ->
      Error
        (Error.of_string
           "resource_id is required - set via parameter or \
            OXFASTMCP_SERVER_AUTH_SCALEKITPROVIDER_RESOURCE_ID")

  (** Resolve base_url from either base_url or mcp_url (deprecated) *)
  let resolve_base_url (t : t) =
    match (t.base_url, t.mcp_url) with
    | Some url, _ -> Ok url
    | None, Some url -> Ok url
    | None, None ->
      Error
        (Error.of_string
           "Either base_url or mcp_url must be provided for ScalekitProvider")

  let validate (t : t) =
    match validate_environment_url t with
    | Error e -> Error e
    | Ok () -> (
      match validate_resource_id t with
      | Error e -> Error e
      | Ok () -> (
        match resolve_base_url t with
        | Error e -> Error e
        | Ok _ -> Ok ()))
end

(** Scalekit resource server provider for OAuth 2.1 authentication.

    This provider implements Scalekit integration using resource server pattern.
    OxFastMCP acts as a protected resource server that validates access tokens
    issued by Scalekit's authorization server.

    IMPORTANT SETUP REQUIREMENTS:
    1. Create an MCP Server in Scalekit Dashboard
    2. Set environment configuration variables
    3. Ensure Resource Identifier matches exactly what you configure as MCP URL *)
module Scalekit_provider = struct
  type t = {
    settings : Settings.t;
    environment_url : string;
    resource_id : string;
    base_url : string;
    required_scopes : string list;
    jwks_uri : string;
    issuer : string;
    authorization_server : string;
  }
  [@@deriving sexp, compare]

  (** Strip trailing slash from URL *)
  let strip_trailing_slash url =
    if String.is_suffix url ~suffix:"/" then
      String.chop_suffix_exn url ~suffix:"/"
    else url

  let create ?environment_url ?resource_id ?base_url ?mcp_url ?required_scopes
      ?_client_id (* deprecated, ignored *) () =
    let explicit_settings =
      Settings.create ?environment_url ?resource_id ?base_url ?mcp_url
        ?required_scopes ()
    in
    let env_settings = Settings.load_from_env () in
    let settings = Settings.merge explicit_settings env_settings in

    match Settings.validate settings with
    | Error e -> Error e
    | Ok () ->
      let environment_url =
        strip_trailing_slash (Option.value_exn settings.environment_url)
      in
      let resource_id = Option.value_exn settings.resource_id in
      let base_url =
        match Settings.resolve_base_url settings with
        | Ok url -> url
        | Error _ -> failwith "base_url should be validated"
      in
      let required_scopes = Option.value settings.required_scopes ~default:[] in

      (* Log deprecation warning if mcp_url was used *)
      (match (settings.base_url, settings.mcp_url) with
      | None, Some _ ->
        Logging.Logger.warning logger
          "ScalekitProvider parameter 'mcp_url' is deprecated and will be \
           removed in a future release. Rename it to 'base_url'."
      | _ -> ());

      (* Build JWKS and issuer URLs *)
      let jwks_uri = sprintf "%s/keys" environment_url in
      let issuer = environment_url in
      let authorization_server =
        sprintf "%s/resources/%s" environment_url resource_id
      in

      Logging.Logger.debug logger
        (sprintf
           "Initializing ScalekitProvider: environment_url=%s resource_id=%s \
            base_url=%s required_scopes=%s"
           environment_url resource_id base_url
           (String.concat ~sep:", " required_scopes));

      Logging.Logger.debug logger
        (sprintf
           "Creating default JWTVerifier for Scalekit: jwks_uri=%s issuer=%s \
            required_scopes=%s"
           jwks_uri issuer
           (String.concat ~sep:", " required_scopes));

      Ok
        {
          settings;
          environment_url;
          resource_id;
          base_url;
          required_scopes;
          jwks_uri;
          issuer;
          authorization_server;
        }

  (** Get the environment URL *)
  let environment_url t = t.environment_url

  (** Get the resource ID *)
  let resource_id t = t.resource_id

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

  (** Get the authorization server URL for this resource *)
  let authorization_server t = t.authorization_server

  (** Build the OAuth authorization server metadata URL *)
  let metadata_url t =
    sprintf "%s/.well-known/oauth-authorization-server/resources/%s"
      t.environment_url t.resource_id
end
