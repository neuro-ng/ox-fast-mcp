(** Supabase authentication provider for OxFastMCP.

    This module provides SupabaseProvider - a complete authentication solution
    that integrates with Supabase Auth's JWT verification, supporting Dynamic
    Client Registration (DCR) for seamless MCP client authentication.

    Example:
    {[
      let auth =
        Supabase.Supabase_provider.create
          ~project_url:"https://abc123.supabase.co"
          ~base_url:"https://your-fastmcp-server.com" ~algorithm:"ES256" ()
      in
      (* Use auth with your OxFastMCP server *)
    ]} *)

open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

let logger =
  Logging.Logger.get_logger "ox-fast-mcp.server.auth.providers.supabase"

(** Supported JWT algorithms for Supabase *)
let supported_algorithms =
  Set.of_list (module String) [ "HS256"; "RS256"; "ES256" ]

module Settings = struct
  type t = {
    project_url : string option;
    base_url : string option;
    algorithm : string option;
    required_scopes : string list option;
  }
  [@@deriving sexp, yojson, compare]

  let create ?project_url ?base_url ?algorithm ?required_scopes () =
    { project_url; base_url; algorithm; required_scopes }

  (** Parse comma-separated scopes string into list *)
  let parse_scopes = function
    | None -> None
    | Some s ->
      Some
        (String.split s ~on:',' |> List.map ~f:String.strip
        |> List.filter ~f:(fun s -> not (String.is_empty s)))

  let load_from_env () =
    let get key = Sys.getenv key in
    let project_url = get "OXFASTMCP_SERVER_AUTH_SUPABASE_PROJECT_URL" in
    let base_url = get "OXFASTMCP_SERVER_AUTH_SUPABASE_BASE_URL" in
    let algorithm = get "OXFASTMCP_SERVER_AUTH_SUPABASE_ALGORITHM" in
    let required_scopes =
      parse_scopes (get "OXFASTMCP_SERVER_AUTH_SUPABASE_REQUIRED_SCOPES")
    in
    { project_url; base_url; algorithm; required_scopes }

  let merge (t1 : t) (t2 : t) =
    {
      project_url = Option.first_some t1.project_url t2.project_url;
      base_url = Option.first_some t1.base_url t2.base_url;
      algorithm = Option.first_some t1.algorithm t2.algorithm;
      required_scopes = Option.first_some t1.required_scopes t2.required_scopes;
    }

  let validate_project_url (t : t) =
    match t.project_url with
    | Some _ -> Ok ()
    | None ->
      Error
        (Error.of_string
           "project_url is required - set via parameter or \
            OXFASTMCP_SERVER_AUTH_SUPABASE_PROJECT_URL")

  let validate_base_url (t : t) =
    match t.base_url with
    | Some _ -> Ok ()
    | None ->
      Error
        (Error.of_string
           "base_url is required - set via parameter or \
            OXFASTMCP_SERVER_AUTH_SUPABASE_BASE_URL")

  let validate_algorithm (t : t) =
    match t.algorithm with
    | None -> Ok () (* default will be applied *)
    | Some alg ->
      if Set.mem supported_algorithms alg then Ok ()
      else
        Error
          (Error.of_string
             (sprintf
                "Unsupported algorithm: %s (must be HS256, RS256, or ES256)" alg))

  let validate (t : t) =
    match validate_project_url t with
    | Error e -> Error e
    | Ok () -> (
      match validate_base_url t with
      | Error e -> Error e
      | Ok () -> validate_algorithm t)
end

(** Supabase metadata provider for DCR (Dynamic Client Registration).

    This provider implements Supabase Auth integration using metadata
    forwarding. This approach allows Supabase to handle the OAuth flow directly
    while OxFastMCP acts as a resource server, verifying JWTs issued by Supabase
    Auth.

    IMPORTANT SETUP REQUIREMENTS:
    1. Create a Supabase project at https://supabase.com
    2. Configure your JWT algorithm in Supabase Auth settings
    3. Asymmetric keys (RS256/ES256) are recommended for production *)
module Supabase_provider = struct
  type t = {
    settings : Settings.t;
    project_url : string;
    base_url : string;
    algorithm : string;
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

  let create ?project_url ?base_url ?algorithm ?required_scopes () =
    let explicit_settings =
      Settings.create ?project_url ?base_url ?algorithm ?required_scopes ()
    in
    let env_settings = Settings.load_from_env () in
    let settings = Settings.merge explicit_settings env_settings in

    match Settings.validate settings with
    | Error e -> Error e
    | Ok () ->
      let project_url =
        strip_trailing_slash (Option.value_exn settings.project_url)
      in
      let base_url =
        strip_trailing_slash (Option.value_exn settings.base_url)
      in
      let algorithm = Option.value settings.algorithm ~default:"ES256" in
      let required_scopes = Option.value settings.required_scopes ~default:[] in

      (* Build JWT/JWKS endpoints *)
      let jwks_uri = sprintf "%s/auth/v1/.well-known/jwks.json" project_url in
      let issuer = sprintf "%s/auth/v1" project_url in
      let authorization_server = sprintf "%s/auth/v1" project_url in

      Logging.Logger.debug logger
        (sprintf
           "Initializing SupabaseProvider: project_url=%s base_url=%s \
            algorithm=%s"
           project_url base_url algorithm);

      Logging.Logger.debug logger
        (sprintf
           "Creating JWT Verifier for Supabase: jwks_uri=%s issuer=%s \
            algorithm=%s"
           jwks_uri issuer algorithm);

      Ok
        {
          settings;
          project_url;
          base_url;
          algorithm;
          required_scopes;
          jwks_uri;
          issuer;
          authorization_server;
        }

  (** Get the project URL *)
  let project_url t = t.project_url

  (** Get the base URL *)
  let base_url t = t.base_url

  (** Get the algorithm *)
  let algorithm t = t.algorithm

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
    sprintf "%s/auth/v1/.well-known/oauth-authorization-server" t.project_url
end
