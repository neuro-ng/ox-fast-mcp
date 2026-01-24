(** Descope authentication provider for OxFastMCP.

    This module provides Descope_provider - a complete authentication solution
    that integrates with Descope's OAuth 2.1 and OpenID Connect services,
    supporting Dynamic Client Registration (DCR) for seamless MCP client
    authentication.

    Important Setup Requirements:
    1. Create an MCP Server in Descope Console
    2. Enable Dynamic Client Registration (DCR)
    3. Note your Well-Known URL

    For detailed setup instructions, see:
    https://docs.descope.com/identity-federation/inbound-apps/creating-inbound-apps *)

open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

let logger =
  Logging.Logger.get_logger "ox-fast-mcp.server.auth.providers.descope"

module Settings = struct
  type t = {
    config_url : string option;
    project_id : string option;
    descope_base_url : string option;
    base_url : string option;
    required_scopes : string list option;
  }
  [@@deriving sexp, yojson, compare]

  let create ?config_url ?project_id ?descope_base_url ?base_url
      ?required_scopes () =
    { config_url; project_id; descope_base_url; base_url; required_scopes }

  (** Parse comma-separated scopes string into list *)
  let parse_scopes = function
    | None -> None
    | Some s ->
      Some
        (String.split s ~on:',' |> List.map ~f:String.strip
        |> List.filter ~f:(fun s -> not (String.is_empty s)))

  let load_from_env () =
    let get key = Sys.getenv key in
    let config_url = get "OXFASTMCP_SERVER_AUTH_DESCOPEPROVIDER_CONFIG_URL" in
    let project_id = get "OXFASTMCP_SERVER_AUTH_DESCOPEPROVIDER_PROJECT_ID" in
    let descope_base_url =
      get "OXFASTMCP_SERVER_AUTH_DESCOPEPROVIDER_DESCOPE_BASE_URL"
    in
    let base_url = get "OXFASTMCP_SERVER_AUTH_DESCOPEPROVIDER_BASE_URL" in
    let required_scopes =
      parse_scopes (get "OXFASTMCP_SERVER_AUTH_DESCOPEPROVIDER_REQUIRED_SCOPES")
    in
    { config_url; project_id; descope_base_url; base_url; required_scopes }

  let merge (t1 : t) (t2 : t) =
    {
      config_url = Option.first_some t1.config_url t2.config_url;
      project_id = Option.first_some t1.project_id t2.project_id;
      descope_base_url =
        Option.first_some t1.descope_base_url t2.descope_base_url;
      base_url = Option.first_some t1.base_url t2.base_url;
      required_scopes = Option.first_some t1.required_scopes t2.required_scopes;
    }

  let validate (t : t) =
    match t.config_url with
    | Some _ -> Ok ()
    | None -> (
      match (t.project_id, t.descope_base_url) with
      | Some _, Some _ -> Ok ()
      | _ ->
        Error
          (Error.of_string
             "Either config_url (new API) or both project_id and \
              descope_base_url (old API) must be provided"))

  let validate_base_url (t : t) =
    match t.base_url with
    | Some _ -> Ok ()
    | None ->
      Error
        (Error.of_string
           "base_url is required - set via parameter or \
            OXFASTMCP_SERVER_AUTH_DESCOPEPROVIDER_BASE_URL")
end

module Descope_provider = struct
  type t = {
    settings : Settings.t;
    project_id : string;
    descope_base_url : string;
    base_url : string;
    issuer_url : string;
    jwks_uri : string;
    required_scopes : string list;
  }
  [@@deriving sexp, compare]

  (** Strip .well-known/openid-configuration suffix from URL if present *)
  let strip_well_known_suffix url =
    let suffix = "/.well-known/openid-configuration" in
    if String.is_suffix url ~suffix then
      String.prefix url (String.length url - String.length suffix)
    else url

  (** Parse config URL to extract project_id and descope_base_url.

      Config URL format:
      https://api.descope.com/v1/apps/agentic/P.../M.../.well-known/openid-configuration

      Returns (project_id, descope_base_url, issuer_url) *)
  let parse_config_url config_url =
    let issuer_url = strip_well_known_suffix config_url in
    let uri = Uri.of_string issuer_url in
    let path = Uri.path uri |> String.strip ~drop:(Char.equal '/') in
    let path_parts = String.split path ~on:'/' in

    (* Extract project_id from path (format: /v1/apps/agentic/P.../M...) *)
    let project_id =
      match List.findi path_parts ~f:(fun _ p -> String.equal p "agentic") with
      | Some (agentic_index, _) -> (
        match List.nth path_parts (agentic_index + 1) with
        | Some proj_id -> Ok proj_id
        | None ->
          Error
            (Error.of_string
               (sprintf "Could not extract project_id from config_url: %s"
                  issuer_url)))
      | None ->
        Error
          (Error.of_string
             (sprintf "Could not find 'agentic' in config_url path: %s"
                issuer_url))
    in

    (* Extract descope_base_url (scheme + host) *)
    let scheme = Uri.scheme uri |> Option.value ~default:"https" in
    let host = Uri.host uri |> Option.value ~default:"" in
    let port = Uri.port uri in
    let descope_base_url =
      match port with
      | Some p -> sprintf "%s://%s:%d" scheme host p
      | None -> sprintf "%s://%s" scheme host
    in

    match project_id with
    | Ok pid -> Ok (pid, descope_base_url, issuer_url)
    | Error e -> Error e

  (** Ensure URL has a scheme, defaulting to https:// *)
  let ensure_scheme url =
    if
      String.is_prefix url ~prefix:"http://"
      || String.is_prefix url ~prefix:"https://"
    then url
    else "https://" ^ url

  let create ?config_url ?project_id ?descope_base_url ?base_url
      ?required_scopes ?_token_verifier () =
    let explicit_settings =
      Settings.create ?config_url ?project_id ?descope_base_url ?base_url
        ?required_scopes ()
    in
    let env_settings = Settings.load_from_env () in
    let settings = Settings.merge explicit_settings env_settings in

    (* Validate settings *)
    match Settings.validate settings with
    | Error e -> Error e
    | Ok () -> (
      match Settings.validate_base_url settings with
      | Error e -> Error e
      | Ok () -> (
        let base_url =
          String.rstrip ~drop:(Char.equal '/')
            (Option.value_exn settings.base_url)
        in

        (* Determine which API is being used and extract values *)
        let result =
          match settings.config_url with
          | Some cfg_url ->
            (* New API: parse config_url *)
            parse_config_url cfg_url
          | None -> (
            (* Old API: use project_id and descope_base_url *)
            match (settings.project_id, settings.descope_base_url) with
            | Some pid, Some dbu ->
              let dbu_normalized =
                String.rstrip ~drop:(Char.equal '/') (ensure_scheme dbu)
              in
              let issuer_url = sprintf "%s/v1/apps/%s" dbu_normalized pid in
              Ok (pid, dbu_normalized, issuer_url)
            | _ ->
              Error
                (Error.of_string
                   "Either config_url or both project_id and descope_base_url \
                    required"))
        in

        match result with
        | Error e -> Error e
        | Ok (proj_id, dbu, issuer) ->
          let jwks_uri = sprintf "%s/%s/.well-known/jwks.json" dbu proj_id in
          let scopes = Option.value settings.required_scopes ~default:[] in

          Logging.Logger.debug logger
            (sprintf
               "Initialized Descope OAuth provider for project %s with scopes: \
                %s"
               proj_id
               (String.concat ~sep:", " scopes));

          Ok
            {
              settings;
              project_id = proj_id;
              descope_base_url = dbu;
              base_url;
              issuer_url = issuer;
              jwks_uri;
              required_scopes = scopes;
            }))

  (** Get the project ID *)
  let project_id t = t.project_id

  (** Get the Descope base URL *)
  let descope_base_url t = t.descope_base_url

  (** Get the base URL *)
  let base_url t = t.base_url

  (** Get the issuer URL *)
  let issuer_url t = t.issuer_url

  (** Get the JWKS URI *)
  let jwks_uri t = t.jwks_uri

  (** Get required scopes *)
  let required_scopes t = t.required_scopes

  (** Get OAuth authorization server metadata URL *)
  let oauth_metadata_url t =
    sprintf "%s/v1/apps/%s/.well-known/oauth-authorization-server"
      t.descope_base_url t.project_id
end
