(** GitHub authentication provider for OxFastMCP.

    This module provides a complete GitHub OAuth integration that's ready to use
    with just a client ID and client secret. It handles all the complexity of
    GitHub's OAuth flow, token validation, and user management.

    Example:
    {[
      let auth =
        Github.Github_provider.create ~client_id:"Ov23li..."
          ~client_secret:"abc123..." ~base_url:"https://my-server.com" ()
      in
      (* Use auth with your OxFastMCP server *)
    ]} *)

open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

let logger =
  Logging.Logger.get_logger "ox-fast-mcp.server.auth.providers.github"

(** GitHub OAuth endpoints *)
let github_authorization_endpoint = "https://github.com/login/oauth/authorize"

let github_token_endpoint = "https://github.com/login/oauth/access_token"
let github_user_api = "https://api.github.com/user"
let github_repos_api = "https://api.github.com/user/repos"

module Settings = struct
  type t = {
    client_id : string option;
    client_secret : string option;
    base_url : string option;
    issuer_url : string option;
    redirect_path : string option;
    required_scopes : string list option;
    timeout_seconds : int option;
    allowed_client_redirect_uris : string list option;
    jwt_signing_key : string option;
  }
  [@@deriving sexp, yojson, compare]

  let create ?client_id ?client_secret ?base_url ?issuer_url ?redirect_path
      ?required_scopes ?timeout_seconds ?allowed_client_redirect_uris
      ?jwt_signing_key () =
    {
      client_id;
      client_secret;
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
    let client_id = get "OXFASTMCP_SERVER_AUTH_GITHUB_CLIENT_ID" in
    let client_secret = get "OXFASTMCP_SERVER_AUTH_GITHUB_CLIENT_SECRET" in
    let base_url = get "OXFASTMCP_SERVER_AUTH_GITHUB_BASE_URL" in
    let issuer_url = get "OXFASTMCP_SERVER_AUTH_GITHUB_ISSUER_URL" in
    let redirect_path = get "OXFASTMCP_SERVER_AUTH_GITHUB_REDIRECT_PATH" in
    let required_scopes =
      parse_scopes (get "OXFASTMCP_SERVER_AUTH_GITHUB_REQUIRED_SCOPES")
    in
    let timeout_seconds =
      match get "OXFASTMCP_SERVER_AUTH_GITHUB_TIMEOUT_SECONDS" with
      | Some s -> Int.of_string_opt s
      | None -> None
    in
    let allowed_client_redirect_uris =
      parse_scopes
        (get "OXFASTMCP_SERVER_AUTH_GITHUB_ALLOWED_CLIENT_REDIRECT_URIS")
    in
    let jwt_signing_key = get "OXFASTMCP_SERVER_AUTH_GITHUB_JWT_SIGNING_KEY" in
    {
      client_id;
      client_secret;
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
            OXFASTMCP_SERVER_AUTH_GITHUB_CLIENT_ID")

  let validate_client_secret (t : t) =
    match t.client_secret with
    | Some _ -> Ok ()
    | None ->
      Error
        (Error.of_string
           "client_secret is required - set via parameter or \
            OXFASTMCP_SERVER_AUTH_GITHUB_CLIENT_SECRET")

  let validate (t : t) =
    match validate_client_id t with
    | Error e -> Error e
    | Ok () -> validate_client_secret t
end

(** GitHub token verifier configuration.

    GitHub OAuth tokens are opaque (not JWTs), so we verify them by calling
    GitHub's API to check if they're valid and get user info. *)
module Github_token_verifier = struct
  type t = { required_scopes : string list; timeout_seconds : int }
  [@@deriving sexp, compare]

  let create ?(required_scopes = []) ?(timeout_seconds = 10) () =
    { required_scopes; timeout_seconds }

  (** Get the required scopes *)
  let required_scopes t = t.required_scopes

  (** Get the timeout in seconds *)
  let timeout_seconds t = t.timeout_seconds

  (** Default User-Agent header for GitHub API requests *)
  let user_agent = "OxFastMCP-GitHub-OAuth"
end

(** GitHub OAuth provider for OxFastMCP.

    This provider makes it trivial to add GitHub OAuth protection to any
    OxFastMCP server. Just provide your GitHub OAuth app credentials and a base
    URL, and you're ready to go.

    Features:
    - Transparent OAuth proxy to GitHub
    - Automatic token validation via GitHub API
    - User information extraction
    - Minimal configuration required *)
module Github_provider = struct
  type t = {
    settings : Settings.t;
    client_id : string;
    client_secret : string;
    base_url : string option;
    issuer_url : string option;
    redirect_path : string;
    required_scopes : string list;
    timeout_seconds : int;
    allowed_client_redirect_uris : string list option;
    jwt_signing_key : string option;
    token_verifier : Github_token_verifier.t;
  }
  [@@deriving sexp, compare]

  let create ?client_id ?client_secret ?base_url ?issuer_url ?redirect_path
      ?required_scopes ?timeout_seconds ?allowed_client_redirect_uris
      ?jwt_signing_key ?_require_authorization_consent () =
    let explicit_settings =
      Settings.create ?client_id ?client_secret ?base_url ?issuer_url
        ?redirect_path ?required_scopes ?timeout_seconds
        ?allowed_client_redirect_uris ?jwt_signing_key ()
    in
    let env_settings = Settings.load_from_env () in
    let settings = Settings.merge explicit_settings env_settings in

    match Settings.validate settings with
    | Error e -> Error e
    | Ok () ->
      let client_id = Option.value_exn settings.client_id in
      let client_secret = Option.value_exn settings.client_secret in
      let base_url = settings.base_url in
      let issuer_url =
        Option.first_some settings.issuer_url settings.base_url
      in
      let redirect_path =
        Option.value settings.redirect_path ~default:"/auth/callback"
      in
      let required_scopes_final =
        Option.value settings.required_scopes ~default:[ "user" ]
      in
      let timeout_seconds_final =
        Option.value settings.timeout_seconds ~default:10
      in
      let allowed_client_redirect_uris =
        settings.allowed_client_redirect_uris
      in
      let jwt_signing_key = settings.jwt_signing_key in

      (* Create token verifier *)
      let token_verifier =
        Github_token_verifier.create ~required_scopes:required_scopes_final
          ~timeout_seconds:timeout_seconds_final ()
      in

      Logging.Logger.debug logger
        (sprintf
           "Initialized GitHub OAuth provider for client %s with scopes: %s"
           client_id
           (String.concat ~sep:", " required_scopes_final));

      Ok
        {
          settings;
          client_id;
          client_secret;
          base_url;
          issuer_url;
          redirect_path;
          required_scopes = required_scopes_final;
          timeout_seconds = timeout_seconds_final;
          allowed_client_redirect_uris;
          jwt_signing_key;
          token_verifier;
        }

  (** Get the client ID *)
  let client_id t = t.client_id

  (** Get the client secret *)
  let client_secret t = t.client_secret

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

  (** Get allowed client redirect URIs *)
  let allowed_client_redirect_uris t = t.allowed_client_redirect_uris

  (** Get JWT signing key *)
  let jwt_signing_key t = t.jwt_signing_key

  (** Get the token verifier *)
  let token_verifier t = t.token_verifier

  (** Get the GitHub authorization endpoint *)
  let authorization_endpoint () = github_authorization_endpoint

  (** Get the GitHub token endpoint *)
  let token_endpoint () = github_token_endpoint
end
