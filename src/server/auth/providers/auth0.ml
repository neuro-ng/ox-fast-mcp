open Core

(* open Lwt *)
open Ppx_yojson_conv_lib.Yojson_conv.Primitives
(* open Ox_fast_mcp.Utilities *)

(* Flag missing dependency *)
(* Missing: fastmcp.server.auth.oidc_proxy (OIDCProxy) *)
(* Missing: fastmcp.utilities.auth (parse_scopes) *)

let logger = Logging.Logger.get_logger "ox-fast-mcp.server.auth.providers.auth0"

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

  let load_from_env () =
    let get key = Sys.getenv key in
    let config_url = get "FASTMCP_SERVER_AUTH_AUTH0_CONFIG_URL" in
    let client_id = get "FASTMCP_SERVER_AUTH_AUTH0_CLIENT_ID" in
    let client_secret = get "FASTMCP_SERVER_AUTH_AUTH0_CLIENT_SECRET" in
    let audience = get "FASTMCP_SERVER_AUTH_AUTH0_AUDIENCE" in
    let base_url = get "FASTMCP_SERVER_AUTH_AUTH0_BASE_URL" in
    let issuer_url = get "FASTMCP_SERVER_AUTH_AUTH0_ISSUER_URL" in
    let redirect_path = get "FASTMCP_SERVER_AUTH_AUTH0_REDIRECT_PATH" in
    let required_scopes =
      match get "FASTMCP_SERVER_AUTH_AUTH0_REQUIRED_SCOPES" with
      | Some s ->
        Some
          (String.split s ~on:',' |> List.map ~f:String.strip
          |> List.filter ~f:(fun s -> not (String.is_empty s)))
      | None -> None
    in
    let allowed_client_redirect_uris =
      match get "FASTMCP_SERVER_AUTH_AUTH0_ALLOWED_CLIENT_REDIRECT_URIS" with
      | Some s ->
        Some
          (String.split s ~on:',' |> List.map ~f:String.strip
          |> List.filter ~f:(fun s -> not (String.is_empty s)))
      | None -> None
    in
    let jwt_signing_key = get "FASTMCP_SERVER_AUTH_AUTH0_JWT_SIGNING_KEY" in
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

  let validate (t : t) =
    let open Result.Let_syntax in
    let check name field =
      match field with
      | Some _ -> Ok ()
      | None ->
        Error
          (Error.of_string
             (sprintf
                "%s is required - set via parameter or \
                 FASTMCP_SERVER_AUTH_AUTH0_%s"
                name (String.uppercase name)))
    in
    let%bind () = check "config_url" t.config_url in
    let%bind () = check "client_id" t.client_id in
    let%bind () = check "client_secret" t.client_secret in
    let%bind () = check "audience" t.audience in
    let%bind () = check "base_url" t.base_url in
    Ok ()
end

module Auth0_provider = struct
  type t = { settings : Settings.t (* Placeholder for OIDCProxy state *) }

  let create ?config_url ?client_id ?client_secret ?audience ?base_url
      ?issuer_url ?redirect_path ?required_scopes ?allowed_client_redirect_uris
      ?_client_storage ?jwt_signing_key ?(require_authorization_consent = true)
      () =
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
      let auth0_required_scopes =
        Option.value settings.required_scopes ~default:[ "openid" ]
      in
      ignore require_authorization_consent;
      (* This is where we would call super().__init__ / OIDCProxy.create *)
      (* Since OIDCProxy and parse_scopes are missing, we log and return the object *)
      Logging.Logger.debug logger
        (sprintf
           "Initialized Auth0 OAuth provider for client %s with scopes: %s"
           (Option.value_exn settings.client_id)
           (String.concat ~sep:", " auth0_required_scopes));

      Ok { settings }
end
