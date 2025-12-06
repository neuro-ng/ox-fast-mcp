open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

let logger = Logging.Logger.get_logger "ox-fast-mcp.server.auth.providers.aws"

module Settings = struct
  type t = {
    user_pool_id : string option;
    aws_region : string option;
    client_id : string option;
    client_secret : string option;
    base_url : string option;
    issuer_url : string option;
    redirect_path : string option;
    required_scopes : string list option;
    allowed_client_redirect_uris : string list option;
    jwt_signing_key : string option;
  }
  [@@deriving sexp, yojson, compare]

  let create ?user_pool_id ?aws_region ?client_id ?client_secret ?base_url
      ?issuer_url ?redirect_path ?required_scopes ?allowed_client_redirect_uris
      ?jwt_signing_key () =
    {
      user_pool_id;
      aws_region;
      client_id;
      client_secret;
      base_url;
      issuer_url;
      redirect_path;
      required_scopes;
      allowed_client_redirect_uris;
      jwt_signing_key;
    }

  let load_from_env () =
    let get key = Sys.getenv key in
    let user_pool_id = get "FASTMCP_SERVER_AUTH_AWS_COGNITO_USER_POOL_ID" in
    let aws_region = get "FASTMCP_SERVER_AUTH_AWS_COGNITO_AWS_REGION" in
    let client_id = get "FASTMCP_SERVER_AUTH_AWS_COGNITO_CLIENT_ID" in
    let client_secret = get "FASTMCP_SERVER_AUTH_AWS_COGNITO_CLIENT_SECRET" in
    let base_url = get "FASTMCP_SERVER_AUTH_AWS_COGNITO_BASE_URL" in
    let issuer_url = get "FASTMCP_SERVER_AUTH_AWS_COGNITO_ISSUER_URL" in
    let redirect_path = get "FASTMCP_SERVER_AUTH_AWS_COGNITO_REDIRECT_PATH" in
    let required_scopes =
      match get "FASTMCP_SERVER_AUTH_AWS_COGNITO_REQUIRED_SCOPES" with
      | Some s ->
        Some
          (String.split s ~on:',' |> List.map ~f:String.strip
          |> List.filter ~f:(fun s -> not (String.is_empty s)))
      | None -> None
    in
    let allowed_client_redirect_uris =
      match
        get "FASTMCP_SERVER_AUTH_AWS_COGNITO_ALLOWED_CLIENT_REDIRECT_URIS"
      with
      | Some s ->
        Some
          (String.split s ~on:',' |> List.map ~f:String.strip
          |> List.filter ~f:(fun s -> not (String.is_empty s)))
      | None -> None
    in
    let jwt_signing_key =
      get "FASTMCP_SERVER_AUTH_AWS_COGNITO_JWT_SIGNING_KEY"
    in
    {
      user_pool_id;
      aws_region;
      client_id;
      client_secret;
      base_url;
      issuer_url;
      redirect_path;
      required_scopes;
      allowed_client_redirect_uris;
      jwt_signing_key;
    }

  let merge (t1 : t) (t2 : t) =
    {
      user_pool_id = Option.first_some t1.user_pool_id t2.user_pool_id;
      aws_region = Option.first_some t1.aws_region t2.aws_region;
      client_id = Option.first_some t1.client_id t2.client_id;
      client_secret = Option.first_some t1.client_secret t2.client_secret;
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
                 FASTMCP_SERVER_AUTH_AWS_COGNITO_%s"
                name (String.uppercase name)))
    in
    let%bind () = check "user_pool_id" t.user_pool_id in
    (* aws_region has a default, typically checked/applied later, but if
       strictly required by type we verify here *)
    let%bind () = check "client_id" t.client_id in
    let%bind () = check "client_secret" t.client_secret in
    (* base_url isn't strictly checked in Python but highly recommended. We'll
       skip strict check if defaults allow it, but for OIDC it's usually needed.
       Python version checks env or param. *)
    Ok ()
end

module Aws_cognito_provider = struct
  type t = {
    settings : Settings.t;
    config_url : string; (* Placeholder for OIDCProxy state *)
  }

  let create ?user_pool_id ?aws_region ?client_id ?client_secret ?base_url
      ?issuer_url ?redirect_path ?required_scopes ?allowed_client_redirect_uris
      ?_client_storage ?jwt_signing_key ?(require_authorization_consent = true)
      () =
    let explicit_settings =
      Settings.create ?user_pool_id ?aws_region ?client_id ?client_secret
        ?base_url ?issuer_url ?redirect_path ?required_scopes
        ?allowed_client_redirect_uris ?jwt_signing_key ()
    in
    let env_settings = Settings.load_from_env () in
    let settings = Settings.merge explicit_settings env_settings in

    match Settings.validate settings with
    | Error e -> Error e
    | Ok () ->
      let aws_region_final =
        Option.value settings.aws_region ~default:"eu-central-1"
      in
      let user_pool_id_final = Option.value_exn settings.user_pool_id in
      let config_url =
        sprintf
          "https://cognito-idp.%s.amazonaws.com/%s/.well-known/openid-configuration"
          aws_region_final user_pool_id_final
      in
      let required_scopes_final =
        Option.value settings.required_scopes ~default:[ "openid" ]
      in

      ignore require_authorization_consent;

      Logging.Logger.debug logger
        (sprintf
           "Initialized AWS Cognito OAuth provider for client %s with scopes: \
            %s"
           (Option.value_exn settings.client_id)
           (String.concat ~sep:", " required_scopes_final));

      Ok { settings; config_url }
end
