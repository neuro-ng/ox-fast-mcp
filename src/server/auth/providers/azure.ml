open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives
(* open Ox_fast_mcp.Utilities *)

let logger = Logging.Logger.get_logger "ox-fast-mcp.server.auth.providers.azure"

module Settings = struct
  type t = {
    client_id : string option;
    client_secret : string option;
    tenant_id : string option;
    identifier_uri : string option;
    base_url : string option;
    issuer_url : string option;
    redirect_path : string option;
    required_scopes : string list option;
    additional_authorize_scopes : string list option;
    allowed_client_redirect_uris : string list option;
    jwt_signing_key : string option;
    base_authority : string option;
  }
  [@@deriving sexp, yojson, compare]

  let create ?client_id ?client_secret ?tenant_id ?identifier_uri ?base_url
      ?issuer_url ?redirect_path ?required_scopes ?additional_authorize_scopes
      ?allowed_client_redirect_uris ?jwt_signing_key
      ?(base_authority = "login.microsoftonline.com") () =
    {
      client_id;
      client_secret;
      tenant_id;
      identifier_uri;
      base_url;
      issuer_url;
      redirect_path;
      required_scopes;
      additional_authorize_scopes;
      allowed_client_redirect_uris;
      jwt_signing_key;
      base_authority = Some base_authority;
    }

  let load_from_env () =
    let get key = Sys.getenv key in
    let client_id = get "FASTMCP_SERVER_AUTH_AZURE_CLIENT_ID" in
    let client_secret = get "FASTMCP_SERVER_AUTH_AZURE_CLIENT_SECRET" in
    let tenant_id = get "FASTMCP_SERVER_AUTH_AZURE_TENANT_ID" in
    let identifier_uri = get "FASTMCP_SERVER_AUTH_AZURE_IDENTIFIER_URI" in
    let base_url = get "FASTMCP_SERVER_AUTH_AZURE_BASE_URL" in
    let issuer_url = get "FASTMCP_SERVER_AUTH_AZURE_ISSUER_URL" in
    let redirect_path = get "FASTMCP_SERVER_AUTH_AZURE_REDIRECT_PATH" in
    let required_scopes =
      match get "FASTMCP_SERVER_AUTH_AZURE_REQUIRED_SCOPES" with
      | Some s ->
        Some
          (String.split s ~on:',' |> List.map ~f:String.strip
          |> List.filter ~f:(fun s -> not (String.is_empty s)))
      | None -> None
    in
    let additional_authorize_scopes =
      match get "FASTMCP_SERVER_AUTH_AZURE_ADDITIONAL_AUTHORIZE_SCOPES" with
      | Some s ->
        Some
          (String.split s ~on:',' |> List.map ~f:String.strip
          |> List.filter ~f:(fun s -> not (String.is_empty s)))
      | None -> None
    in
    let allowed_client_redirect_uris =
      match get "FASTMCP_SERVER_AUTH_AZURE_ALLOWED_CLIENT_REDIRECT_URIS" with
      | Some s ->
        Some
          (String.split s ~on:',' |> List.map ~f:String.strip
          |> List.filter ~f:(fun s -> not (String.is_empty s)))
      | None -> None
    in
    let jwt_signing_key = get "FASTMCP_SERVER_AUTH_AZURE_JWT_SIGNING_KEY" in
    let base_authority = get "FASTMCP_SERVER_AUTH_AZURE_BASE_AUTHORITY" in
    {
      client_id;
      client_secret;
      tenant_id;
      identifier_uri;
      base_url;
      issuer_url;
      redirect_path;
      required_scopes;
      additional_authorize_scopes;
      allowed_client_redirect_uris;
      jwt_signing_key;
      base_authority;
    }

  let merge (t1 : t) (t2 : t) =
    {
      client_id = Option.first_some t1.client_id t2.client_id;
      client_secret = Option.first_some t1.client_secret t2.client_secret;
      tenant_id = Option.first_some t1.tenant_id t2.tenant_id;
      identifier_uri = Option.first_some t1.identifier_uri t2.identifier_uri;
      base_url = Option.first_some t1.base_url t2.base_url;
      issuer_url = Option.first_some t1.issuer_url t2.issuer_url;
      redirect_path = Option.first_some t1.redirect_path t2.redirect_path;
      required_scopes = Option.first_some t1.required_scopes t2.required_scopes;
      additional_authorize_scopes =
        Option.first_some t1.additional_authorize_scopes
          t2.additional_authorize_scopes;
      allowed_client_redirect_uris =
        Option.first_some t1.allowed_client_redirect_uris
          t2.allowed_client_redirect_uris;
      jwt_signing_key = Option.first_some t1.jwt_signing_key t2.jwt_signing_key;
      base_authority = Option.first_some t1.base_authority t2.base_authority;
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
                 FASTMCP_SERVER_AUTH_AZURE_%s"
                name (String.uppercase name)))
    in
    let%bind () = check "client_id" t.client_id in
    let%bind () = check "client_secret" t.client_secret in

    let%bind () =
      match t.tenant_id with
      | Some _ -> Ok ()
      | None ->
        Error
          (Error.of_string
             "tenant_id is required - set via parameter or \
              FASTMCP_SERVER_AUTH_AZURE_TENANT_ID. Use your Azure tenant ID \
              (found in Azure Portal), 'organizations', or 'consumers'")
    in

    let%bind () =
      match t.required_scopes with
      | Some (_ :: _) -> Ok () (* Non-empty list *)
      | Some [] | None ->
        Error
          (Error.of_string
             "required_scopes must include at least one scope - set via \
              parameter or FASTMCP_SERVER_AUTH_AZURE_REQUIRED_SCOPES. Azure's \
              OAuth API requires the 'scope' parameter in authorization \
              requests. Use the unprefixed scope names from your Azure App \
              registration (e.g., ['read', 'write'])")
    in

    Ok ()
end

module Azure_provider = struct
  type t = {
    settings : Settings.t;
    identifier_uri : string;
    authorization_endpoint : string;
    token_endpoint : string;
    issuer : string;
    jwks_uri : string; (* Placeholder for OAuthProxy state *)
  }

  let prefix_scopes_for_azure identifier_uri scopes =
    List.map scopes ~f:(fun scope ->
        if
          String.is_substring scope ~substring:"://"
          || String.is_substring scope ~substring:"/"
        then scope
        else sprintf "%s/%s" identifier_uri scope)

  let create ?client_id ?client_secret ?tenant_id ?identifier_uri ?base_url
      ?issuer_url ?redirect_path ?required_scopes ?additional_authorize_scopes
      ?allowed_client_redirect_uris ?_client_storage ?jwt_signing_key
      ?(require_authorization_consent = true) ?base_authority () =
    let explicit_settings =
      Settings.create ?client_id ?client_secret ?tenant_id ?identifier_uri
        ?base_url ?issuer_url ?redirect_path ?required_scopes
        ?additional_authorize_scopes ?allowed_client_redirect_uris
        ?jwt_signing_key ?base_authority ()
    in
    let env_settings = Settings.load_from_env () in
    let settings = Settings.merge explicit_settings env_settings in

    match Settings.validate settings with
    | Error e -> Error e
    | Ok () ->
      let client_id_final = Option.value_exn settings.client_id in
      let tenant_id_final = Option.value_exn settings.tenant_id in
      let base_authority_final =
        Option.value settings.base_authority
          ~default:"login.microsoftonline.com"
      in
      let identifier_uri_final =
        Option.value settings.identifier_uri
          ~default:(sprintf "api://%s" client_id_final)
      in

      let authorization_endpoint =
        sprintf "https://%s/%s/oauth2/v2.0/authorize" base_authority_final
          tenant_id_final
      in
      let token_endpoint =
        sprintf "https://%s/%s/oauth2/v2.0/token" base_authority_final
          tenant_id_final
      in
      let issuer =
        sprintf "https://%s/%s/v2.0" base_authority_final tenant_id_final
      in
      let jwks_uri =
        sprintf "https://%s/%s/discovery/v2.0/keys" base_authority_final
          tenant_id_final
      in

      ignore require_authorization_consent;

      let authority_info =
        if String.(base_authority_final <> "login.microsoftonline.com") then
          sprintf " using authority %s" base_authority_final
        else ""
      in

      Logging.Logger.info logger
        (sprintf
           "Initialized Azure OAuth provider for client %s with tenant %s%s%s"
           client_id_final tenant_id_final
           (if Option.is_some settings.identifier_uri then
              sprintf " and identifier_uri %s" identifier_uri_final
            else "")
           authority_info);

      Ok
        {
          settings;
          identifier_uri = identifier_uri_final;
          authorization_endpoint;
          token_endpoint;
          issuer;
          jwks_uri;
        }
end
