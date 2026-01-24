(** OAuth 2.0 Token Introspection (RFC 7662) provider for OxFastMCP.

    This module provides token verification for opaque tokens using the OAuth
    2.0 Token Introspection protocol defined in RFC 7662. It allows OxFastMCP
    servers to validate tokens issued by authorization servers that don't use
    JWT format.

    Example:
    {[
      let verifier =
        Introspection.Introspection_token_verifier.create
          ~introspection_url:"https://auth.example.com/oauth/introspect"
          ~client_id:"your-client-id" ~client_secret:"your-client-secret"
          ~required_scopes:[ "read"; "write" ] ()
      in
      (* Use verifier with your OxFastMCP server *)
    ]} *)

open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

let logger =
  Logging.Logger.get_logger "ox-fast-mcp.server.auth.providers.introspection"

module Settings = struct
  type t = {
    introspection_url : string option;
    client_id : string option;
    client_secret : string option;
    timeout_seconds : int;
    required_scopes : string list option;
    base_url : string option;
  }
  [@@deriving sexp, yojson, compare]

  let create ?introspection_url ?client_id ?client_secret
      ?(timeout_seconds = 10) ?required_scopes ?base_url () =
    {
      introspection_url;
      client_id;
      client_secret;
      timeout_seconds;
      required_scopes;
      base_url;
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
    let introspection_url =
      get "OXFASTMCP_SERVER_AUTH_INTROSPECTION_INTROSPECTION_URL"
    in
    let client_id = get "OXFASTMCP_SERVER_AUTH_INTROSPECTION_CLIENT_ID" in
    let client_secret =
      get "OXFASTMCP_SERVER_AUTH_INTROSPECTION_CLIENT_SECRET"
    in
    let timeout_seconds =
      match get "OXFASTMCP_SERVER_AUTH_INTROSPECTION_TIMEOUT_SECONDS" with
      | Some s -> Int.of_string_opt s |> Option.value ~default:10
      | None -> 10
    in
    let required_scopes =
      parse_scopes (get "OXFASTMCP_SERVER_AUTH_INTROSPECTION_REQUIRED_SCOPES")
    in
    let base_url = get "OXFASTMCP_SERVER_AUTH_INTROSPECTION_BASE_URL" in
    {
      introspection_url;
      client_id;
      client_secret;
      timeout_seconds;
      required_scopes;
      base_url;
    }

  let merge (t1 : t) (t2 : t) =
    {
      introspection_url =
        Option.first_some t1.introspection_url t2.introspection_url;
      client_id = Option.first_some t1.client_id t2.client_id;
      client_secret = Option.first_some t1.client_secret t2.client_secret;
      timeout_seconds =
        (if t1.timeout_seconds <> 10 then t1.timeout_seconds
         else t2.timeout_seconds);
      required_scopes = Option.first_some t1.required_scopes t2.required_scopes;
      base_url = Option.first_some t1.base_url t2.base_url;
    }

  let validate_introspection_url (t : t) =
    match t.introspection_url with
    | Some _ -> Ok ()
    | None ->
      Error
        (Error.of_string
           "introspection_url is required - set via parameter or \
            OXFASTMCP_SERVER_AUTH_INTROSPECTION_INTROSPECTION_URL")

  let validate_client_id (t : t) =
    match t.client_id with
    | Some _ -> Ok ()
    | None ->
      Error
        (Error.of_string
           "client_id is required - set via parameter or \
            OXFASTMCP_SERVER_AUTH_INTROSPECTION_CLIENT_ID")

  let validate_client_secret (t : t) =
    match t.client_secret with
    | Some _ -> Ok ()
    | None ->
      Error
        (Error.of_string
           "client_secret is required - set via parameter or \
            OXFASTMCP_SERVER_AUTH_INTROSPECTION_CLIENT_SECRET")

  let validate (t : t) =
    match validate_introspection_url t with
    | Error e -> Error e
    | Ok () -> (
      match validate_client_id t with
      | Error e -> Error e
      | Ok () -> validate_client_secret t)
end

(** OAuth 2.0 Token Introspection verifier (RFC 7662).

    This verifier validates opaque tokens by calling an OAuth 2.0 token
    introspection endpoint. Unlike JWT verification which is stateless, token
    introspection requires a network call to the authorization server for each
    token validation.

    The verifier authenticates to the introspection endpoint using HTTP Basic
    Auth with the provided client_id and client_secret, as specified in RFC

    7662.

    Use this when:
    - Your authorization server issues opaque (non-JWT) tokens
    - You need to validate tokens from Auth0, Okta, Keycloak, or other OAuth
      servers
    - Your tokens require real-time revocation checking
    - Your authorization server supports RFC 7662 introspection *)
module Introspection_token_verifier = struct
  type t = {
    settings : Settings.t;
    introspection_url : string;
    client_id : string;
    client_secret : string;
    timeout_seconds : int;
    required_scopes : string list;
    base_url : string option;
  }
  [@@deriving sexp, compare]

  let create ?introspection_url ?client_id ?client_secret ?timeout_seconds
      ?required_scopes ?base_url () =
    let explicit_settings =
      Settings.create ?introspection_url ?client_id ?client_secret
        ?timeout_seconds ?required_scopes ?base_url ()
    in
    let env_settings = Settings.load_from_env () in
    let settings = Settings.merge explicit_settings env_settings in

    match Settings.validate settings with
    | Error e -> Error e
    | Ok () ->
      let introspection_url = Option.value_exn settings.introspection_url in
      let client_id = Option.value_exn settings.client_id in
      let client_secret = Option.value_exn settings.client_secret in
      let timeout_seconds = settings.timeout_seconds in
      let required_scopes = Option.value settings.required_scopes ~default:[] in
      let base_url = settings.base_url in

      Logging.Logger.debug logger
        (sprintf
           "Initialized Introspection Token Verifier for endpoint %s with \
            client %s"
           introspection_url client_id);

      Ok
        {
          settings;
          introspection_url;
          client_id;
          client_secret;
          timeout_seconds;
          required_scopes;
          base_url;
        }

  (** Get the introspection URL *)
  let introspection_url t = t.introspection_url

  (** Get the client ID *)
  let client_id t = t.client_id

  (** Get the client secret *)
  let client_secret t = t.client_secret

  (** Get the timeout in seconds *)
  let timeout_seconds t = t.timeout_seconds

  (** Get required scopes *)
  let required_scopes t = t.required_scopes

  (** Get base URL *)
  let base_url t = t.base_url

  (** Create HTTP Basic Auth header value from client credentials. *)
  let create_basic_auth_header t =
    let credentials = sprintf "%s:%s" t.client_id t.client_secret in
    let encoded = Base64.encode_exn credentials in
    sprintf "Basic %s" encoded

  (** Extract scopes from introspection response.

      RFC 7662 allows scopes to be returned as either:
      - A space-separated string in the 'scope' field
      - An array of strings in the 'scope' field (less common but valid) *)
  let extract_scopes json =
    match json with
    | `Assoc fields -> (
      match List.Assoc.find fields ~equal:String.equal "scope" with
      | None -> []
      | Some (`String s) ->
        String.split s ~on:' ' |> List.map ~f:String.strip
        |> List.filter ~f:(fun s -> not (String.is_empty s))
      | Some (`List arr) ->
        List.filter_map arr ~f:(function
          | `String s when not (String.is_empty s) -> Some s
          | _ -> None)
      | Some _ -> [])
    | _ -> []

  (** Check if required scopes are present *)
  let check_required_scopes ~required ~token_scopes =
    if List.is_empty required then true
    else
      let required_set = Set.of_list (module String) required in
      let token_set = Set.of_list (module String) token_scopes in
      Set.is_subset required_set ~of_:token_set
end
