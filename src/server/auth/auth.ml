(** Authentication Providers for OxFastMCP.

    This module provides authentication provider implementations including:
    - TokenVerifier: Base class for token verification
    - RemoteAuthProvider: RFC 9728 protected resource metadata
    - OAuthProvider: Full OAuth Authorization Server *)

open Core
open Cohttp
open Mcp_server_auth.Provider
open Settings

module type TOKEN_VERIFIER = TOKEN_VERIFIER
(** Token verifier module type (alias from Mcp_server_auth.Provider) *)

type route = {
  path : string;
  methods : string list;
  handler : Request.t -> (Response.t * Body.t) Lwt.t;
}
(** Route type for HTTP endpoints *)

(** Authentication provider base module type *)
module type AUTH_PROVIDER = sig
  val verify_token : string -> access_token option Lwt.t
  val base_url : string option
  val required_scopes : string list

  val get_routes : mcp_path:string option -> route list
  (** Get routes for this authentication provider.

      @param mcp_path The path where the MCP endpoint is mounted (e.g., "/mcp")
      @return List of routes for this provider *)

  val get_well_known_routes : mcp_path:string option -> route list
  (** Get well-known discovery routes (RFC 8414, RFC 9728).

      These should be mounted at root level of the application.

      @param mcp_path The path where the MCP endpoint is mounted
      @return List of well-known routes *)
end

(** Get resource URL by combining base_url and path *)
let get_resource_url ~base_url ~path =
  match path with
  | None -> base_url
  | Some p ->
    let prefix = String.rstrip base_url ~drop:(Char.equal '/') in
    let suffix = String.lstrip p ~drop:(Char.equal '/') in
    prefix ^ "/" ^ suffix

(** Create RFC 9728 protected resource metadata routes.

    Creates a well-known endpoint that advertises:
    - The protected resource URL
    - Authorization servers that issue valid tokens
    - Supported scopes

    @param resource_url The URL of the protected resource
    @param authorization_servers List of authorization server URLs
    @param scopes_supported Optional list of supported scopes
    @param resource_name Optional name for the resource
    @param resource_documentation Optional documentation URL *)
let create_protected_resource_routes ~resource_url ~authorization_servers
    ?scopes_supported ?resource_name ?resource_documentation () =
  (* RFC 9728: path-scoped well-known URL *)
  let uri = Uri.of_string resource_url in
  let resource_path = Uri.path uri in
  let well_known_path =
    "/.well-known/oauth-protected-resource" ^ resource_path
  in
  [
    {
      path = well_known_path;
      methods = [ "GET" ];
      handler =
        (fun _req ->
          (* Build response body per RFC 9728 *)
          let auth_servers_json =
            `List (List.map authorization_servers ~f:(fun s -> `String s))
          in
          let base_fields =
            [
              ("resource", `String resource_url);
              ("authorization_servers", auth_servers_json);
            ]
          in
          let optional_fields =
            List.filter_opt
              [
                Option.map scopes_supported ~f:(fun scopes ->
                    ( "scopes_supported",
                      `List (List.map scopes ~f:(fun s -> `String s)) ));
                Option.map resource_name ~f:(fun name ->
                    ("resource_name", `String name));
                Option.map resource_documentation ~f:(fun doc ->
                    ("resource_documentation", `String doc));
              ]
          in
          let body = `Assoc (base_fields @ optional_fields) in
          let body_string = Yojson.Safe.to_string body in
          let headers =
            Header.of_list
              [
                ("Content-Type", "application/json");
                ("Content-Length", string_of_int (String.length body_string));
                ("Cache-Control", "max-age=3600");
              ]
          in
          let response = Response.make ~status:`OK ~headers () in
          Lwt.return (response, Body.of_string body_string));
    };
  ]

(** Remote Auth Provider.

    Authentication provider for resource servers that verify tokens from known
    authorization servers. Creates RFC 9728 protected resource metadata
    endpoints. *)
module Remote_auth_provider (T : TOKEN_VERIFIER) : sig
  include AUTH_PROVIDER

  val create :
    base_url:string ->
    authorization_servers:string list ->
    ?required_scopes:string list ->
    ?resource_name:string ->
    ?resource_documentation:string ->
    unit ->
    (module AUTH_PROVIDER)
end = struct
  let verify_token = T.verify_token
  let base_url = None
  let required_scopes = []
  let get_routes ~mcp_path:_ = []
  let get_well_known_routes ~mcp_path:_ = []

  let create ~base_url:base_url_str ~authorization_servers
      ?(required_scopes = []) ?resource_name ?resource_documentation () =
    (module struct
      let verify_token = T.verify_token
      let base_url = Some base_url_str
      let required_scopes = required_scopes

      let get_routes ~mcp_path =
        (* Get the resource URL based on the MCP path *)
        let resource_url =
          get_resource_url ~base_url:base_url_str ~path:mcp_path
        in
        (* Create protected resource metadata routes *)
        create_protected_resource_routes ~resource_url ~authorization_servers
          ~scopes_supported:required_scopes ?resource_name
          ?resource_documentation ()

      let get_well_known_routes ~mcp_path =
        (* All routes from Remote_auth_provider are well-known routes *)
        get_routes ~mcp_path
    end : AUTH_PROVIDER)
end

(** OAuth provider base module *)
module Base_oauth_provider = struct
  type t = {
    issuer_url : string;
    service_documentation_url : string option;
    client_registration_options : client_registration_options option;
    revocation_options : revocation_options option;
    required_scopes : string list option;
  }

  let create ~issuer_url ?service_documentation_url ?client_registration_options
      ?revocation_options ?required_scopes () =
    {
      issuer_url;
      service_documentation_url;
      client_registration_options;
      revocation_options;
      required_scopes;
    }
end

(** OAuth provider module type *)
module type OAUTH_PROVIDER = sig
  include OAUTH_AUTHORIZATION_SERVER_PROVIDER
  include AUTH_PROVIDER

  val create :
    issuer_url:string ->
    ?service_documentation_url:string ->
    ?client_registration_options:client_registration_options ->
    ?revocation_options:revocation_options ->
    ?required_scopes:string list ->
    unit ->
    (module OAUTH_AUTHORIZATION_SERVER_PROVIDER)
end

(** OAuth provider functor *)
module Make_oauth_provider (P : OAUTH_AUTHORIZATION_SERVER_PROVIDER) = struct
  include P

  (* Stub AUTH_PROVIDER implementation *)
  let verify_token token = load_access_token token
  let base_url = None
  let required_scopes = []

  let get_routes ~mcp_path:_ =
    (* TODO: Implement create_auth_routes and
       create_protected_resource_routes *)
    []

  let get_well_known_routes ~mcp_path =
    (* Filter for well-known routes only *)
    List.filter (get_routes ~mcp_path) ~f:(fun route ->
        String.is_prefix route.path ~prefix:"/.well-known/")

  let create ~issuer_url ?service_documentation_url ?client_registration_options
      ?revocation_options ?required_scopes () =
    let _state =
      Base_oauth_provider.create ~issuer_url ?service_documentation_url
        ?client_registration_options ?revocation_options ?required_scopes ()
    in
    (module struct
      include P
    end : OAUTH_AUTHORIZATION_SERVER_PROVIDER)
end
