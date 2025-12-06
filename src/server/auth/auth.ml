(* open Core *)
(* open Lwt.Syntax *)
open Mcp_server_auth.Provider
open Settings

module type TOKEN_VERIFIER = TOKEN_VERIFIER
(** Token verifier module type (alias from Mcp_server_auth.Provider) *)

(** Authentication provider base module type *)
module type AUTH_PROVIDER = sig
  val verify_token : string -> access_token option Lwt.t
  val base_url : string option

  (* TODO: Define proper Route type. For now, using a placeholder list. *)
  (* The Python code returns `starlette.routing.Route` list. 
     In OCaml, this will likely be `Opium.App.builder` or similar depending on the web framework. 
     For now, we'll return a generic list to satisfy the interface translation structure. *)
  val get_routes : unit -> unit list
end

(** Remote Auth Provider *)
module Remote_auth_provider (T : TOKEN_VERIFIER) : sig
  include AUTH_PROVIDER

  val create :
    base_url:string ->
    authorization_servers:string list ->
    ?resource_name:string ->
    ?resource_documentation:string ->
    unit ->
    (module AUTH_PROVIDER)
end = struct
  (* type t = { base_url : string; authorization_servers : string list;
     resource_name : string option; resource_documentation : string option; } *)

  (* This functor requires instantiation to create a specific provider instance
     context if needed. However, the Python RemoteAuthProvider is a class. Here
     we'll use a functor to generate a module that matches AUTH_PROVIDER. *)

  let verify_token = T.verify_token
  let base_url = None (* Default for the functor itself, overridden by create *)
  let get_routes () = []

  let create ~base_url ~authorization_servers:_ ?resource_name:_
      ?resource_documentation:_ () =
    (module struct
      let verify_token = T.verify_token
      let base_url = Some base_url

      let get_routes () =
        (* TODO: Implement create_protected_resource_routes logic when routes.ml
           is available *)
        []
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
  (* This return type in original auth.mli was just
     OAUTH_AUTHORIZATION_SERVER_PROVIDER, checking auth.mli... *)
  (* In Python OAuthProvider IS an AuthProvider. *)
end

(** OAuth provider functor *)
module Make_oauth_provider (P : OAUTH_AUTHORIZATION_SERVER_PROVIDER) = struct
  include P

  (* Stub AUTH_PROVIDER implementation *)
  let verify_token token = load_access_token token

  (* We need base_url to be passed during creation or configuration. The
     original functor didn't take base_url. For now, we will default to None or
     assume it's handled via closure if generic. However, to fully satisfy
     AUTH_PROVIDER, we need to expose it. *)
  let base_url = None

  let get_routes () =
    (* TODO: Implement create_auth_routes and create_protected_resource_routes
       logic *)
    []

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
