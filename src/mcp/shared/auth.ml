open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type oauth_token = {
  access_token : string;
  token_type : string;
  expires_in : int option; [@yojson.option]
  scope : string option; [@yojson.option]
  refresh_token : string option; [@yojson.option]
}
[@@deriving yojson]
(** OAuth token as defined in RFC 6749 Section 5.1 *)

exception Invalid_scope_error of string
(** Invalid scope error *)

exception Invalid_redirect_uri_error of string
(** Invalid redirect URI error *)

type oauth_client_metadata = {
  redirect_uris : string list;
  token_endpoint_auth_method : [ `None | `Client_secret_post ];
  grant_types : [ `Authorization_code | `Refresh_token ] list;
  response_types : [ `Code ] list;
  scope : string option; [@yojson.option]
  client_name : string option; [@yojson.option]
  client_uri : string option; [@yojson.option]
  logo_uri : string option; [@yojson.option]
  contacts : string list option; [@yojson.option]
  tos_uri : string option; [@yojson.option]
  policy_uri : string option; [@yojson.option]
  jwks_uri : string option; [@yojson.option]
  jwks : Mcp.Types.json option; [@yojson.option]
  software_id : string option; [@yojson.option]
  software_version : string option; [@yojson.option]
}
[@@deriving yojson]
(** OAuth client metadata as defined in RFC 7591 Section 2 *)

type oauth_client_information = {
  client_id : string;
  client_secret : string option; [@yojson.option]
  client_id_issued_at : int option; [@yojson.option]
  client_secret_expires_at : int option; [@yojson.option]
}
[@@deriving fields, yojson]
(** OAuth client information (metadata plus client credentials) *)

type oauth_client_information_full = {
  metadata : oauth_client_metadata;
  info : oauth_client_information;
}
[@@deriving yojson]
(** Full OAuth client information combining metadata and credentials *)

type oauth_metadata = {
  issuer : string;
  authorization_endpoint : string;
  token_endpoint : string;
  registration_endpoint : string option; [@yojson.option]
  scopes_supported : string list option; [@yojson.option]
  response_types_supported : string list;
  response_modes_supported : [ `Query | `Fragment ] list option;
      [@yojson.option]
  grant_types_supported : string list option; [@yojson.option]
  token_endpoint_auth_methods_supported : string list option; [@yojson.option]
  token_endpoint_auth_signing_alg_values_supported : unit option;
      [@yojson.option]
  service_documentation : string option; [@yojson.option]
  ui_locales_supported : string list option; [@yojson.option]
  op_policy_uri : string option; [@yojson.option]
  op_tos_uri : string option; [@yojson.option]
  revocation_endpoint : string option; [@yojson.option]
  revocation_endpoint_auth_methods_supported : string list option;
      [@yojson.option]
  revocation_endpoint_auth_signing_alg_values_supported : unit option;
      [@yojson.option]
  introspection_endpoint : string option; [@yojson.option]
  introspection_endpoint_auth_methods_supported : string list option;
      [@yojson.option]
  introspection_endpoint_auth_signing_alg_values_supported : unit option;
      [@yojson.option]
  code_challenge_methods_supported : string list option; [@yojson.option]
  client_id_metadata_document_supported : bool option; [@yojson.option]
}
[@@deriving yojson]
(** OAuth authorization server metadata as defined in RFC 8414 Section 2 *)

type protected_resource_metadata = {
  resource : string;
  authorization_servers : string list;
  scopes_supported : string list option; [@yojson.option]
  bearer_methods_supported : string list;
  resource_documentation : string option; [@yojson.option]
}
[@@deriving yojson]
(** Protected resource metadata as defined in RFC 9728 Section 2 *)

let validate_scope metadata requested_scope =
  match requested_scope with
  | None -> None
  | Some scope ->
    let requested_scopes = String.split ~on:' ' scope in
    let allowed_scopes =
      match metadata.scope with
      | None -> []
      | Some s -> String.split ~on:' ' s
    in
    let allowed_scopes_set = String.Set.of_list allowed_scopes in
    List.iter requested_scopes ~f:(fun scope ->
        if not (Set.mem allowed_scopes_set scope) then
          raise
            (Invalid_scope_error
               (Printf.sprintf "Client was not registered with scope %s" scope)));
    Some requested_scopes

let validate_redirect_uri metadata redirect_uri =
  match redirect_uri with
  | Some uri ->
    if not (List.mem metadata.redirect_uris uri ~equal:String.equal) then
      raise
        (Invalid_redirect_uri_error
           (Printf.sprintf "Redirect URI '%s' not registered for client" uri));
    uri
  | None -> (
    match metadata.redirect_uris with
    | [ uri ] -> uri
    | _ ->
      raise
        (Invalid_redirect_uri_error
           "redirect_uri must be specified when client has multiple registered \
            URIs"))
