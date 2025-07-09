open Core
open Async
open Cohttp
open Lwt.Syntax
open Mcp.Server.Auth.Provider

type authenticated_user = {
  client_id : string;
  access_token : access_token;
  scopes : string list;
}

type auth_credentials = { scopes : string list }
type auth_result = (auth_credentials * authenticated_user) option
type bearer_auth_config = { token_verifier : (module TOKEN_VERIFIER) }

type require_auth_config = {
  required_scopes : string list;
  resource_metadata_url : string option;
}

module Bearer_auth_backend = struct
  let create config = config

  let authenticate config req =
    let open Request in
    let auth_header = Header.get (headers req) "authorization" in
    match auth_header with
    | None -> Lwt.return None
    | Some h when not (String.is_prefix ~prefix:"Bearer " (String.lowercase h))
      -> Lwt.return None
    | Some h -> (
      let token = String.drop_prefix h 7 in
      (* Remove "Bearer " prefix *)
      let module V = (val config.token_verifier : TOKEN_VERIFIER) in
      let* auth_info = V.verify_token token in
      match auth_info with
      | None -> Lwt.return None
      | Some info -> (
        match info.expires_at with
        | Some exp when exp < int_of_float (Unix.time ()) -> Lwt.return None
        | _ ->
          let auth_user =
            {
              client_id = info.client_id;
              access_token = info;
              scopes = info.scopes;
            }
          in
          let auth_creds = { scopes = info.scopes } in
          Lwt.return (Some (auth_creds, auth_user))))
end

module Require_auth_middleware = struct
  let create config = config

  let send_auth_error ~status ~error ~description ~resource_metadata_url =
    let www_auth_parts =
      [
        Printf.sprintf "error=\"%s\"" error;
        Printf.sprintf "error_description=\"%s\"" description;
      ]
    in
    let www_auth_parts =
      match resource_metadata_url with
      | None -> www_auth_parts
      | Some url ->
        www_auth_parts @ [ Printf.sprintf "resource_metadata=\"%s\"" url ]
    in
    let www_authenticate =
      Printf.sprintf "Bearer %s" (String.concat ~sep:", " www_auth_parts)
    in
    let body =
      `Assoc
        [ ("error", `String error); ("error_description", `String description) ]
    in
    let body_string = Yojson.Safe.to_string body in
    let headers =
      Header.init_with
        [
          ("Content-Type", "application/json");
          ("Content-Length", string_of_int (String.length body_string));
          ("WWW-Authenticate", www_authenticate);
        ]
    in
    let response = Response.make ~status ~headers () in
    Lwt.return (response, Body.of_string body_string)

  let handle config auth_result _req next =
    match auth_result with
    | None ->
      send_auth_error ~status:`Unauthorized ~error:"invalid_token"
        ~description:"Authentication required"
        ~resource_metadata_url:config.resource_metadata_url
    | Some (auth_creds, _) -> (
      let missing_scope =
        List.find config.required_scopes ~f:(fun required_scope ->
            not (List.mem auth_creds.scopes required_scope ~equal:String.equal))
      in
      match missing_scope with
      | Some scope ->
        send_auth_error ~status:`Forbidden ~error:"insufficient_scope"
          ~description:(Printf.sprintf "Required scope: %s" scope)
          ~resource_metadata_url:config.resource_metadata_url
      | None -> next)
end
