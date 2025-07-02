open Core
open Lwt.Syntax
open Server.Auth.Provider

(** Default expiration times (in seconds) *)
let default_auth_code_expiry_seconds = 5 * 60  (* 5 minutes *)
let default_access_token_expiry_seconds = 60 * 60  (* 1 hour *)
let default_refresh_token_expiry_seconds = None  (* No expiry *)

(** In-memory OAuth provider *)
module In_memory_provider : sig
  include OAUTH_AUTHORIZATION_SERVER_PROVIDER

  val create : 
    ?issuer_url:string ->
    ?service_documentation_url:string ->
    ?required_scopes:string list ->
    unit ->
    (module OAUTH_AUTHORIZATION_SERVER_PROVIDER)
end = struct
  type t = {
    issuer_url: string;
    service_documentation_url: string option;
    required_scopes: string list;
    clients: (string, oauth_client_information) Hashtbl.t;
    auth_codes: (string, authorization_code) Hashtbl.t;
    access_tokens: (string, access_token) Hashtbl.t;
    refresh_tokens: (string, refresh_token) Hashtbl.t;
    access_to_refresh_map: (string, string) Hashtbl.t;
    refresh_to_access_map: (string, string) Hashtbl.t;
  }

  let state = ref None

  let get_state () =
    match !state with
    | Some s -> s
    | None -> failwith "In_memory_provider not initialized"

  let create ?(issuer_url="http://fastmcp.example.com") ?service_documentation_url ?(required_scopes=[]) () =
    let t = {
      issuer_url;
      service_documentation_url;
      required_scopes;
      clients = Hashtbl.create (module String);
      auth_codes = Hashtbl.create (module String);
      access_tokens = Hashtbl.create (module String);
      refresh_tokens = Hashtbl.create (module String);
      access_to_refresh_map = Hashtbl.create (module String);
      refresh_to_access_map = Hashtbl.create (module String);
    } in
    state := Some t;
    (module struct
      let get_client client_id =
        let+ () = Lwt.return () in
        Hashtbl.find (get_state()).clients client_id

      let register_client client_info =
        let+ () = Lwt.return () in
        Hashtbl.set (get_state()).clients ~key:client_info.client_id ~data:client_info

      let authorize client params =
        let state = get_state () in
        if not (Hashtbl.mem state.clients client.client_id) then
          Lwt.fail (Authorization_error {
            error=`UnauthorizedClient;
            error_description=Some (Printf.sprintf "Client '%s' not registered." client.client_id);
          })
        else
          let auth_code_value = Printf.sprintf "test_auth_code_%s" (Random.bits () |> Int.to_string) in
          let expires_at = Unix.time () +. float_of_int default_auth_code_expiry_seconds in
          let scopes_list = match params.scopes with
            | None -> []
            | Some s -> s
          in
          let scopes_list = match client.scope with
            | None -> scopes_list
            | Some scope ->
              let client_scopes = String.split ~on:' ' scope |> String.Set.of_list in
              List.filter scopes_list ~f:(Set.mem client_scopes)
          in
          let auth_code = {
            code = auth_code_value;
            client_id = client.client_id;
            redirect_uri = params.redirect_uri;
            redirect_uri_provided_explicitly = params.redirect_uri_provided_explicitly;
            scopes = scopes_list;
            expires_at;
            code_challenge = params.code_challenge;
            resource = params.resource;
          } in
          Hashtbl.set state.auth_codes ~key:auth_code_value ~data:auth_code;
          let+ () = Lwt.return () in
          construct_redirect_uri params.redirect_uri [
            ("code", Some auth_code_value);
            ("state", params.state);
          ]

      let load_authorization_code client code =
        let state = get_state () in
        let+ () = Lwt.return () in
        match Hashtbl.find state.auth_codes code with
        | None -> None
        | Some auth_code ->
          if auth_code.client_id <> client.client_id then None
          else if auth_code.expires_at < Unix.time () then (
            Hashtbl.remove state.auth_codes code;
            None
          ) else Some auth_code

      let exchange_authorization_code client auth_code =
        let state = get_state () in
        if not (Hashtbl.mem state.auth_codes auth_code.code) then
          Lwt.fail (Token_error {
            error=`InvalidGrant;
            error_description=Some "Authorization code not found or already used.";
          })
        else (
          Hashtbl.remove state.auth_codes auth_code.code;
          let access_token_value = Printf.sprintf "test_access_token_%s" (Random.bits () |> Int.to_string) in
          let refresh_token_value = Printf.sprintf "test_refresh_token_%s" (Random.bits () |> Int.to_string) in
          let access_token_expires_at = Some (int_of_float (Unix.time ()) + default_access_token_expiry_seconds) in
          let refresh_token_expires_at = Option.map default_refresh_token_expiry_seconds ~f:(fun x ->
            int_of_float (Unix.time ()) + x
          ) in
          let access_token = {
            token = access_token_value;
            client_id = client.client_id;
            scopes = auth_code.scopes;
            expires_at = access_token_expires_at;
            resource = auth_code.resource;
          } in
          let refresh_token = {
            token = refresh_token_value;
            client_id = client.client_id;
            scopes = auth_code.scopes;
            expires_at = refresh_token_expires_at;
          } in
          Hashtbl.set state.access_tokens ~key:access_token_value ~data:access_token;
          Hashtbl.set state.refresh_tokens ~key:refresh_token_value ~data:refresh_token;
          Hashtbl.set state.access_to_refresh_map ~key:access_token_value ~data:refresh_token_value;
          Hashtbl.set state.refresh_to_access_map ~key:refresh_token_value ~data:access_token_value;
          let+ () = Lwt.return () in
          {
            access_token = access_token_value;
            token_type = "Bearer";
            expires_in = default_access_token_expiry_seconds;
            refresh_token = Some refresh_token_value;
            scope = Some (String.concat ~sep:" " auth_code.scopes);
          }
        )

      let load_refresh_token client refresh_token =
        let state = get_state () in
        let+ () = Lwt.return () in
        match Hashtbl.find state.refresh_tokens refresh_token with
        | None -> None
        | Some token ->
          if token.client_id <> client.client_id then None
          else match token.expires_at with
            | Some exp when exp < int_of_float (Unix.time ()) ->
              Hashtbl.remove state.refresh_tokens refresh_token;
              None
            | _ -> Some token

      let exchange_refresh_token client refresh_token scopes =
        let state = get_state () in
        let original_scopes = Set.of_list (module String) refresh_token.scopes in
        let requested_scopes = Set.of_list (module String) scopes in
        if not (Set.is_subset requested_scopes ~of_:original_scopes) then
          Lwt.fail (Token_error {
            error=`InvalidScope;
            error_description=Some "Requested scopes exceed those authorized by the refresh token.";
          })
        else (
          (* Revoke old tokens *)
          Hashtbl.remove state.refresh_tokens refresh_token.token;
          (match Hashtbl.find state.refresh_to_access_map refresh_token.token with
          | Some old_access -> Hashtbl.remove state.access_tokens old_access
          | None -> ());
          
          (* Issue new tokens *)
          let new_access_token_value = Printf.sprintf "test_access_token_%s" (Random.bits () |> Int.to_string) in
          let new_refresh_token_value = Printf.sprintf "test_refresh_token_%s" (Random.bits () |> Int.to_string) in
          let access_token_expires_at = Some (int_of_float (Unix.time ()) + default_access_token_expiry_seconds) in
          let refresh_token_expires_at = Option.map default_refresh_token_expiry_seconds ~f:(fun x ->
            int_of_float (Unix.time ()) + x
          ) in
          let access_token = {
            token = new_access_token_value;
            client_id = client.client_id;
            scopes;
            expires_at = access_token_expires_at;
            resource = None;
          } in
          let refresh_token = {
            token = new_refresh_token_value;
            client_id = client.client_id;
            scopes;
            expires_at = refresh_token_expires_at;
          } in
          Hashtbl.set state.access_tokens ~key:new_access_token_value ~data:access_token;
          Hashtbl.set state.refresh_tokens ~key:new_refresh_token_value ~data:refresh_token;
          Hashtbl.set state.access_to_refresh_map ~key:new_access_token_value ~data:new_refresh_token_value;
          Hashtbl.set state.refresh_to_access_map ~key:new_refresh_token_value ~data:new_access_token_value;
          let+ () = Lwt.return () in
          {
            access_token = new_access_token_value;
            token_type = "Bearer";
            expires_in = default_access_token_expiry_seconds;
            refresh_token = Some new_refresh_token_value;
            scope = Some (String.concat ~sep:" " scopes);
          }
        )

      let load_access_token token =
        let state = get_state () in
        let+ () = Lwt.return () in
        match Hashtbl.find state.access_tokens token with
        | None -> None
        | Some token_obj ->
          match token_obj.expires_at with
          | Some exp when exp < int_of_float (Unix.time ()) ->
            Hashtbl.remove state.access_tokens token;
            None
          | _ -> Some token_obj

      let revoke_token = function
        | `Access token ->
          let state = get_state () in
          Hashtbl.remove state.access_tokens token.token;
          (match Hashtbl.find state.access_to_refresh_map token.token with
          | Some refresh -> Hashtbl.remove state.refresh_tokens refresh
          | None -> ());
          Lwt.return_unit
        | `Refresh token ->
          let state = get_state () in
          Hashtbl.remove state.refresh_tokens token.token;
          (match Hashtbl.find state.refresh_to_access_map token.token with
          | Some access -> Hashtbl.remove state.access_tokens access
          | None -> ());
          Lwt.return_unit
    end : OAUTH_AUTHORIZATION_SERVER_PROVIDER)
end 