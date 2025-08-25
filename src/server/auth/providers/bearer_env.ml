open Core
open Mcp_server_auth.Provider
(* open Bearer *)

module Bearer_settings = struct
  type t = {
    public_key : string option;
    jwks_uri : string option;
    issuer : string option;
    audience : string list option;
    required_scopes : string list option;
  }

  let env_prefix = "FASTMCP_AUTH_BEARER_"
  let get_env_opt name : string option = 
    try Some (Stdlib.Sys.getenv (env_prefix ^ name))
    with _ -> None

  let get_env_list_opt name =
    match get_env_opt name with
    | None -> None  
    | Some value -> Some (String.split value ~on:',' |> List.map ~f:String.strip)

  let load () =
    {
      public_key = get_env_opt "PUBLIC_KEY";
      jwks_uri = get_env_opt "JWKS_URI";
      issuer = get_env_opt "ISSUER";
      audience = get_env_list_opt "AUDIENCE";
      required_scopes = get_env_list_opt "REQUIRED_SCOPES";
    }
end

module Env_bearer_auth_provider = struct
  let create ?public_key ?jwks_uri ?issuer ?audience ?required_scopes () =
    let settings = Bearer_settings.load () in
    Bearer.Bearer_auth_provider.create
      ?public_key:(Option.first_some public_key settings.public_key)
      ?jwks_uri:(Option.first_some jwks_uri settings.jwks_uri)
      ?issuer:(Option.first_some issuer settings.issuer)
      ?audience:(Option.first_some audience settings.audience)
      ?required_scopes:
        (Option.first_some required_scopes settings.required_scopes)
      ()
  
  (* The interface is expecting these to be types, but they should come from the provider interface *)
  type authorization_code_t = authorization_code
  type refresh_token_t = refresh_token  
  type access_token_t = access_token
  
  (* Dummy implementations to satisfy interface - these should not be used directly *)
  let get_client _ = Lwt.return None
  let register_client _ = Lwt.return_unit
  let authorize _ _ = Lwt.fail (Failure "Use create() to get a proper provider")
  let load_authorization_code _ _ = Lwt.return None
  let exchange_authorization_code _ _ = Lwt.fail (Failure "Use create() to get a proper provider")
  let load_refresh_token _ _ = Lwt.return None
  let exchange_refresh_token _ _ _ = Lwt.fail (Failure "Use create() to get a proper provider")
  let load_access_token _ = Lwt.return None
  let revoke_token _ = Lwt.return_unit
end
