open Core
open Shared.Auth.Provider
open Bearer

module Bearer_settings = struct
  type t = {
    public_key : string option;
    jwks_uri : string option;
    issuer : string option;
    audience : string list option;
    required_scopes : string list option;
  }

  let env_prefix = "FASTMCP_AUTH_BEARER_"

  let get_env_opt name =
    Sys.getenv_opt (env_prefix ^ name)

  let get_env_list_opt name =
    get_env_opt name
    |> Option.map ~f:(String.split ~on:',')
    |> Option.map ~f:(List.map ~f:String.strip)

  let load () = {
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
    Bearer_auth_provider.create
      ?public_key:(Option.first_some public_key settings.public_key)
      ?jwks_uri:(Option.first_some jwks_uri settings.jwks_uri)
      ?issuer:(Option.first_some issuer settings.issuer)
      ?audience:(Option.first_some audience settings.audience)
      ?required_scopes:(Option.first_some required_scopes settings.required_scopes)
      ()
end 