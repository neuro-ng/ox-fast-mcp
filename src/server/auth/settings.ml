type client_registration_options = {
  enabled : bool;
  client_secret_expiry_seconds : int option;
  valid_scopes : string list option;
  default_scopes : string list option;
}
(** Client registration options *)

type revocation_options = { enabled : bool }
(** Revocation options *)

type auth_settings = {
  issuer_url : string;
      (** OAuth authorization server URL that issues tokens for this resource
          server *)
  service_documentation_url : string option;
  client_registration_options : client_registration_options option;
  revocation_options : revocation_options option;
  required_scopes : string list option;
  resource_server_url : string option;
      (** The URL of the MCP server to be used as the resource identifier and
          base route to look up OAuth Protected Resource Metadata *)
}
(** Auth settings *)

(** Create default client registration options *)
let default_client_registration_options () =
  {
    enabled = false;
    client_secret_expiry_seconds = None;
    valid_scopes = None;
    default_scopes = None;
  }

(** Create default revocation options *)
let default_revocation_options () = { enabled = false }
