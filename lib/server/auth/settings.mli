(** Client registration options *)
type client_registration_options = {
  enabled: bool;
  client_secret_expiry_seconds: int option;
  valid_scopes: string list option;
  default_scopes: string list option;
}

(** Revocation options *)
type revocation_options = {
  enabled: bool;
}

(** Auth settings *)
type auth_settings = {
  issuer_url: string;  (** OAuth authorization server URL that issues tokens for this resource server *)
  service_documentation_url: string option;
  client_registration_options: client_registration_options option;
  revocation_options: revocation_options option;
  required_scopes: string list option;
  resource_server_url: string option;  (** The URL of the MCP server to be used as the resource identifier and base route to look up OAuth Protected Resource Metadata *)
}

(** Create default client registration options *)
val default_client_registration_options : unit -> client_registration_options

(** Create default revocation options *)
val default_revocation_options : unit -> revocation_options 