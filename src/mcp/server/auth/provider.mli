type authorization_params = {
  state : string option;
  scopes : string list option;
  code_challenge : string;
  redirect_uri : string;
  redirect_uri_provided_explicitly : bool;
  resource : string option;  (** RFC 8707 resource indicator *)
}
(** Authorization parameters *)

type authorization_code = {
  code : string;
  scopes : string list;
  expires_at : float;
  client_id : string;
  code_challenge : string;
  redirect_uri : string;
  redirect_uri_provided_explicitly : bool;
  resource : string option;  (** RFC 8707 resource indicator *)
}
(** Authorization code *)

type refresh_token = {
  token : string;
  client_id : string;
  scopes : string list;
  expires_at : int option;
}
(** Refresh token *)

type access_token = {
  token : string;
  client_id : string;
  scopes : string list;
  expires_at : int option;
  resource : string option;  (** RFC 8707 resource indicator *)
}
(** Access token *)

type registration_error_code =
  [ `Invalid_redirect_uri
  | `Invalid_client_metadata
  | `Invalid_software_statement
  | `Unapproved_software_statement ]
(** Registration error codes *)

exception
  Registration_error of {
    error : registration_error_code;
    error_description : string option;
  }
(** Registration error *)

type authorization_error_code =
  [ `Invalid_request
  | `Unauthorized_client
  | `Access_denied
  | `Unsupported_response_type
  | `Invalid_scope
  | `Server_error
  | `Temporarily_unavailable ]
(** Authorization error codes *)

exception
  Authorization_error of {
    error : authorization_error_code;
    error_description : string option;
  }
(** Authorization error *)

type token_error_code =
  [ `Invalid_request
  | `Invalid_client
  | `Invalid_grant
  | `Unauthorized_client
  | `Unsupported_grant_type
  | `Invalid_scope ]
(** Token error codes *)

exception
  Token_error of { error : token_error_code; error_description : string option }
(** Token error *)

type oauth_client_information = {
  client_id : string;
  client_secret : string option;
  redirect_uris : string list;
  scope : string option;
}
(** OAuth client information *)

type oauth_token = {
  access_token : string;
  token_type : string;
  expires_in : int;
  refresh_token : string option;
  scope : string option;
}
(** OAuth token *)

(** Token verifier module type *)
module type TOKEN_VERIFIER = sig
  val verify_token : string -> access_token option Lwt.t
end

(** OAuth authorization server provider module type *)
module type OAUTH_AUTHORIZATION_SERVER_PROVIDER = sig
  type authorization_code_t = authorization_code
  type refresh_token_t = refresh_token
  type access_token_t = access_token

  val get_client :
    string -> Mcp_shared.Auth.oauth_client_information_full option Lwt.t
  (** Get client information by client ID *)

  val register_client :
    Mcp_shared.Auth.oauth_client_information_full -> unit Lwt.t
  (** Register a new client
      @raise Registration_error if client metadata is invalid *)

  val authorize :
    Mcp_shared.Auth.oauth_client_information_full ->
    authorization_params ->
    string Lwt.t
  (** Authorize a client and return redirect URL
      @raise Authorization_error if request is invalid *)

  val load_authorization_code :
    Mcp_shared.Auth.oauth_client_information_full ->
    string ->
    authorization_code_t option Lwt.t
  (** Load authorization code *)

  val exchange_authorization_code :
    Mcp_shared.Auth.oauth_client_information_full ->
    authorization_code_t ->
    Mcp_shared.Auth.oauth_token Lwt.t
  (** Exchange authorization code for tokens
      @raise Token_error if request is invalid *)

  val load_refresh_token :
    Mcp_shared.Auth.oauth_client_information_full ->
    string ->
    refresh_token_t option Lwt.t
  (** Load refresh token *)

  val exchange_refresh_token :
    Mcp_shared.Auth.oauth_client_information_full ->
    refresh_token_t ->
    string list ->
    Mcp_shared.Auth.oauth_token Lwt.t
  (** Exchange refresh token for new tokens
      @raise Token_error if request is invalid *)

  val load_access_token : string -> access_token_t option Lwt.t
  (** Load access token *)

  val revoke_token :
    [< `Access of access_token_t | `Refresh of refresh_token_t ] -> unit Lwt.t
  (** Revoke a token *)
end

val construct_redirect_uri : string -> (string * string option) list -> string
(** Helper function to construct redirect URI *)

(** Provider token verifier implementation *)
module Provider_token_verifier (P : OAUTH_AUTHORIZATION_SERVER_PROVIDER) :
  TOKEN_VERIFIER
