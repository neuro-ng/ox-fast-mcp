(** OAuth Proxy Provider for OxFastMCP.

    This provider acts as a transparent proxy to an upstream OAuth Authorization
    Server, handling Dynamic Client Registration locally while forwarding all
    other OAuth flows. *)

open Cohttp

(** Default token expiration times *)

val default_access_token_expiry_seconds : int
val default_auth_code_expiry_seconds : int
val http_timeout_seconds : int

(** OAuth transaction state for consent flow *)
type oauth_transaction = {
  txn_id : string;
  client_id : string;
  client_redirect_uri : string;
  client_state : string;
  code_challenge : string option;
  code_challenge_method : string;
  scopes : string list;
  created_at : float;
  resource : string option;
  proxy_code_verifier : string option;
  csrf_token : string option;
  csrf_expires_at : float option;
}
[@@deriving yojson, compare, sexp]

(** Client authorization code with PKCE and upstream tokens *)
type client_code = {
  code : string;
  cc_client_id : string; [@key "client_id"]
  redirect_uri : string;
  cc_code_challenge : string option; [@key "code_challenge"]
  cc_code_challenge_method : string; [@key "code_challenge_method"]
  cc_scopes : string list; [@key "scopes"]
  idp_tokens : Yojson.Safe.t;
  expires_at : float;
  cc_created_at : float; [@key "created_at"]
}
[@@deriving yojson, compare, sexp]

(** Stored upstream OAuth tokens from identity provider *)
type upstream_token_set = {
  upstream_token_id : string;
  access_token : string;
  refresh_token : string option;
  refresh_token_expires_at : float option;
  uts_expires_at : float; [@key "expires_at"]
  token_type : string;
  scope : string;
  uts_client_id : string; [@key "client_id"]
  uts_created_at : float; [@key "created_at"]
  raw_token_data : Yojson.Safe.t;
}
[@@deriving yojson, compare, sexp]

(** Maps OxFastMCP token JTI to upstream token ID *)
type jti_mapping = {
  jti : string;
  upstream_token_id : string;
  jm_created_at : float; [@key "created_at"]
}
[@@deriving yojson, compare, sexp]

(** Proxy DCR client with configurable redirect URI validation *)
type proxy_dcr_client = {
  pdc_client_id : string; [@key "client_id"]
  client_secret : string option;
  redirect_uris : string list;
  allowed_redirect_uri_patterns : string list option;
  client_name : string option;
  pdc_created_at : float; [@key "created_at"]
}
[@@deriving yojson, compare, sexp]

(** In-Memory Storage *)
module Storage : sig
  type 'a t

  val create : unit -> 'a t
  val get : 'a t -> key:string -> 'a option
  val put : 'a t -> key:string -> value:'a -> unit
  val remove : 'a t -> key:string -> unit
  val clear : 'a t -> unit
end

(** PKCE Utilities *)
module Pkce : sig
  val generate_pair : unit -> string * string
  (** Generate PKCE code verifier and challenge pair using S256 method *)

  val verify : verifier:string -> challenge:string -> method_:string -> bool
  (** Verify PKCE code verifier against challenge *)
end

(** Cookie Utilities *)
module Cookie : sig
  val sign : secret:string -> payload:string -> string
  (** Sign a cookie payload with HMAC-SHA256 *)

  val verify : secret:string -> signed_value:string -> string option
  (** Verify and extract payload from signed cookie *)
end

(** Token Verifier Interface *)
module type TOKEN_VERIFIER = sig
  val required_scopes : string list

  val verify_token :
    string -> Mcp_server_auth.Provider.access_token option Lwt.t
end

(** OAuth Proxy Configuration *)
type config = {
  upstream_authorization_endpoint : string;
  upstream_token_endpoint : string;
  upstream_client_id : string;
  upstream_client_secret : string;
  upstream_revocation_endpoint : string option;
  base_url : string;
  redirect_path : string;
  issuer_url : string option;
  service_documentation_url : string option;
  allowed_client_redirect_uris : string list option;
  valid_scopes : string list option;
  forward_pkce : bool;
  token_endpoint_auth_method : string option;
  extra_authorize_params : (string * string) list option;
  extra_token_params : (string * string) list option;
  jwt_signing_key : string option;
  require_authorization_consent : bool;
  consent_csp_policy : string option;
  required_scopes : string list;
}
[@@deriving compare, sexp]

(** OAuth Proxy State *)
type t

(** Create a new OAuth proxy provider *)
val create :
  upstream_authorization_endpoint:string ->
  upstream_token_endpoint:string ->
  upstream_client_id:string ->
  upstream_client_secret:string ->
  ?upstream_revocation_endpoint:string ->
  base_url:string ->
  ?redirect_path:string ->
  ?issuer_url:string ->
  ?service_documentation_url:string ->
  ?allowed_client_redirect_uris:string list ->
  ?valid_scopes:string list ->
  ?forward_pkce:bool ->
  ?token_endpoint_auth_method:string ->
  ?extra_authorize_params:(string * string) list ->
  ?extra_token_params:(string * string) list ->
  ?jwt_signing_key:string ->
  ?require_authorization_consent:bool ->
  ?consent_csp_policy:string ->
  required_scopes:string list ->
  unit ->
  t

(** Set the token verifier for the proxy *)
val set_token_verifier : t -> (module TOKEN_VERIFIER) -> unit

(** Get client information by ID *)
val get_client : t -> client_id:string -> proxy_dcr_client option Lwt.t

(** Register a client locally with DCR *)
val register_client :
  t ->
  client_id:string ->
  ?client_secret:string ->
  redirect_uris:string list ->
  ?client_name:string ->
  unit ->
  proxy_dcr_client Lwt.t

(** Validate redirect URI for a client *)
val validate_redirect_uri :
  t -> client:proxy_dcr_client -> redirect_uri:string -> bool

(** Start OAuth transaction - returns consent page URL *)
val authorize :
  t ->
  client_id:string ->
  redirect_uri:string ->
  state:string ->
  code_challenge:string option ->
  ?code_challenge_method:string ->
  scopes:string list ->
  ?resource:string ->
  unit ->
  string Lwt.t

(** Load authorization code for validation *)
val load_authorization_code : t -> code:string -> client_code option Lwt.t

(** Exchange authorization code for tokens *)
val exchange_authorization_code :
  t ->
  client_id:string ->
  code:string ->
  code_verifier:string ->
  (Yojson.Safe.t, string) result Lwt.t

(** Load refresh token from local storage *)
val load_refresh_token :
  t -> token:string -> upstream_token_set option Lwt.t

(** Exchange refresh token for new access token *)
val exchange_refresh_token :
  t ->
  client_id:string ->
  refresh_token:string ->
  scopes:string list ->
  (Yojson.Safe.t, string) result Lwt.t

(** Validate access token *)
val load_access_token :
  t -> token:string -> Mcp_server_auth.Provider.access_token option Lwt.t

(** Revoke token locally and with upstream server if supported *)
val revoke_token : t -> token:string -> unit Lwt.t

(** Build upstream authorization URL from transaction *)
val build_upstream_authorize_url :
  t -> txn_id:string -> transaction:oauth_transaction -> string

(** Create consent page HTML *)
val create_consent_html :
  client_id:string ->
  redirect_uri:string ->
  scopes:string list ->
  txn_id:string ->
  csrf_token:string ->
  ?client_name:string ->
  ?server_name:string ->
  unit ->
  string

(** Create error page HTML *)
val create_error_html :
  error_title:string ->
  error_message:string ->
  ?error_details:(string * string) list ->
  unit ->
  string

(** Handle consent page GET request *)
val handle_consent_get : t -> txn_id:string -> (Response.t * Body.t) Lwt.t

(** Handle consent page POST request *)
val handle_consent_post :
  t ->
  txn_id:string ->
  csrf_token:string ->
  action:string ->
  (Response.t * Body.t) Lwt.t

(** Handle IDP callback and forward to client *)
val handle_idp_callback :
  t -> code:string -> state:string -> (Response.t * Body.t) Lwt.t

(** Route handler type *)
type route = {
  path : string;
  methods : string list;
  handler : Request.t -> (Response.t * Body.t) Lwt.t;
}

(** Get OAuth routes for this proxy *)
val get_routes : t -> route list
