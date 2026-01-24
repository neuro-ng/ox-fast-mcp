(** GitHub authentication provider for OxFastMCP.

    This module provides a complete GitHub OAuth integration that's ready to use
    with just a client ID and client secret. *)

(** GitHub OAuth endpoints *)

val github_authorization_endpoint : string
val github_token_endpoint : string
val github_user_api : string
val github_repos_api : string

(** Provider settings configuration. *)
module Settings : sig
  type t = {
    client_id : string option;
    client_secret : string option;
    base_url : string option;
    issuer_url : string option;
    redirect_path : string option;
    required_scopes : string list option;
    timeout_seconds : int option;
    allowed_client_redirect_uris : string list option;
    jwt_signing_key : string option;
  }
  [@@deriving sexp, yojson, compare]

  val create :
    ?client_id:string ->
    ?client_secret:string ->
    ?base_url:string ->
    ?issuer_url:string ->
    ?redirect_path:string ->
    ?required_scopes:string list ->
    ?timeout_seconds:int ->
    ?allowed_client_redirect_uris:string list ->
    ?jwt_signing_key:string ->
    unit ->
    t

  val load_from_env : unit -> t
  val merge : t -> t -> t
  val validate : t -> (unit, Core.Error.t) result
end

(** GitHub token verifier for opaque OAuth tokens.

    GitHub OAuth tokens are opaque (not JWTs), so we verify them by calling
    GitHub's API to check if they're valid and get user info. *)
module Github_token_verifier : sig
  type t [@@deriving sexp, compare]

  val create : ?required_scopes:string list -> ?timeout_seconds:int -> unit -> t
  val required_scopes : t -> string list
  val timeout_seconds : t -> int
  val user_agent : string
end

(** GitHub OAuth provider for OxFastMCP. *)
module Github_provider : sig
  type t [@@deriving sexp, compare]

  val create :
    ?client_id:string ->
    ?client_secret:string ->
    ?base_url:string ->
    ?issuer_url:string ->
    ?redirect_path:string ->
    ?required_scopes:string list ->
    ?timeout_seconds:int ->
    ?allowed_client_redirect_uris:string list ->
    ?jwt_signing_key:string ->
    ?_require_authorization_consent:bool ->
    unit ->
    (t, Core.Error.t) result
  (** Create a new GitHub provider.

      @param client_id GitHub OAuth app client ID
      @param client_secret GitHub OAuth app client secret
      @param base_url
        Public URL where OAuth endpoints will be accessible (includes any mount
        path)
      @param issuer_url Issuer URL for OAuth metadata (defaults to base_url)
      @param redirect_path
        Redirect path configured in GitHub OAuth app (defaults to
        "/auth/callback")
      @param required_scopes Required GitHub scopes (defaults to ["user"])
      @param timeout_seconds HTTP request timeout for GitHub API calls
      @param allowed_client_redirect_uris
        List of allowed redirect URI patterns for MCP clients
      @param jwt_signing_key Secret for signing OxFastMCP JWT tokens
      @param _require_authorization_consent
        Whether to require user consent before authorizing clients (placeholder) *)

  val client_id : t -> string
  val client_secret : t -> string
  val base_url : t -> string option
  val issuer_url : t -> string option
  val redirect_path : t -> string
  val required_scopes : t -> string list
  val timeout_seconds : t -> int
  val allowed_client_redirect_uris : t -> string list option
  val jwt_signing_key : t -> string option
  val token_verifier : t -> Github_token_verifier.t
  val authorization_endpoint : unit -> string
  val token_endpoint : unit -> string
end
