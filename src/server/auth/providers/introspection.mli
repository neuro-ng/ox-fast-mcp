(** OAuth 2.0 Token Introspection (RFC 7662) provider for OxFastMCP.

    This module provides token verification for opaque tokens using the OAuth
    2.0 Token Introspection protocol defined in RFC 7662. *)

(** Provider settings configuration. *)
module Settings : sig
  type t = {
    introspection_url : string option;
    client_id : string option;
    client_secret : string option;
    timeout_seconds : int;
    required_scopes : string list option;
    base_url : string option;
  }
  [@@deriving sexp, yojson, compare]

  val create :
    ?introspection_url:string ->
    ?client_id:string ->
    ?client_secret:string ->
    ?timeout_seconds:int ->
    ?required_scopes:string list ->
    ?base_url:string ->
    unit ->
    t

  val load_from_env : unit -> t
  val merge : t -> t -> t
  val validate : t -> (unit, Core.Error.t) result
end

(** OAuth 2.0 Token Introspection verifier (RFC 7662).

    This verifier validates opaque tokens by calling an OAuth 2.0 token
    introspection endpoint. The verifier authenticates using HTTP Basic Auth
    with the provided client_id and client_secret. *)
module Introspection_token_verifier : sig
  type t [@@deriving sexp, compare]

  val create :
    ?introspection_url:string ->
    ?client_id:string ->
    ?client_secret:string ->
    ?timeout_seconds:int ->
    ?required_scopes:string list ->
    ?base_url:string ->
    unit ->
    (t, Core.Error.t) result
  (** Create a new introspection token verifier.

      @param introspection_url URL of the OAuth 2.0 token introspection endpoint
      @param client_id
        OAuth client ID for authenticating to the introspection endpoint
      @param client_secret
        OAuth client secret for authenticating to the introspection endpoint
      @param timeout_seconds HTTP request timeout in seconds (default: 10)
      @param required_scopes Required scopes for all tokens (optional)
      @param base_url Base URL for TokenVerifier protocol *)

  val introspection_url : t -> string
  val client_id : t -> string
  val client_secret : t -> string
  val timeout_seconds : t -> int
  val required_scopes : t -> string list
  val base_url : t -> string option

  val create_basic_auth_header : t -> string
  (** Create HTTP Basic Auth header value from client credentials. *)

  val extract_scopes : Yojson.Safe.t -> string list
  (** Extract scopes from introspection response JSON. Handles both
      space-separated string and array formats per RFC 7662. *)

  val check_required_scopes :
    required:string list -> token_scopes:string list -> bool
  (** Check if required scopes are present in token scopes. *)
end
