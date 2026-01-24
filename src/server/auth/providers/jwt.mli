(** JWT TokenVerifier implementations for OxFastMCP.

    This module provides JWT token verification supporting both asymmetric
    (RSA/ECDSA) and symmetric (HMAC) algorithms, as well as JWKS endpoint
    support for automatic key rotation. *)

val supported_algorithms : (string, Core.String.comparator_witness) Core.Set.t
(** Supported JWT algorithms set. *)

(** JSON Web Key data structure. *)
module Jwk_data : sig
  type t = {
    kty : string;
    kid : string option;
    use : string option;
    alg : string option;
    n : string option;
    e : string option;
    x5c : string list option;
    x5t : string option;
  }
  [@@deriving sexp, yojson, compare]
end

(** JSON Web Key Set data structure. *)
module Jwks_data : sig
  type t = { keys : Jwk_data.t list } [@@deriving sexp, yojson, compare]
end

(** JWT verifier settings configuration. *)
module Settings : sig
  type t = {
    public_key : string option;
    jwks_uri : string option;
    issuer : string option;
    issuer_list : string list option;
    algorithm : string option;
    audience : string option;
    audience_list : string list option;
    required_scopes : string list option;
    base_url : string option;
  }
  [@@deriving sexp, yojson, compare]

  val create :
    ?public_key:string ->
    ?jwks_uri:string ->
    ?issuer:string ->
    ?issuer_list:string list ->
    ?algorithm:string ->
    ?audience:string ->
    ?audience_list:string list ->
    ?required_scopes:string list ->
    ?base_url:string ->
    unit ->
    t

  val load_from_env : unit -> t
  val merge : t -> t -> t
  val validate : t -> (unit, Core.Error.t) result
end

(** JWT token verifier supporting both asymmetric and symmetric algorithms. *)
module Jwt_verifier : sig
  type t [@@deriving sexp, compare]

  val create :
    ?public_key:string ->
    ?jwks_uri:string ->
    ?issuer:string ->
    ?issuer_list:string list ->
    ?audience:string ->
    ?audience_list:string list ->
    ?algorithm:string ->
    ?required_scopes:string list ->
    ?base_url:string ->
    unit ->
    (t, Core.Error.t) result

  val public_key : t -> string option
  val jwks_uri : t -> string option
  val issuer : t -> string list option
  val audience : t -> string list option
  val algorithm : t -> string
  val required_scopes : t -> string list
  val base_url : t -> string option
  val cache_ttl : t -> int

  val extract_scopes : Yojson.Safe.t -> string list
  (** Extract scopes from JWT claims JSON. Supports 'scope' and 'scp' claims. *)

  val check_required_scopes :
    required:string list -> token_scopes:string list -> bool
  (** Check if all required scopes are present in token scopes. *)

  val extract_client_id : Yojson.Safe.t -> string
  (** Extract client ID from claims. Tries client_id, azp, sub in order. *)

  val validate_issuer : expected:string list option -> actual:string -> bool
  (** Validate issuer against expected list. *)

  val validate_audience :
    expected:string list option -> actual:Yojson.Safe.t -> bool
  (** Validate audience against expected list. *)
end

(** Simple static token verifier for testing and development.

    WARNING: Never use this in production - tokens are stored in plain text! *)
module Static_token_verifier : sig
  type token_data = {
    client_id : string;
    scopes : string list;
    expires_at : int option;
    claims_json : string;
  }
  [@@deriving sexp, compare]

  type t

  val create :
    tokens:(string * token_data) list ->
    ?required_scopes:string list ->
    unit ->
    t

  val required_scopes : t -> string list
  val get_token_data : t -> string -> token_data option
  val is_expired : token_data -> bool
  val check_scopes : t -> token_data -> bool

  val make_token_data :
    client_id:string ->
    ?scopes:string list ->
    ?expires_at:int ->
    ?claims_json:string ->
    unit ->
    token_data
end
