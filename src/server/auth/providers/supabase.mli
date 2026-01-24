(** Supabase authentication provider for OxFastMCP.

    This module provides SupabaseProvider - a complete authentication solution
    that integrates with Supabase Auth's JWT verification. *)

val supported_algorithms : (string, Core.String.comparator_witness) Core.Set.t
(** Supported JWT algorithms set. *)

(** Provider settings configuration. *)
module Settings : sig
  type t = {
    project_url : string option;
    base_url : string option;
    algorithm : string option;
    required_scopes : string list option;
  }
  [@@deriving sexp, yojson, compare]

  val create :
    ?project_url:string ->
    ?base_url:string ->
    ?algorithm:string ->
    ?required_scopes:string list ->
    unit ->
    t

  val load_from_env : unit -> t
  val merge : t -> t -> t
  val validate : t -> (unit, Core.Error.t) result
end

(** Supabase metadata provider for DCR (Dynamic Client Registration). *)
module Supabase_provider : sig
  type t [@@deriving sexp, compare]

  val create :
    ?project_url:string ->
    ?base_url:string ->
    ?algorithm:string ->
    ?required_scopes:string list ->
    unit ->
    (t, Core.Error.t) result
  (** Create a new Supabase provider.

      @param project_url
        Your Supabase project URL (e.g., "https://abc123.supabase.co")
      @param base_url Public URL of this OxFastMCP server
      @param algorithm
        JWT signing algorithm (HS256, RS256, or ES256). Defaults to ES256.
      @param required_scopes Optional list of scopes to require for all requests *)

  val project_url : t -> string
  val base_url : t -> string
  val algorithm : t -> string
  val required_scopes : t -> string list
  val jwks_uri : t -> string
  val issuer : t -> string
  val authorization_servers : t -> string list
  val authorization_server : t -> string
  val metadata_url : t -> string
end
