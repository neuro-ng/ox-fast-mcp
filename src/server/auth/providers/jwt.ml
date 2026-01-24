(** JWT TokenVerifier implementations for OxFastMCP.

    This module provides JWT token verification supporting both asymmetric
    (RSA/ECDSA) and symmetric (HMAC) algorithms, as well as JWKS endpoint
    support for automatic key rotation.

    Example:
    {[
      let verifier =
        Jwt.Jwt_verifier.create ~public_key:pem_key ~issuer:"https://auth.example.com"
          ~required_scopes:[ "read"; "write" ] ()
      in
      (* Use verifier with your OxFastMCP server *)
    ]} *)

open Core
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

let logger = Logging.Logger.get_logger "ox-fast-mcp.server.auth.providers.jwt"

(** Supported JWT algorithms *)
let supported_algorithms =
  Set.of_list (module String)
    [
      "HS256";
      "HS384";
      "HS512";
      "RS256";
      "RS384";
      "RS512";
      "ES256";
      "ES384";
      "ES512";
      "PS256";
      "PS384";
      "PS512";
    ]

(** JSON Web Key data structure. *)
module Jwk_data = struct
  type t = {
    kty : string; (* Key type, e.g., "RSA" - required *)
    kid : string option; [@yojson.option] (* Key ID *)
    use : string option; [@yojson.option] (* Usage, e.g., "sig" *)
    alg : string option; [@yojson.option] (* Algorithm, e.g., "RS256" *)
    n : string option; [@yojson.option] (* Modulus for RSA keys *)
    e : string option; [@yojson.option] (* Exponent for RSA keys *)
    x5c : string list option; [@yojson.option] (* X.509 certificate chain *)
    x5t : string option; [@yojson.option] (* X.509 certificate thumbprint *)
  }
  [@@deriving sexp, yojson, compare]
end

(** JSON Web Key Set data structure. *)
module Jwks_data = struct
  type t = { keys : Jwk_data.t list } [@@deriving sexp, yojson, compare]
end

module Settings = struct
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

  let create ?public_key ?jwks_uri ?issuer ?issuer_list ?algorithm ?audience
      ?audience_list ?required_scopes ?base_url () =
    {
      public_key;
      jwks_uri;
      issuer;
      issuer_list;
      algorithm;
      audience;
      audience_list;
      required_scopes;
      base_url;
    }

  (** Parse comma-separated scopes string into list *)
  let parse_scopes = function
    | None -> None
    | Some s ->
      Some
        (String.split s ~on:',' |> List.map ~f:String.strip
        |> List.filter ~f:(fun s -> not (String.is_empty s)))

  let load_from_env () =
    let get key = Sys.getenv key in
    let public_key = get "OXFASTMCP_SERVER_AUTH_JWT_PUBLIC_KEY" in
    let jwks_uri = get "OXFASTMCP_SERVER_AUTH_JWT_JWKS_URI" in
    let issuer = get "OXFASTMCP_SERVER_AUTH_JWT_ISSUER" in
    let issuer_list = parse_scopes (get "OXFASTMCP_SERVER_AUTH_JWT_ISSUERS") in
    let algorithm = get "OXFASTMCP_SERVER_AUTH_JWT_ALGORITHM" in
    let audience = get "OXFASTMCP_SERVER_AUTH_JWT_AUDIENCE" in
    let audience_list =
      parse_scopes (get "OXFASTMCP_SERVER_AUTH_JWT_AUDIENCES")
    in
    let required_scopes =
      parse_scopes (get "OXFASTMCP_SERVER_AUTH_JWT_REQUIRED_SCOPES")
    in
    let base_url = get "OXFASTMCP_SERVER_AUTH_JWT_BASE_URL" in
    {
      public_key;
      jwks_uri;
      issuer;
      issuer_list;
      algorithm;
      audience;
      audience_list;
      required_scopes;
      base_url;
    }

  let merge (t1 : t) (t2 : t) =
    {
      public_key = Option.first_some t1.public_key t2.public_key;
      jwks_uri = Option.first_some t1.jwks_uri t2.jwks_uri;
      issuer = Option.first_some t1.issuer t2.issuer;
      issuer_list = Option.first_some t1.issuer_list t2.issuer_list;
      algorithm = Option.first_some t1.algorithm t2.algorithm;
      audience = Option.first_some t1.audience t2.audience;
      audience_list = Option.first_some t1.audience_list t2.audience_list;
      required_scopes = Option.first_some t1.required_scopes t2.required_scopes;
      base_url = Option.first_some t1.base_url t2.base_url;
    }

  let validate_key_source (t : t) =
    match (t.public_key, t.jwks_uri) with
    | None, None ->
      Error (Error.of_string "Either public_key or jwks_uri must be provided")
    | Some _, Some _ ->
      Error (Error.of_string "Provide either public_key or jwks_uri, not both")
    | _ -> Ok ()

  let validate_algorithm (t : t) =
    match t.algorithm with
    | None -> Ok () (* default will be applied *)
    | Some alg ->
      if Set.mem supported_algorithms alg then Ok ()
      else Error (Error.of_string (sprintf "Unsupported algorithm: %s" alg))

  let validate (t : t) =
    match validate_key_source t with
    | Error e -> Error e
    | Ok () -> validate_algorithm t
end

(** JWT token verifier supporting both asymmetric (RSA/ECDSA) and symmetric
    (HMAC) algorithms.

    Use this when:
    - You have JWT tokens issued by an external service (asymmetric)
    - You need JWKS support for automatic key rotation (asymmetric)
    - You have internal microservices sharing a secret key (symmetric)
    - Your tokens contain standard OAuth scopes and claims *)
module Jwt_verifier = struct
  type t = {
    settings : Settings.t;
    public_key : string option;
    jwks_uri : string option;
    issuer : string list option; (* normalized to list *)
    audience : string list option; (* normalized to list *)
    algorithm : string;
    required_scopes : string list;
    base_url : string option;
    cache_ttl : int; (* seconds *)
  }
  [@@deriving sexp, compare]

  let create ?public_key ?jwks_uri ?issuer ?issuer_list ?audience ?audience_list
      ?algorithm ?required_scopes ?base_url () =
    let explicit_settings =
      Settings.create ?public_key ?jwks_uri ?issuer ?issuer_list ?algorithm
        ?audience ?audience_list ?required_scopes ?base_url ()
    in
    let env_settings = Settings.load_from_env () in
    let settings = Settings.merge explicit_settings env_settings in

    match Settings.validate settings with
    | Error e -> Error e
    | Ok () ->
      let public_key = settings.public_key in
      let jwks_uri = settings.jwks_uri in
      let algorithm = Option.value settings.algorithm ~default:"RS256" in
      let required_scopes = Option.value settings.required_scopes ~default:[] in
      let base_url = settings.base_url in

      (* Normalize issuer to list *)
      let issuer =
        match (settings.issuer, settings.issuer_list) with
        | Some iss, _ -> Some [ iss ]
        | None, Some iss_list -> Some iss_list
        | None, None -> None
      in

      (* Normalize audience to list *)
      let audience =
        match (settings.audience, settings.audience_list) with
        | Some aud, _ -> Some [ aud ]
        | None, Some aud_list -> Some aud_list
        | None, None -> None
      in

      let key_source =
        if Option.is_some public_key then "public_key" else "jwks_uri"
      in
      Logging.Logger.debug logger
        (sprintf "Initialized JWT Verifier with %s, algorithm: %s" key_source
           algorithm);

      Ok
        {
          settings;
          public_key;
          jwks_uri;
          issuer;
          audience;
          algorithm;
          required_scopes;
          base_url;
          cache_ttl = 3600;
        }

  (** Get the public key *)
  let public_key t = t.public_key

  (** Get the JWKS URI *)
  let jwks_uri t = t.jwks_uri

  (** Get the issuer(s) *)
  let issuer t = t.issuer

  (** Get the audience(s) *)
  let audience t = t.audience

  (** Get the algorithm *)
  let algorithm t = t.algorithm

  (** Get required scopes *)
  let required_scopes t = t.required_scopes

  (** Get base URL *)
  let base_url t = t.base_url

  (** Get cache TTL in seconds *)
  let cache_ttl t = t.cache_ttl

  (** Extract scopes from JWT claims. Supports both 'scope' and 'scp' claims. *)
  let extract_scopes json =
    let try_claim name =
      match json with
      | `Assoc fields -> (
        match List.Assoc.find fields ~equal:String.equal name with
        | Some (`String s) ->
          Some
            (String.split s ~on:' '
            |> List.filter ~f:(fun s -> not (String.is_empty s)))
        | Some (`List arr) ->
          Some
            (List.filter_map arr ~f:(function
              | `String s when not (String.is_empty s) -> Some s
              | _ -> None))
        | _ -> None)
      | _ -> None
    in
    match try_claim "scope" with
    | Some scopes -> scopes
    | None -> Option.value (try_claim "scp") ~default:[]

  (** Check if all required scopes are present *)
  let check_required_scopes ~required ~token_scopes =
    if List.is_empty required then true
    else
      let required_set = Set.of_list (module String) required in
      let token_set = Set.of_list (module String) token_scopes in
      Set.is_subset required_set ~of_:token_set

  (** Extract client ID from claims. Tries client_id, azp, sub in order. *)
  let extract_client_id json =
    match json with
    | `Assoc fields -> (
      let try_field name =
        List.Assoc.find fields ~equal:String.equal name
        |> Option.bind ~f:(function
             | `String s -> Some s
             | _ -> None)
      in
      match try_field "client_id" with
      | Some id -> id
      | None -> (
        match try_field "azp" with
        | Some id -> id
        | None -> Option.value (try_field "sub") ~default:"unknown"))
    | _ -> "unknown"

  (** Validate issuer against expected list *)
  let validate_issuer ~expected ~actual =
    match expected with
    | None -> true
    | Some issuers -> List.mem issuers actual ~equal:String.equal

  (** Validate audience against expected list. Actual audience can be a string
      or list. *)
  let validate_audience ~expected ~actual =
    match expected with
    | None -> true
    | Some expected_list -> (
      match actual with
      | `String aud -> List.mem expected_list aud ~equal:String.equal
      | `List auds ->
        List.exists expected_list ~f:(fun exp ->
            List.exists auds ~f:(function
              | `String a -> String.equal a exp
              | _ -> false))
      | _ -> false)
end

(** Simple static token verifier for testing and development.

    This verifier validates tokens against a predefined dictionary of valid
    token strings and their associated claims.

    WARNING: Never use this in production - tokens are stored in plain text! *)
module Static_token_verifier = struct
  type token_data = {
    client_id : string;
    scopes : string list;
    expires_at : int option;
    claims_json : string; (* JSON string instead of Yojson.Safe.t *)
  }
  [@@deriving sexp, compare]
  (** Token data stored for static tokens. claims_json stores the serialized
      JSON string of additional claims. *)

  type t = {
    tokens : (string, token_data) Hashtbl.t;
    required_scopes : string list;
  }

  let create ~tokens ?(required_scopes = []) () =
    let tbl = Hashtbl.create (module String) in
    List.iter tokens ~f:(fun (token, data) -> Hashtbl.set tbl ~key:token ~data);
    { tokens = tbl; required_scopes }

  (** Get required scopes *)
  let required_scopes t = t.required_scopes

  (** Get the token data for a given token string, or None *)
  let get_token_data t token = Hashtbl.find t.tokens token

  (** Check if token is expired *)
  let is_expired token_data =
    match token_data.expires_at with
    | None -> false
    | Some exp ->
      let now = Core_unix.gettimeofday () |> Float.to_int in
      exp < now

  (** Check required scopes against token scopes *)
  let check_scopes t token_data =
    if List.is_empty t.required_scopes then true
    else
      let required_set = Set.of_list (module String) t.required_scopes in
      let token_set = Set.of_list (module String) token_data.scopes in
      Set.is_subset required_set ~of_:token_set

  (** Create token_data from basic parameters *)
  let make_token_data ~client_id ?(scopes = []) ?expires_at
      ?(claims_json = "{}") () =
    { client_id; scopes; expires_at; claims_json }
end
