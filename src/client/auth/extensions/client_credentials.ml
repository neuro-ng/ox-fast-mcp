open! Core
open! Async

(* Logger for client credentials authentication *)
let logger =
  Logging.Logger.get_logger "Client.Auth.Extensions.ClientCredentials"

(* JWT Parameters for RFC 7523 JWT Bearer authentication

   This module handles JWT assertion generation for OAuth2 client credentials
   flow as defined in RFC 7523. *)
module Jwt_parameters = struct
  type t = {
    assertion : string option;
    issuer : string option;
    subject : string option;
    audience : string option;
    claims : (string * Yojson.Safe.t) list option;
    jwt_signing_algorithm : string option;
    jwt_signing_key : Secret_string.t option;
    jwt_lifetime_seconds : int;
  }

  (* Custom sexp serialization that handles Yojson and redacts secrets *)
  let sexp_of_t t =
    let open Sexp in
    List
      [
        List [ Atom "assertion"; [%sexp_of: string option] t.assertion ];
        List [ Atom "issuer"; [%sexp_of: string option] t.issuer ];
        List [ Atom "subject"; [%sexp_of: string option] t.subject ];
        List [ Atom "audience"; [%sexp_of: string option] t.audience ];
        List
          [
            Atom "claims";
            (match t.claims with
            | None -> Atom "None"
            | Some _claims -> Atom "<REDACTED>");
          ];
        List
          [
            Atom "jwt_signing_algorithm";
            [%sexp_of: string option] t.jwt_signing_algorithm;
          ];
        List
          [
            Atom "jwt_signing_key";
            (match t.jwt_signing_key with
            | None -> Atom "None"
            | Some _ -> Atom "<REDACTED>");
          ];
        List
          [
            Atom "jwt_lifetime_seconds"; [%sexp_of: int] t.jwt_lifetime_seconds;
          ];
      ]

  (* Minimal t_of_sexp - claims are complex to deserialize so we leave them
     empty *)
  let t_of_sexp sexp =
    let open Sexp in
    match sexp with
    | List fields ->
      let get_field name =
        List.find_map fields ~f:(fun field ->
            match field with
            | List [ Atom field_name; value ] when String.equal field_name name
              -> Some value
            | _ -> None)
      in
      {
        assertion =
          Option.bind (get_field "assertion") ~f:[%of_sexp: string option];
        issuer = Option.bind (get_field "issuer") ~f:[%of_sexp: string option];
        subject = Option.bind (get_field "subject") ~f:[%of_sexp: string option];
        audience =
          Option.bind (get_field "audience") ~f:[%of_sexp: string option];
        claims = None (* Claims deserialization not supported *);
        jwt_signing_algorithm =
          Option.bind
            (get_field "jwt_signing_algorithm")
            ~f:[%of_sexp: string option];
        jwt_signing_key =
          None (* Key deserialization not supported for security *);
        jwt_lifetime_seconds =
          (match get_field "jwt_lifetime_seconds" with
          | Some v -> [%of_sexp: int] v
          | None -> 300);
      }
    | Atom _ -> failwith "Jwt_parameters.t_of_sexp: expected List, got Atom"

  let create ?assertion ?issuer ?subject ?audience ?claims
      ?(jwt_signing_algorithm = "RS256") ?jwt_signing_key
      ?(jwt_lifetime_seconds = 300) () =
    let jwt_signing_key = Option.map jwt_signing_key ~f:Secret_string.create in
    {
      assertion;
      issuer;
      subject;
      audience;
      claims;
      jwt_signing_algorithm = Some jwt_signing_algorithm;
      jwt_signing_key;
      jwt_lifetime_seconds;
    }

  (* Generate JWT assertion with the configured parameters

     @param t The JWT parameters @param with_audience_fallback Fallback audience
     if not configured @return The JWT assertion string @raise Failure if
     required parameters are missing *)
  let to_assertion t ~with_audience_fallback =
    match t.assertion with
    | Some assertion ->
      (* Use predefined JWT assertion *)
      Logging.Logger.debug logger "Using predefined JWT assertion";
      assertion
    | None -> (
      (* Generate new JWT assertion *)
      match t.jwt_signing_key with
      | None -> failwith "Missing signing key for JWT bearer grant"
      | Some _key_secret -> (
        match t.issuer with
        | None -> failwith "Missing issuer for JWT bearer grant"
        | Some issuer -> (
          match t.subject with
          | None -> failwith "Missing subject for JWT bearer grant"
          | Some subject -> (
            let audience =
              match t.audience with
              | Some aud -> Some aud
              | None -> with_audience_fallback
            in
            match audience with
            | None -> failwith "Missing audience for JWT bearer grant"
            | Some aud ->
              (* TODO: Implement actual JWT generation using jose or ocaml-jwt
                 library For now, this is a stub that shows the structure.

                 Required steps: 1. Get current time for iat and exp claims 2.
                 Generate UUID for jti claim 3. Build claims object with iss,
                 sub, aud, exp, iat, jti 4. Merge in additional claims from
                 t.claims 5. Sign with t.jwt_signing_key using
                 t.jwt_signing_algorithm *)
              let now = Float.to_int (Core_unix.time ()) in
              let exp = now + t.jwt_lifetime_seconds in
              (* Generate a simple UUID v4-like string using random data *)
              let jti =
                sprintf "%08x-%04x-4%03x-%04x-%012x" (Random.int 0x100000000)
                  (Random.int 0x10000) (Random.int 0x1000) (Random.int 0x10000)
                  (Random.int 0x1000000000000)
              in
              (* Build base claims *)
              let base_claims =
                [
                  ("iss", `String issuer);
                  ("sub", `String subject);
                  ("aud", `String aud);
                  ("exp", `Int exp);
                  ("iat", `Int now);
                  ("jti", `String jti);
                ]
              in
              (* Merge additional claims *)
              let all_claims =
                match t.claims with
                | None -> base_claims
                | Some extra ->
                  (* Additional claims override base claims for same keys *)
                  let claim_map = String.Map.of_alist_exn base_claims in
                  let updated_map =
                    List.fold extra ~init:claim_map ~f:(fun acc (k, v) ->
                        Map.set acc ~key:k ~data:v)
                  in
                  Map.to_alist updated_map
              in
              Logging.Logger.debug logger
                (sprintf
                   "Generated JWT claims for issuer=%s, subject=%s, audience=%s"
                   issuer subject aud);
              (* TODO: Actually encode and sign the JWT This requires
                 integrating a JWT library like jose or ocaml-jwt For now,
                 return a placeholder indicating the structure *)
              let claims_json = `Assoc all_claims |> Yojson.Safe.to_string in
              sprintf "STUB_JWT_TODO[alg=%s,claims=%s]"
                (Option.value t.jwt_signing_algorithm ~default:"RS256")
                claims_json))))
end

(* RFC 7523 OAuth Client Provider

   This is a STUB implementation showing the intended structure. Full
   implementation requires: - OAuthClientProvider base class - OAuth metadata
   types - Token storage interface - HTTP client integration

   See client_credentials.todo for details. *)
module Rfc7523_oauth_client_provider = struct
  (* This is a stub type showing the intended structure *)
  type t = {
    server_url : string;
    jwt_parameters : Jwt_parameters.t option;
        (* TODO: Add fields from OAuthClientProvider base class: -
           client_metadata: OAuthClientMetadata.t - storage: TokenStorage.t -
           redirect_handler: (string -> unit Deferred.t) option -
           callback_handler: (unit -> (string * string option) Deferred.t)
           option - timeout: float - context: OAuth provider context *)
  }
  [@@deriving sexp]

  let create ~server_url ?jwt_parameters () = { server_url; jwt_parameters }

  (* Stub methods - TODO: Implement when OAuth infrastructure is available *)

  let _exchange_token_jwt_bearer _t =
    failwith
      "TODO: Implement _exchange_token_jwt_bearer - requires OAuth metadata, \
       HTTP client, and token endpoint"

  let _add_client_authentication_jwt _t ~token_data:_ =
    failwith
      "TODO: Implement _add_client_authentication_jwt - requires OAuth context \
       and metadata"

  let _perform_authorization _t =
    failwith
      "TODO: Implement _perform_authorization - requires full OAuth client \
       provider base class"

  let _exchange_token_authorization_code _t _auth_code _code_verifier
      ~token_data:_ =
    failwith
      "TODO: Implement _exchange_token_authorization_code - requires OAuth \
       base class"
end
