open Core

(* open Lwt.Syntax *)
open Mirage_crypto
(* open Mirage_crypto_pk *)

(* Minimal KDF implementations since dependent libraries might be missing *)

(* HMAC-SHA256 *)
let hmac_sha256 ~key data =
  let key = Cstruct.of_string key in
  let data = Cstruct.of_string data in
  Hash.SHA256.hmac ~key data |> Cstruct.to_string

(* HKDF-SHA256 *)
(* RFC 5869 *)
let hkdf_extract ~salt ~ikm = hmac_sha256 ~key:salt ikm

let hkdf_expand ~prk ~info ~length =
  let prk = Cstruct.of_string prk in
  let info = Cstruct.of_string info in
  let n = Float.iround_up_exn (float_of_int length /. 32.0) in
  let rec loop i prev acc =
    if i > n then acc
    else
      let t_prev = if i > 1 then prev else Cstruct.empty in
      let counter = Cstruct.of_string (String.make 1 (char_of_int i)) in
      let data = Cstruct.concat [ t_prev; info; counter ] in
      let t_i = Hash.SHA256.hmac ~key:prk data in
      loop (i + 1) t_i (Cstruct.append acc t_i)
  in
  let t = loop 1 Cstruct.empty Cstruct.empty in
  Cstruct.to_string (Cstruct.sub t 0 length)

let hkdf_derive ~salt ~ikm ~info ~length =
  let prk = hkdf_extract ~salt ~ikm in
  hkdf_expand ~prk ~info ~length

(* PBKDF2-HMAC-SHA256 *)
(* RFC 2898 *)
let pbkdf2_derive ~salt ~password ~iterations ~length =
  (* This is a complex logic to implement correctly without bugs. Given the
     constraints, checking if we can use a simpler approach or if there's a
     library we missed. Revisiting: Using a simple placeholder or a very careful
     implementation.

     Actually, if we can avoid implementing PBKDF2 by only supporting
     high_entropy_material (HKDF), that would be safer, but we need to support
     low_entropy matching existing logic.

     Let's try to implement a basic PBKDF2/HMAC-SHA256 loop. *)
  let password = Cstruct.of_string password in
  let salt = Cstruct.of_string salt in
  let hLen = 32 in
  (* SHA256 output length *)
  let dkLen = length in
  let l = Float.iround_up_exn (float_of_int dkLen /. float_of_int hLen) in

  let f password salt c i =
    let i_be = Cstruct.create 4 in
    Cstruct.BE.set_uint32 i_be 0 (Int32.of_int_exn i);
    let u_1 = Hash.SHA256.hmac ~key:password (Cstruct.append salt i_be) in
    let rec xor_loop u_prev u_acc k =
      if k >= c then u_acc
      else
        let u_curr = Hash.SHA256.hmac ~key:password u_prev in
        (* XOR u_curr into u_acc *)
        let u_next = Mirage_crypto.Uncommon.Cs.xor u_acc u_curr in
        xor_loop u_curr u_next (k + 1)
    in
    xor_loop u_1 u_1 1
  in

  let rec outer_loop i acc =
    if i > l then acc
    else
      let block = f password salt iterations i in
      outer_loop (i + 1) (Cstruct.append acc block)
  in

  let derived = outer_loop 1 Cstruct.empty in
  Cstruct.to_string (Cstruct.sub derived 0 dkLen)

let derive_jwt_key ?high_entropy_material ?low_entropy_material ~salt () =
  match (high_entropy_material, low_entropy_material) with
  | Some _, Some _ ->
    invalid_arg
      "Either high_entropy_material or low_entropy_material must be provided, \
       but not both"
  | None, None ->
    invalid_arg
      "Either high_entropy_material or low_entropy_material must be provided"
  | Some hem, None ->
    let key = hkdf_derive ~salt ~ikm:hem ~info:"Fernet" ~length:32 in
    Base64.encode_string ~alphabet:Base64.uri_safe_alphabet key
  | None, Some lem ->
    let key =
      pbkdf2_derive ~salt ~password:lem ~iterations:1000000 ~length:32
    in
    Base64.encode_string ~alphabet:Base64.uri_safe_alphabet key

module JWTIssuer = struct
  type t = {
    issuer : string;
    audience : string;
    signing_key : string;
        (* Raw key bytes or base64? Python uses bytes derived. *)
        (* Python derived key is base64 encoded string returned by
           derive_jwt_key. Wait, derive_jwt_key returns bytes in python
           annotations -> bytes? Python: return
           base64.urlsafe_b64encode(derived_key) which is BYTES. So signing_key
           passed to JWTIssuer is bytes. In OCaml we'll treat it as string
           (bytes). *)
  }

  let create ~issuer ~audience ~signing_key = { issuer; audience; signing_key }

  (* Helper to encode HEADER.PAYLOAD *)
  (* Jose library usually handles this but we need to match "HS256" and specific claims. *)

  let issue_access_token t ~client_id ~scopes ~jti ?(expires_in = 3600) () =
    let now = Core_unix.time () |> int_of_float in
    let exp = now + expires_in in

    let payload =
      `Assoc
        [
          ("iss", `String t.issuer);
          ("aud", `String t.audience);
          ("client_id", `String client_id);
          ("scope", `String (String.concat ~sep:" " scopes));
          ("exp", `Int exp);
          ("iat", `Int now);
          ("jti", `String jti);
        ]
    in

    let key = Jose.Jwk.make_oct t.signing_key in
    match Jose.Jwt.sign ~payload key with
    | Ok token ->
      (* Log debug... skipping utility logger for now or use Printf/Logs *)
      token
    | Error _ -> failwith "Failed to sign or verify token (Jose error)"

  let issue_refresh_token t ~client_id ~scopes ~jti ~expires_in =
    let now = Core_unix.time () |> int_of_float in
    let exp = now + expires_in in

    let payload =
      `Assoc
        [
          ("iss", `String t.issuer);
          ("aud", `String t.audience);
          ("client_id", `String client_id);
          ("scope", `String (String.concat ~sep:" " scopes));
          ("exp", `Int exp);
          ("iat", `Int now);
          ("jti", `String jti);
          ("token_use", `String "refresh");
        ]
    in

    let key = Jose.Jwk.make_oct t.signing_key in
    match Jose.Jwt.sign ~payload key with
    | Ok token -> token
    | Error _ -> failwith "Failed to sign refresh token (Jose error)"

  let verify_token t token =
    let key = Jose.Jwk.make_oct t.signing_key in
    let now = Core_unix.time () |> Ptime.of_float_s |> Option.value_exn in
    match Jose.Jwt.validate ~now ~jwk:key token with
    | Ok jwt ->
      let payload = jwt.payload in

      (* Validate exp, iss, aud manually if Jose doesn't do strict check or to be sure *)
      (* Jose.Jwt.validate checks exp if present. *)
      let open Yojson.Safe.Util in
      let iss = payload |> member "iss" |> to_string_option in
      let aud = payload |> member "aud" |> to_string_option in

      (match iss with
      | Some i when String.equal i t.issuer -> ()
      | _ -> failwith "Invalid token issuer");

      (match aud with
      | Some a when String.equal a t.audience -> ()
      | _ -> failwith "Invalid token audience");

      (* Return payload as Yojson object *)
      payload
    | Error _ -> failwith "Token validation failed (Jose error)"
end
