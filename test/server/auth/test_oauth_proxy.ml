(** Tests for OAuth Proxy Provider functionality.

    Note: Tests that require Mirage_crypto_rng are skipped because the RNG needs
    special initialization (mirage-crypto-rng-unix) which is not available in
    this environment. The PKCE functions are tested via integration tests
    instead. *)

open! Core
open Server_auth.Oauth_proxy

(** Test PKCE verify - uses deterministic inputs only *)
let%expect_test "Pkce.verify - plain method" =
  let valid = Pkce.verify ~verifier:"test" ~challenge:"test" ~method_:"plain" in
  let invalid =
    Pkce.verify ~verifier:"test" ~challenge:"wrong" ~method_:"plain"
  in
  printf "Valid verifier: %b\n" valid;
  printf "Invalid verifier: %b\n" invalid;
  [%expect {|
    Valid verifier: true
    Invalid verifier: false
    |}]

(** Test Cookie utilities - uses deterministic inputs only *)
let%expect_test "Cookie.sign and verify - valid signature" =
  let secret = "test-secret-key" in
  let payload = "test-payload" in
  let signed = Cookie.sign ~secret ~payload in
  let verified = Cookie.verify ~secret ~signed_value:signed in
  printf "Signed contains dot: %b\n" (String.is_substring signed ~substring:".");
  printf "Verified payload: %s\n" (Option.value verified ~default:"NONE");
  [%expect
    {|
    Signed contains dot: true
    Verified payload: test-payload
    |}]

let%expect_test "Cookie.verify - invalid signature" =
  let secret = "test-secret-key" in
  let tampered = "dGVzdC1wYXlsb2Fk.invalidsignature" in
  let verified = Cookie.verify ~secret ~signed_value:tampered in
  printf "Verified: %s\n"
    (match verified with
    | None -> "None"
    | Some p -> p);
  [%expect {| Verified: None |}]

(** Test Storage module *)
let%expect_test "Storage - basic operations" =
  let store = Storage.create () in
  Storage.put store ~key:"key1" ~value:"value1";
  let v1 = Storage.get store ~key:"key1" in
  let v2 = Storage.get store ~key:"nonexistent" in
  printf "key1: %s\n" (Option.value v1 ~default:"NONE");
  printf "nonexistent: %s\n" (Option.value v2 ~default:"NONE");
  Storage.remove store ~key:"key1";
  let v3 = Storage.get store ~key:"key1" in
  printf "after remove: %s\n" (Option.value v3 ~default:"NONE");
  [%expect
    {|
    key1: value1
    nonexistent: NONE
    after remove: NONE
    |}]

(** Test HTML generation *)
let%expect_test "create_consent_html - basic" =
  let html =
    create_consent_html ~client_id:"test-client"
      ~redirect_uri:"http://localhost:3000/callback" ~scopes:[ "read"; "write" ]
      ~txn_id:"txn-123" ~csrf_token:"csrf-456" ()
  in
  printf "Contains form: %b\n" (String.is_substring html ~substring:"<form");
  printf "Contains txn_id: %b\n" (String.is_substring html ~substring:"txn-123");
  printf "Contains csrf_token: %b\n"
    (String.is_substring html ~substring:"csrf-456");
  printf "Contains approve button: %b\n"
    (String.is_substring html ~substring:"approve");
  printf "Contains deny button: %b\n"
    (String.is_substring html ~substring:"deny");
  [%expect
    {|
    Contains form: true
    Contains txn_id: true
    Contains csrf_token: true
    Contains approve button: true
    Contains deny button: true
    |}]

let%expect_test "create_error_html - basic" =
  let html =
    create_error_html ~error_title:"Test Error"
      ~error_message:"Something failed" ()
  in
  printf "Contains title: %b\n"
    (String.is_substring html ~substring:"Test Error");
  printf "Contains message: %b\n"
    (String.is_substring html ~substring:"Something failed");
  [%expect {|
    Contains title: true
    Contains message: true
    |}]

let%expect_test "create_error_html - with details" =
  let html =
    create_error_html ~error_title:"Auth Error" ~error_message:"Invalid client"
      ~error_details:[ ("Code", "invalid_client"); ("Status", "401") ]
      ()
  in
  printf "Contains Code: %b\n" (String.is_substring html ~substring:"Code");
  printf "Contains invalid_client: %b\n"
    (String.is_substring html ~substring:"invalid_client");
  [%expect {|
    Contains Code: true
    Contains invalid_client: true
    |}]
