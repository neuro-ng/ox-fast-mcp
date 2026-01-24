(** Tests for OAuth proxy with persistent storage.

    Translation of Python test_oauth_proxy_storage.py.

    Tests the OxFastMCP OAuth Proxy storage functionality including:
    - Registering and retrieving clients
    - Client persistence in storage
    - ProxyDCRClient with redirect patterns *)

open! Core
open Lwt.Syntax
open Server_auth.Oauth_proxy

[@@@alert "-unsafe_multidomain"]

(* ============================================================================
   Helper Functions
   ============================================================================ *)

(** Create an OAuth proxy for testing *)
let create_test_proxy ?allowed_client_redirect_uris () =
  create
    ~upstream_authorization_endpoint:"https://github.com/login/oauth/authorize"
    ~upstream_token_endpoint:"https://github.com/login/oauth/access_token"
    ~upstream_client_id:"test-client-id"
    ~upstream_client_secret:"test-client-secret"
    ~base_url:"https://myserver.com" ~redirect_path:"/auth/callback"
    ?allowed_client_redirect_uris ~required_scopes:[ "read"; "write" ] ()

(* ============================================================================
   TestOAuthProxyStorage - Client storage functionality
   ============================================================================ *)

let%expect_test "register_and_get_client - basic flow" =
  Lwt_main.run
    (let proxy = create_test_proxy () in
     (* Register client *)
     let* registered =
       register_client proxy ~client_id:"test-client-123"
         ~client_secret:"secret-456"
         ~redirect_uris:[ "http://localhost:8080/callback" ]
         ~client_name:"Test Client" ()
     in
     printf "Registered client_id: %s\n" registered.pdc_client_id;
     (* Get client back *)
     let* client = get_client proxy ~client_id:"test-client-123" in
     (match client with
     | None -> print_endline "ERROR: Client not found"
     | Some c ->
       printf "Retrieved client_id: %s\n" c.pdc_client_id;
       printf "Has secret: %b\n" (Option.is_some c.client_secret);
       printf "Redirect URIs count: %d\n" (List.length c.redirect_uris));
     Lwt.return ());
  [%expect
    {|
    Registered client_id: test-client-123
    Retrieved client_id: test-client-123
    Has secret: true
    Redirect URIs count: 1
    |}]

let%expect_test "nonexistent_client_returns_none" =
  Lwt_main.run
    (let proxy = create_test_proxy () in
     let* client = get_client proxy ~client_id:"does-not-exist" in
     (match client with
     | None -> print_endline "Non-existent client returns None"
     | Some _ -> print_endline "ERROR: Found non-existent client");
     Lwt.return ());
  [%expect {| Non-existent client returns None |}]

let%expect_test "proxy_dcr_client_redirect_validation" =
  Lwt_main.run
    (let proxy =
       create_test_proxy ~allowed_client_redirect_uris:[ "http://localhost:*" ]
         ()
     in
     (* Register client *)
     let* _registered =
       register_client proxy ~client_id:"test-proxy-client"
         ~client_secret:"secret"
         ~redirect_uris:[ "http://localhost:8080/callback" ]
         ()
     in
     (* Get client back *)
     let* client = get_client proxy ~client_id:"test-proxy-client" in
     (match client with
     | None -> print_endline "ERROR: Client not found"
     | Some c ->
       print_endline "Client retrieved";
       (* Validate redirect URIs - dynamic localhost ports should be allowed *)
       let valid =
         validate_redirect_uri proxy ~client:c
           ~redirect_uri:"http://localhost:12345/callback"
       in
       printf "Dynamic localhost port allowed: %b\n" valid);
     Lwt.return ());
  [%expect
    {|
    Client retrieved
    Dynamic localhost port allowed: true
    |}]

let%expect_test "in_memory_storage - same instance shares data" =
  Lwt_main.run
    (let proxy1 = create_test_proxy () in
     (* Register client in first proxy *)
     let* _registered =
       register_client proxy1 ~client_id:"memory-client"
         ~client_secret:"memory-secret"
         ~redirect_uris:[ "http://localhost:8080/callback" ]
         ()
     in
     print_endline "Client registered in proxy1";
     (* Same proxy can retrieve it *)
     let* client = get_client proxy1 ~client_id:"memory-client" in
     (match client with
     | None -> print_endline "ERROR: Client not found in same proxy"
     | Some _ -> print_endline "Client found in same proxy");
     (* New proxy instance won't have it (separate in-memory storage) *)
     let proxy2 = create_test_proxy () in
     let* client2 = get_client proxy2 ~client_id:"memory-client" in
     (match client2 with
     | None -> print_endline "New proxy instance has separate storage"
     | Some _ -> print_endline "ERROR: Found client in new proxy");
     Lwt.return ());
  [%expect
    {|
    Client registered in proxy1
    Client found in same proxy
    New proxy instance has separate storage
    |}]

let%expect_test "storage_operations - put get remove" =
  let store = Storage.create () in
  (* Put a value *)
  Storage.put store ~key:"client-1" ~value:"data-1";
  print_endline "Put client-1";
  (* Get it back *)
  let v1 = Storage.get store ~key:"client-1" in
  printf "Get client-1: %s\n" (Option.value v1 ~default:"NONE");
  (* Non-existent key *)
  let v2 = Storage.get store ~key:"non-existent" in
  printf "Get non-existent: %s\n" (Option.value v2 ~default:"NONE");
  (* Remove *)
  Storage.remove store ~key:"client-1";
  let v3 = Storage.get store ~key:"client-1" in
  printf "After remove: %s\n" (Option.value v3 ~default:"NONE");
  (* Clear all *)
  Storage.put store ~key:"a" ~value:"1";
  Storage.put store ~key:"b" ~value:"2";
  Storage.clear store;
  let v4 = Storage.get store ~key:"a" in
  printf "After clear: %s\n" (Option.value v4 ~default:"NONE");
  [%expect
    {|
    Put client-1
    Get client-1: data-1
    Get non-existent: NONE
    After remove: NONE
    After clear: NONE
    |}]

let%expect_test "client_data_structure - fields preserved" =
  Lwt_main.run
    (let proxy = create_test_proxy () in
     let* registered =
       register_client proxy ~client_id:"structured-client"
         ~client_secret:"secret"
         ~redirect_uris:[ "http://localhost:8080/callback" ]
         ~client_name:"My App" ()
     in
     (* Check fields are set correctly *)
     printf "client_id: %s\n" registered.pdc_client_id;
     printf "has_secret: %b\n" (Option.is_some registered.client_secret);
     printf "redirect_uris: [%s]\n"
       (String.concat ~sep:", " registered.redirect_uris);
     printf "client_name: %s\n"
       (Option.value registered.client_name ~default:"NONE");
     printf "created_at > 0: %b\n" Float.(registered.pdc_created_at > 0.0);
     Lwt.return ());
  [%expect
    {|
    client_id: structured-client
    has_secret: true
    redirect_uris: [http://localhost:8080/callback]
    client_name: My App
    created_at > 0: true
    |}]

let%expect_test "multiple_clients_isolated" =
  Lwt_main.run
    (let proxy = create_test_proxy () in
     (* Register two clients *)
     let* _c1 =
       register_client proxy ~client_id:"client-a" ~client_secret:"secret-a"
         ~redirect_uris:[ "http://a.example.com" ] ()
     in
     let* _c2 =
       register_client proxy ~client_id:"client-b" ~client_secret:"secret-b"
         ~redirect_uris:[ "http://b.example.com" ] ()
     in
     (* Retrieve each *)
     let* client_a = get_client proxy ~client_id:"client-a" in
     let* client_b = get_client proxy ~client_id:"client-b" in
     (match (client_a, client_b) with
     | Some a, Some b ->
       printf "Client A secret: %s\n"
         (Option.value a.client_secret ~default:"NONE");
       printf "Client B secret: %s\n"
         (Option.value b.client_secret ~default:"NONE");
       printf "Clients are different: %b\n"
         (not (String.equal a.pdc_client_id b.pdc_client_id))
     | _ -> print_endline "ERROR: Missing client");
     Lwt.return ());
  [%expect
    {|
    Client A secret: secret-a
    Client B secret: secret-b
    Clients are different: true
    |}]
