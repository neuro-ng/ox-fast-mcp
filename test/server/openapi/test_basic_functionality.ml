(** Tests for OpenAPI basic functionality.

    Translated from Python test_basic_functionality.py to OCaml. Tests OpenAPI
    server creation, route mapping, and component types.

    Note: Python tests require Client/httpx not available in OCaml. These tests
    focus on unit testing available functionality directly. *)

open! Core
open! Expect_test_helpers_core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* Use conftest module for fixtures *)
module Conftest = Conftest

(* =============================================================================
   Test: MCPType
   ============================================================================= *)

let%expect_test "MCPType.to_string - converts types correctly" =
  printf "Tool: %s\n" (Conftest.MCPType.to_string Conftest.MCPType.Tool);
  printf "Resource: %s\n" (Conftest.MCPType.to_string Conftest.MCPType.Resource);
  printf "ResourceTemplate: %s\n"
    (Conftest.MCPType.to_string Conftest.MCPType.ResourceTemplate);
  printf "Exclude: %s\n" (Conftest.MCPType.to_string Conftest.MCPType.Exclude);
  [%expect
    {|
    Tool: tool
    Resource: resource
    ResourceTemplate: resource_template
    Exclude: exclude
    |}]

let%expect_test "MCPType.of_string - parses types correctly" =
  let test_of_string s =
    match Conftest.MCPType.of_string s with
    | Some t -> Conftest.MCPType.to_string t
    | None -> "None"
  in
  printf "tool: %s\n" (test_of_string "tool");
  printf "resource: %s\n" (test_of_string "resource");
  printf "resource_template: %s\n" (test_of_string "resource_template");
  printf "exclude: %s\n" (test_of_string "exclude");
  printf "invalid: %s\n" (test_of_string "invalid");
  [%expect
    {|
    tool: tool
    resource: resource
    resource_template: resource_template
    exclude: exclude
    invalid: None
    |}]

(* =============================================================================
   Test: route_map
   ============================================================================= *)

let%expect_test "get_route_maps - has correct structure" =
  let maps = Conftest.get_route_maps in
  printf "count: %d\n" (List.length maps);
  List.iteri maps ~f:(fun i map ->
      printf "map[%d].methods: %s\n" i (String.concat ~sep:"," map.methods);
      printf "map[%d].mcp_type: %s\n" i
        (Conftest.MCPType.to_string map.mcp_type));
  [%expect
    {|
    count: 2
    map[0].methods: GET
    map[0].mcp_type: resource_template
    map[1].methods: GET
    map[1].mcp_type: resource
    |}]

let%expect_test "get_route_maps - first pattern matches paths with params" =
  let maps = Conftest.get_route_maps in
  let first_map = List.hd_exn maps in
  let regex = Re.Pcre.regexp first_map.pattern in
  printf "matches /users/{user_id}: %b\n" (Re.execp regex "/users/{user_id}");
  printf "matches /users/{id}/active: %b\n"
    (Re.execp regex "/users/{id}/active");
  printf "matches /users: %b\n" (Re.execp regex "/users");
  [%expect
    {|
    matches /users/{user_id}: true
    matches /users/{id}/active: true
    matches /users: false
    |}]

let%expect_test "get_route_maps - second pattern matches any path" =
  let maps = Conftest.get_route_maps in
  let second_map = List.nth_exn maps 1 in
  let regex = Re.Pcre.regexp second_map.pattern in
  printf "matches /users: %b\n" (Re.execp regex "/users");
  printf "matches /search: %b\n" (Re.execp regex "/search");
  printf "matches /ping: %b\n" (Re.execp regex "/ping");
  [%expect
    {|
    matches /users: true
    matches /search: true
    matches /ping: true
    |}]

(* =============================================================================
   Test: User Types (from conftest)
   ============================================================================= *)

let%expect_test "user - creates correctly" =
  let user : Conftest.user = { id = 1; name = "Alice"; active = true } in
  printf "id: %d\n" user.id;
  printf "name: %s\n" user.name;
  printf "active: %b\n" user.active;
  [%expect {|
    id: 1
    name: Alice
    active: true
    |}]

let%expect_test "user - serializes to JSON correctly" =
  let user : Conftest.user = { id = 1; name = "Alice"; active = true } in
  let json = Conftest.yojson_of_user user in
  printf "%s\n" (Yojson.Safe.to_string json);
  [%expect {| {"id":1,"name":"Alice","active":true} |}]

let%expect_test "user - deserializes from JSON correctly" =
  let json =
    `Assoc [ ("id", `Int 2); ("name", `String "Bob"); ("active", `Bool false) ]
  in
  let user = Conftest.user_of_yojson json in
  printf "id: %d, name: %s, active: %b\n" user.id user.name user.active;
  [%expect {| id: 2, name: Bob, active: false |}]

(* =============================================================================
   Test: users_db fixture
   ============================================================================= *)

let%expect_test "users_db - contains initial users" =
  let db = Conftest.users_db () in
  printf "count: %d\n" (Map.length db);
  printf "has_user_1: %b\n" (Map.mem db 1);
  printf "has_user_2: %b\n" (Map.mem db 2);
  printf "has_user_3: %b\n" (Map.mem db 3);
  printf "has_user_4: %b\n" (Map.mem db 4);
  [%expect
    {|
    count: 3
    has_user_1: true
    has_user_2: true
    has_user_3: true
    has_user_4: false
    |}]

let%expect_test "users_db - users have correct data" =
  let db = Conftest.users_db () in
  let user1 = Map.find_exn db 1 in
  let user2 = Map.find_exn db 2 in
  let user3 = Map.find_exn db 3 in
  printf "user1: %s, active=%b\n" user1.name user1.active;
  printf "user2: %s, active=%b\n" user2.name user2.active;
  printf "user3: %s, active=%b\n" user3.name user3.active;
  [%expect
    {|
    user1: Alice, active=true
    user2: Bob, active=true
    user3: Charlie, active=false
    |}]

(* =============================================================================
   Test: get_users
   ============================================================================= *)

let%expect_test "get_users - returns sorted list" =
  let db = Conftest.users_db () in
  let users = Conftest.get_users db in
  List.iter users ~f:(fun u -> printf "id=%d name=%s\n" u.id u.name);
  [%expect
    {|
    id=1 name=Alice
    id=2 name=Bob
    id=3 name=Charlie
    |}]

(* =============================================================================
   Test: search_users
   ============================================================================= *)

let%expect_test "search_users - no filters returns all" =
  let db = Conftest.users_db () in
  let users = Conftest.search_users ~db () in
  printf "count: %d\n" (List.length users);
  [%expect {| count: 3 |}]

let%expect_test "search_users - filter by name" =
  let db = Conftest.users_db () in
  let users = Conftest.search_users ~db ~name:"alice" () in
  printf "count: %d\n" (List.length users);
  printf "name: %s\n" (List.hd_exn users).name;
  [%expect {|
    count: 1
    name: Alice
    |}]

let%expect_test "search_users - filter by active" =
  let db = Conftest.users_db () in
  let active_users = Conftest.search_users ~db ~active:true () in
  let inactive_users = Conftest.search_users ~db ~active:false () in
  printf "active_count: %d\n" (List.length active_users);
  printf "inactive_count: %d\n" (List.length inactive_users);
  [%expect {|
    active_count: 2
    inactive_count: 1
    |}]

let%expect_test "search_users - filter by min_id" =
  let db = Conftest.users_db () in
  let users = Conftest.search_users ~db ~min_id:2 () in
  printf "count: %d\n" (List.length users);
  List.iter users ~f:(fun u -> printf "id=%d\n" u.id);
  [%expect {|
    count: 2
    id=2
    id=3
    |}]

let%expect_test "search_users - multiple filters" =
  let db = Conftest.users_db () in
  let users = Conftest.search_users ~db ~active:true ~min_id:2 () in
  printf "count: %d\n" (List.length users);
  List.iter users ~f:(fun u -> printf "id=%d name=%s\n" u.id u.name);
  [%expect {|
    count: 1
    id=2 name=Bob
    |}]

(* =============================================================================
   Test: get_user
   ============================================================================= *)

let%expect_test "get_user - returns user when exists" =
  let db = Conftest.users_db () in
  let user = Conftest.get_user ~db 1 in
  printf "found: %b\n" (Option.is_some user);
  printf "name: %s\n" (Option.value_map user ~default:"" ~f:(fun u -> u.name));
  [%expect {|
    found: true
    name: Alice
    |}]

let%expect_test "get_user - returns None when not exists" =
  let db = Conftest.users_db () in
  let user = Conftest.get_user ~db 999 in
  printf "found: %b\n" (Option.is_some user);
  [%expect {| found: false |}]

(* =============================================================================
   Test: get_user_active_state
   ============================================================================= *)

let%expect_test "get_user_active_state - returns user when active matches" =
  let db = Conftest.users_db () in
  let user = Conftest.get_user_active_state ~db ~user_id:1 ~is_active:true in
  printf "found: %b\n" (Option.is_some user);
  printf "name: %s\n" (Option.value_map user ~default:"" ~f:(fun u -> u.name));
  [%expect {|
    found: true
    name: Alice
    |}]

let%expect_test "get_user_active_state - returns None when active doesn't match"
    =
  let db = Conftest.users_db () in
  let user = Conftest.get_user_active_state ~db ~user_id:1 ~is_active:false in
  printf "found: %b\n" (Option.is_some user);
  [%expect {| found: false |}]

(* =============================================================================
   Test: create_user
   ============================================================================= *)

let%expect_test "create_user - creates new user with next id" =
  let db = Conftest.users_db () in
  let new_user : Conftest.user_create = { name = "David"; active = false } in
  let new_db, created = Conftest.create_user ~db new_user in
  printf "new_id: %d\n" created.id;
  printf "new_name: %s\n" created.name;
  printf "new_active: %b\n" created.active;
  printf "db_size: %d\n" (Map.length new_db);
  [%expect
    {|
    new_id: 4
    new_name: David
    new_active: false
    db_size: 4
    |}]

(* =============================================================================
   Test: update_user_name
   ============================================================================= *)

let%expect_test "update_user_name - updates existing user" =
  let db = Conftest.users_db () in
  let result = Conftest.update_user_name ~db ~user_id:1 ~name:"XYZ" in
  (match result with
  | Some (new_db, updated) ->
    printf "updated: true\n";
    printf "new_name: %s\n" updated.name;
    printf "in_db: %b\n"
      (Option.value_map (Map.find new_db 1) ~default:false ~f:(fun u ->
           String.equal u.name "XYZ"))
  | None -> printf "updated: false\n");
  [%expect {|
    updated: true
    new_name: XYZ
    in_db: true
    |}]

let%expect_test "update_user_name - returns None for non-existent user" =
  let db = Conftest.users_db () in
  let result = Conftest.update_user_name ~db ~user_id:999 ~name:"Test" in
  printf "updated: %b\n" (Option.is_some result);
  [%expect {| updated: false |}]

(* =============================================================================
   Test: mock_openapi_spec
   ============================================================================= *)

let%expect_test "mock_openapi_spec - has correct structure" =
  let spec = Conftest.mock_openapi_spec () in
  let get_string key obj =
    match obj with
    | `Assoc fields -> (
      match List.Assoc.find fields ~equal:String.equal key with
      | Some (`String s) -> s
      | _ -> "missing")
    | _ -> "not_object"
  in
  let info =
    match spec with
    | `Assoc fields -> List.Assoc.find_exn fields ~equal:String.equal "info"
    | _ -> `Null
  in
  printf "openapi: %s\n" (get_string "openapi" spec);
  printf "title: %s\n" (get_string "title" info);
  printf "version: %s\n" (get_string "version" info);
  [%expect
    {|
    openapi: 3.0.0
    title: Test FastAPI App
    version: 1.0.0
    |}]

let%expect_test "mock_openapi_spec - has paths" =
  let spec = Conftest.mock_openapi_spec () in
  let paths =
    match spec with
    | `Assoc fields -> (
      match List.Assoc.find fields ~equal:String.equal "paths" with
      | Some (`Assoc p) -> List.map p ~f:fst
      | _ -> [])
    | _ -> []
  in
  printf "path_count: %d\n" (List.length paths);
  List.iter paths ~f:(fun p -> printf "path: %s\n" p);
  [%expect
    {|
    path_count: 4
    path: /users
    path: /users/{user_id}
    path: /search
    path: /ping
    |}]
