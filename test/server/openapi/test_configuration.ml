(** Tests for OpenAPI configuration.

    Translated from Python test_configuration.py to OCaml. Tests route map
    wildcard matching, tag filtering, mcp_names, mcp_tags, and global tags.

    Note: Python tests use httpx.AsyncClient which is not available in OCaml.
    These tests focus on unit testing the configuration logic. *)

open! Core
open! Expect_test_helpers_core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
module Conftest = Conftest

(* =============================================================================
   Helper Functions (translated from Python test logic)
   ============================================================================= *)

(** Check if methods matches (supports "*" wildcard) *)
let method_matches ~(route_method : string) ~(mapping_methods : string list) =
  List.mem mapping_methods "*" ~equal:String.equal
  || List.mem mapping_methods route_method ~equal:String.equal

(** Check if pattern matches path *)
let pattern_matches ~(path : string) ~(pattern : string) =
  let regex = Re.Pcre.regexp pattern in
  Re.execp regex path

(** Check if route has all required tags (AND condition) *)
let tags_match ~(route_tags : string list) ~(required_tags : string list) =
  if List.is_empty required_tags then true
  else List.for_all required_tags ~f:(List.mem route_tags ~equal:String.equal)

(** Slugify a name - remove special characters and spaces *)
let slugify name =
  name
  |> String.map ~f:(fun c ->
         if Char.is_alphanum c || Char.equal c '_' then c else '_')
  |> String.substr_replace_all ~pattern:"__" ~with_:"_"
  |> String.strip ~drop:(Char.equal '_')

(** Truncate name to max length *)
let truncate_name ?(max_length = 56) name =
  if String.length name <= max_length then name
  else String.prefix name max_length

(** Get short operation ID (up to double underscore) *)
let get_short_operation_id op_id =
  match String.substr_index op_id ~pattern:"__" with
  | Some idx -> String.prefix op_id idx
  | None -> op_id

(** Combine tags from multiple sources *)
let combine_tags ~openapi_tags ~global_tags ~mcp_tags =
  List.concat [ openapi_tags; global_tags; mcp_tags ]
  |> List.dedup_and_sort ~compare:String.compare

(* =============================================================================
   Test: Route Map Wildcard (TestRouteMapWildcard)
   ============================================================================= *)

let%expect_test "wildcard method - matches all HTTP methods" =
  let methods_wildcard = [ "*" ] in
  printf "GET matches: %b\n"
    (method_matches ~route_method:"GET" ~mapping_methods:methods_wildcard);
  printf "POST matches: %b\n"
    (method_matches ~route_method:"POST" ~mapping_methods:methods_wildcard);
  printf "PUT matches: %b\n"
    (method_matches ~route_method:"PUT" ~mapping_methods:methods_wildcard);
  printf "DELETE matches: %b\n"
    (method_matches ~route_method:"DELETE" ~mapping_methods:methods_wildcard);
  printf "PATCH matches: %b\n"
    (method_matches ~route_method:"PATCH" ~mapping_methods:methods_wildcard);
  [%expect
    {|
    GET matches: true
    POST matches: true
    PUT matches: true
    DELETE matches: true
    PATCH matches: true
    |}]

let%expect_test "specific methods - only matches listed methods" =
  let methods_get_only = [ "GET" ] in
  let methods_post_put = [ "POST"; "PUT" ] in
  printf "GET on [GET]: %b\n"
    (method_matches ~route_method:"GET" ~mapping_methods:methods_get_only);
  printf "POST on [GET]: %b\n"
    (method_matches ~route_method:"POST" ~mapping_methods:methods_get_only);
  printf "POST on [POST,PUT]: %b\n"
    (method_matches ~route_method:"POST" ~mapping_methods:methods_post_put);
  printf "GET on [POST,PUT]: %b\n"
    (method_matches ~route_method:"GET" ~mapping_methods:methods_post_put);
  [%expect
    {|
    GET on [GET]: true
    POST on [GET]: false
    POST on [POST,PUT]: true
    GET on [POST,PUT]: false
    |}]

let%expect_test "wildcard pattern - matches all paths" =
  let pattern = ".*" in
  printf "/users: %b\n" (pattern_matches ~path:"/users" ~pattern);
  printf "/posts: %b\n" (pattern_matches ~path:"/posts" ~pattern);
  printf "/admin/stats: %b\n" (pattern_matches ~path:"/admin/stats" ~pattern);
  [%expect {|
    /users: true
    /posts: true
    /admin/stats: true
    |}]

(* =============================================================================
   Test: Route Map Tags (TestRouteMapTags)
   ============================================================================= *)

let%expect_test "tags_match - single tag required" =
  let route_tags = [ "users"; "public" ] in
  printf "has admin: %b\n" (tags_match ~route_tags ~required_tags:[ "admin" ]);
  printf "has users: %b\n" (tags_match ~route_tags ~required_tags:[ "users" ]);
  printf "has public: %b\n" (tags_match ~route_tags ~required_tags:[ "public" ]);
  [%expect
    {|
    has admin: false
    has users: true
    has public: true
    |}]

let%expect_test "tags_match - multiple tags AND condition" =
  let route_tags_users_admin = [ "users"; "admin" ] in
  let route_tags_users_public = [ "users"; "public" ] in
  let required = [ "users"; "admin" ] in
  printf "users+admin has both: %b\n"
    (tags_match ~route_tags:route_tags_users_admin ~required_tags:required);
  printf "users+public has both: %b\n"
    (tags_match ~route_tags:route_tags_users_public ~required_tags:required);
  [%expect
    {|
    users+admin has both: true
    users+public has both: false
    |}]

let%expect_test "tags_match - empty required tags matches all" =
  let route_tags = [ "users"; "public" ] in
  let empty_route = [] in
  printf "route with tags: %b\n" (tags_match ~route_tags ~required_tags:[]);
  printf "route without tags: %b\n"
    (tags_match ~route_tags:empty_route ~required_tags:[]);
  [%expect {|
    route with tags: true
    route without tags: true
    |}]

let%expect_test "route matching - pattern and tags combination" =
  (* Route must match both pattern AND have required tags *)
  let matches_route ~path ~route_tags ~pattern ~required_tags =
    pattern_matches ~path ~pattern && tags_match ~route_tags ~required_tags
  in
  (* /admin/stats has admin tag and matches /admin/ pattern *)
  printf "/admin/stats with admin tag: %b\n"
    (matches_route ~path:"/admin/stats" ~route_tags:[ "admin"; "internal" ]
       ~pattern:".*/admin/.*" ~required_tags:[ "admin" ]);
  (* /users has admin tag but doesn't match /admin/ pattern *)
  printf "/users with admin tag: %b\n"
    (matches_route ~path:"/users" ~route_tags:[ "admin" ] ~pattern:".*/admin/.*"
       ~required_tags:[ "admin" ]);
  (* /admin/stats doesn't have users tag *)
  printf "/admin/stats with users required: %b\n"
    (matches_route ~path:"/admin/stats" ~route_tags:[ "admin" ]
       ~pattern:".*/admin/.*" ~required_tags:[ "users" ]);
  [%expect
    {|
    /admin/stats with admin tag: true
    /users with admin tag: false
    /admin/stats with users required: false
    |}]

(* =============================================================================
   Test: MCP Names (TestMCPNames)
   ============================================================================= *)

let%expect_test "mcp_names - custom mapping overrides operationId" =
  let mcp_names =
    [
      ("list_users__with_pagination", "user_list");
      ("create_user_admin__special_permissions", "admin_create_user");
      ("get_user_by_id__admin_only", "user_detail");
    ]
  in
  let get_name op_id =
    match List.Assoc.find mcp_names ~equal:String.equal op_id with
    | Some custom -> custom
    | None -> get_short_operation_id op_id
  in
  printf "list_users__with_pagination: %s\n"
    (get_name "list_users__with_pagination");
  printf "create_user_admin__special_permissions: %s\n"
    (get_name "create_user_admin__special_permissions");
  printf "unmapped_operation__suffix: %s\n"
    (get_name "unmapped_operation__suffix");
  [%expect
    {|
    list_users__with_pagination: user_list
    create_user_admin__special_permissions: admin_create_user
    unmapped_operation__suffix: unmapped_operation
    |}]

let%expect_test "get_short_operation_id - truncates at __" =
  printf "list_users__with_pagination: %s\n"
    (get_short_operation_id "list_users__with_pagination");
  printf "get_user_by_id__admin_only: %s\n"
    (get_short_operation_id "get_user_by_id__admin_only");
  printf "simple_name: %s\n" (get_short_operation_id "simple_name");
  printf "name_with_underscore: %s\n"
    (get_short_operation_id "name_with_underscore");
  [%expect
    {|
    list_users__with_pagination: list_users
    get_user_by_id__admin_only: get_user_by_id
    simple_name: simple_name
    name_with_underscore: name_with_underscore
    |}]

let%expect_test "slugify - removes special characters" =
  printf "special-chars@and#spaces: %s\n" (slugify "special-chars@and#spaces");
  printf "with spaces: %s\n" (slugify "with spaces");
  printf "With$Dollar%%Percent: %s\n" (slugify "With$Dollar%Percent");
  printf "normal_name: %s\n" (slugify "normal_name");
  [%expect
    {|
    special-chars@and#spaces: special_chars_and_spaces
    with spaces: with_spaces
    With$Dollar%Percent: With_Dollar_Percent
    normal_name: normal_name
    |}]

let%expect_test "truncate_name - limits to 56 characters" =
  let long_name =
    "this_is_a_very_long_operation_id_that_exceeds_fifty_six_characters_and_should_be_truncated"
  in
  let short_name = "short_name" in
  let truncated = truncate_name long_name in
  let kept = truncate_name short_name in
  printf "long name length: %d\n" (String.length long_name);
  printf "truncated length: %d\n" (String.length truncated);
  printf "truncated <= 56: %b\n" (String.length truncated <= 56);
  printf "short name kept: %b\n" (String.equal kept short_name);
  [%expect
    {|
    long name length: 90
    truncated length: 56
    truncated <= 56: true
    short name kept: true
    |}]

let%expect_test "truncate_name - custom names also truncated" =
  let very_long_custom =
    "this_is_a_very_long_custom_name_that_exceeds_fifty_six_characters_and_should_be_truncated"
  in
  let truncated = truncate_name very_long_custom in
  printf "truncated: %s\n" truncated;
  printf "length: %d\n" (String.length truncated);
  printf "is_exactly_56: %b\n" (String.length truncated = 56);
  [%expect
    {|
    truncated: this_is_a_very_long_custom_name_that_exceeds_fifty_six_c
    length: 56
    is_exactly_56: true
    |}]

(* =============================================================================
   Test: Route Map MCP Tags (TestRouteMapMCPTags)
   ============================================================================= *)

let%expect_test "mcp_tags - added to component tags" =
  let openapi_tags = [ "users" ] in
  let mcp_tags = [ "custom"; "api-write" ] in
  let combined = combine_tags ~openapi_tags ~global_tags:[] ~mcp_tags in
  printf "combined tags: %s\n" (String.concat ~sep:", " combined);
  printf "has users: %b\n" (List.mem combined "users" ~equal:String.equal);
  printf "has custom: %b\n" (List.mem combined "custom" ~equal:String.equal);
  printf "has api-write: %b\n"
    (List.mem combined "api-write" ~equal:String.equal);
  [%expect
    {|
    combined tags: api-write, custom, users
    has users: true
    has custom: true
    has api-write: true
    |}]

let%expect_test "mcp_tags - different route maps add different tags" =
  (* Simulating different route maps applying to different routes *)
  let post_mcp_tags = [ "write-operation"; "mutation" ] in
  let get_detail_mcp_tags = [ "detail"; "single-item" ] in
  let get_list_mcp_tags = [ "list"; "collection" ] in
  let openapi_tags = [ "items" ] in

  let post_combined =
    combine_tags ~openapi_tags ~global_tags:[] ~mcp_tags:post_mcp_tags
  in
  let get_detail_combined =
    combine_tags ~openapi_tags ~global_tags:[] ~mcp_tags:get_detail_mcp_tags
  in
  let get_list_combined =
    combine_tags ~openapi_tags ~global_tags:[] ~mcp_tags:get_list_mcp_tags
  in

  printf "POST tags: %s\n" (String.concat ~sep:", " post_combined);
  printf "GET detail tags: %s\n" (String.concat ~sep:", " get_detail_combined);
  printf "GET list tags: %s\n" (String.concat ~sep:", " get_list_combined);
  [%expect
    {|
    POST tags: items, mutation, write-operation
    GET detail tags: detail, items, single-item
    GET list tags: collection, items, list
    |}]

(* =============================================================================
   Test: Global Tags Parameter (TestGlobalTagsParameter)
   ============================================================================= *)

let%expect_test "global_tags - added to all components" =
  let openapi_tags = [ "items" ] in
  let global_tags = [ "global"; "api-v1" ] in
  let combined = combine_tags ~openapi_tags ~global_tags ~mcp_tags:[] in
  printf "combined: %s\n" (String.concat ~sep:", " combined);
  printf "has items: %b\n" (List.mem combined "items" ~equal:String.equal);
  printf "has global: %b\n" (List.mem combined "global" ~equal:String.equal);
  printf "has api-v1: %b\n" (List.mem combined "api-v1" ~equal:String.equal);
  [%expect
    {|
    combined: api-v1, global, items
    has items: true
    has global: true
    has api-v1: true
    |}]

let%expect_test "global_tags - combine with route map mcp_tags" =
  let openapi_tags = [ "items" ] in
  let global_tags = [ "global" ] in
  let mcp_tags = [ "route-specific" ] in
  let combined = combine_tags ~openapi_tags ~global_tags ~mcp_tags in
  printf "combined: %s\n" (String.concat ~sep:", " combined);
  printf "has items: %b\n" (List.mem combined "items" ~equal:String.equal);
  printf "has global: %b\n" (List.mem combined "global" ~equal:String.equal);
  printf "has route-specific: %b\n"
    (List.mem combined "route-specific" ~equal:String.equal);
  [%expect
    {|
    combined: global, items, route-specific
    has items: true
    has global: true
    has route-specific: true
    |}]

let%expect_test "global_tags - resource without mcp_tags only has openapi and \
                 global" =
  let openapi_tags = [ "items" ] in
  let global_tags = [ "global" ] in
  (* Resource matched by different route map without mcp_tags *)
  let mcp_tags = [] in
  let combined = combine_tags ~openapi_tags ~global_tags ~mcp_tags in
  printf "combined: %s\n" (String.concat ~sep:", " combined);
  printf "count: %d\n" (List.length combined);
  printf "has route-specific: %b\n"
    (List.mem combined "route-specific" ~equal:String.equal);
  [%expect
    {|
    combined: global, items
    count: 2
    has route-specific: false
    |}]

(* =============================================================================
   Test: Route Map Configuration (integration of all matching logic)
   ============================================================================= *)

type route_info = {
  path : string;
  method_ : string;
  tags : string list;
  operation_id : string;
}
[@@deriving sexp]

type route_map_config = {
  methods : string list;
  pattern : string;
  mcp_type : Conftest.MCPType.t;
  required_tags : string list;
  mcp_tags : string list;
}
[@@deriving sexp]

let find_matching_map route maps =
  List.find maps ~f:(fun map ->
      method_matches ~route_method:route.method_ ~mapping_methods:map.methods
      && pattern_matches ~path:route.path ~pattern:map.pattern
      && tags_match ~route_tags:route.tags ~required_tags:map.required_tags)

let%expect_test "route mapping - priority order matters" =
  let maps =
    [
      (* First: admin-tagged routes with /admin/ pattern become tools *)
      {
        methods = [ "*" ];
        pattern = ".*/admin/.*";
        mcp_type = Conftest.MCPType.Tool;
        required_tags = [ "admin" ];
        mcp_tags = [];
      };
      (* Second: GET with path params become resource templates *)
      {
        methods = [ "GET" ];
        pattern = ".*\\{.*\\}.*";
        mcp_type = Conftest.MCPType.ResourceTemplate;
        required_tags = [];
        mcp_tags = [];
      };
      (* Third: Other GET become resources *)
      {
        methods = [ "GET" ];
        pattern = ".*";
        mcp_type = Conftest.MCPType.Resource;
        required_tags = [];
        mcp_tags = [];
      };
      (* Fourth: POST become tools *)
      {
        methods = [ "POST" ];
        pattern = ".*";
        mcp_type = Conftest.MCPType.Tool;
        required_tags = [];
        mcp_tags = [];
      };
    ]
  in
  let routes =
    [
      {
        path = "/admin/stats";
        method_ = "GET";
        tags = [ "admin"; "internal" ];
        operation_id = "getAdminStats";
      };
      {
        path = "/users/{user_id}";
        method_ = "GET";
        tags = [ "users" ];
        operation_id = "getUser";
      };
      {
        path = "/users";
        method_ = "GET";
        tags = [ "users" ];
        operation_id = "getUsers";
      };
      {
        path = "/users";
        method_ = "POST";
        tags = [ "users" ];
        operation_id = "createUser";
      };
    ]
  in
  List.iter routes ~f:(fun route ->
      match find_matching_map route maps with
      | Some map ->
        printf "%s %s -> %s\n" route.method_ route.path
          (Conftest.MCPType.to_string map.mcp_type)
      | None -> printf "%s %s -> no match\n" route.method_ route.path);
  [%expect
    {|
    GET /admin/stats -> tool
    GET /users/{user_id} -> resource_template
    GET /users -> resource
    POST /users -> tool
    |}]

let%expect_test "route mapping - exclude takes precedence" =
  let maps =
    [
      (* Exclude internal routes first *)
      {
        methods = [ "*" ];
        pattern = ".*";
        mcp_type = Conftest.MCPType.Exclude;
        required_tags = [ "internal" ];
        mcp_tags = [];
      };
      (* Then map GET to resources *)
      {
        methods = [ "GET" ];
        pattern = ".*";
        mcp_type = Conftest.MCPType.Resource;
        required_tags = [];
        mcp_tags = [];
      };
    ]
  in
  let internal_route =
    {
      path = "/admin/stats";
      method_ = "GET";
      tags = [ "admin"; "internal" ];
      operation_id = "getAdminStats";
    }
  in
  let public_route =
    {
      path = "/users";
      method_ = "GET";
      tags = [ "users"; "public" ];
      operation_id = "getUsers";
    }
  in
  (match find_matching_map internal_route maps with
  | Some map ->
    printf "internal: %s\n" (Conftest.MCPType.to_string map.mcp_type)
  | None -> printf "internal: no match\n");
  (match find_matching_map public_route maps with
  | Some map -> printf "public: %s\n" (Conftest.MCPType.to_string map.mcp_type)
  | None -> printf "public: no match\n");
  [%expect {|
    internal: exclude
    public: resource
    |}]

(* =============================================================================
   Test: OpenAPI Spec Parsing Helpers
   ============================================================================= *)

let extract_operation_id route_json =
  match route_json with
  | `Assoc fields -> (
    match List.Assoc.find fields ~equal:String.equal "operationId" with
    | Some (`String id) -> Some id
    | _ -> None)
  | _ -> None

let extract_tags route_json =
  match route_json with
  | `Assoc fields -> (
    match List.Assoc.find fields ~equal:String.equal "tags" with
    | Some (`List tags) ->
      List.filter_map tags ~f:(function
        | `String s -> Some s
        | _ -> None)
    | _ -> [])
  | _ -> []

let%expect_test "extract_operation_id - from OpenAPI route" =
  let route =
    `Assoc
      [
        ("operationId", `String "getUsers");
        ("summary", `String "Get all users");
        ("tags", `List [ `String "users" ]);
      ]
  in
  let route_no_id = `Assoc [ ("summary", `String "Get all users") ] in
  printf "with operationId: %s\n"
    (Option.value (extract_operation_id route) ~default:"None");
  printf "without operationId: %s\n"
    (Option.value (extract_operation_id route_no_id) ~default:"None");
  [%expect
    {|
    with operationId: getUsers
    without operationId: None
    |}]

let%expect_test "extract_tags - from OpenAPI route" =
  let route =
    `Assoc
      [
        ("operationId", `String "createUser");
        ("tags", `List [ `String "users"; `String "admin" ]);
      ]
  in
  let route_no_tags = `Assoc [ ("operationId", `String "createUser") ] in
  let tags = extract_tags route in
  let tags_empty = extract_tags route_no_tags in
  printf "tags: %s\n" (String.concat ~sep:", " tags);
  printf "empty tags count: %d\n" (List.length tags_empty);
  [%expect {|
    tags: users, admin
    empty tags count: 0
    |}]
