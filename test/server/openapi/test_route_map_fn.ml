(** Tests for route_map_fn and component_fn functionality in OxFastMCPOpenAPI.

    Translated from Python test_route_map_fn.py to OCaml. Tests custom route
    mapping functions and component customization.

    Note: Python tests use full server integration. These OCaml tests focus on
    the routing logic and component customization. *)

open! Core
open! Expect_test_helpers_core

(* =============================================================================
   Types for Route Mapping
   ============================================================================= *)

type mcp_type = Tool | Resource | ResourceTemplate | Exclude
[@@deriving sexp, equal]

let mcp_type_to_string = function
  | Tool -> "tool"
  | Resource -> "resource"
  | ResourceTemplate -> "resource_template"
  | Exclude -> "exclude"

type http_route = {
  path : string;
  method_ : string;
  operation_id : string;
  summary : string option;
  tags : string list;
}
[@@deriving sexp]

type route_map = {
  methods : string list;
  pattern : string option;
  mcp_type : mcp_type;
}

(* =============================================================================
   Route Mapping Logic
   ============================================================================= *)

(** Determine MCP type for a route using default logic *)
let default_mcp_type (route : http_route) : mcp_type =
  match String.uppercase route.method_ with
  | "GET" ->
    if String.is_substring route.path ~substring:"{" then ResourceTemplate
    else Resource
  | "POST" | "PUT" | "DELETE" | "PATCH" -> Tool
  | _ -> Exclude

(** Check if route matches a route map pattern *)
let route_matches_map (route : http_route) (map : route_map) : bool =
  let method_matches =
    List.mem map.methods (String.uppercase route.method_) ~equal:String.equal
    || List.mem map.methods "*" ~equal:String.equal
  in
  let pattern_matches =
    match map.pattern with
    | None -> true
    | Some pattern ->
      let regex = Re.Pcre.regexp pattern in
      Re.execp regex route.path
  in
  method_matches && pattern_matches

(** Apply route maps to determine MCP type *)
let apply_route_maps (route : http_route) (maps : route_map list) : mcp_type =
  match List.find maps ~f:(route_matches_map route) with
  | Some map -> map.mcp_type
  | None -> default_mcp_type route

type route_map_fn = http_route -> mcp_type -> mcp_type option
(** Route map function type *)

(** Apply route_map_fn to potentially override MCP type *)
let apply_route_map_fn ~(route_map_fn : route_map_fn option)
    (route : http_route) (initial_type : mcp_type) : mcp_type =
  match route_map_fn with
  | None -> initial_type
  | Some fn -> (
    match fn route initial_type with
    | Some new_type -> new_type
    | None -> initial_type)

(* =============================================================================
   Tests: Default Route Mapping
   ============================================================================= *)

let%expect_test "default mapping - GET without path param is Resource" =
  let route =
    {
      path = "/users";
      method_ = "GET";
      operation_id = "listUsers";
      summary = Some "List users";
      tags = [];
    }
  in
  printf "type: %s\n" (mcp_type_to_string (default_mcp_type route));
  [%expect {| type: resource |}]

let%expect_test "default mapping - GET with path param is ResourceTemplate" =
  let route =
    {
      path = "/users/{id}";
      method_ = "GET";
      operation_id = "getUserById";
      summary = Some "Get user by ID";
      tags = [];
    }
  in
  printf "type: %s\n" (mcp_type_to_string (default_mcp_type route));
  [%expect {| type: resource_template |}]

let%expect_test "default mapping - POST is Tool" =
  let route =
    {
      path = "/users";
      method_ = "POST";
      operation_id = "createUser";
      summary = Some "Create user";
      tags = [];
    }
  in
  printf "type: %s\n" (mcp_type_to_string (default_mcp_type route));
  [%expect {| type: tool |}]

(* =============================================================================
   Tests: Route Map Pattern Matching
   ============================================================================= *)

let%expect_test "route matches map - method matching" =
  let route =
    {
      path = "/admin/settings";
      method_ = "GET";
      operation_id = "getSettings";
      summary = None;
      tags = [];
    }
  in
  let map_get = { methods = [ "GET" ]; pattern = None; mcp_type = Resource } in
  let map_post = { methods = [ "POST" ]; pattern = None; mcp_type = Tool } in
  let map_all = { methods = [ "*" ]; pattern = None; mcp_type = Tool } in
  printf "matches GET: %b\n" (route_matches_map route map_get);
  printf "matches POST: %b\n" (route_matches_map route map_post);
  printf "matches *: %b\n" (route_matches_map route map_all);
  [%expect
    {|
    matches GET: true
    matches POST: false
    matches *: true
    |}]

let%expect_test "route matches map - pattern matching" =
  let route =
    {
      path = "/admin/settings";
      method_ = "GET";
      operation_id = "getSettings";
      summary = None;
      tags = [];
    }
  in
  let map_admin =
    { methods = [ "GET" ]; pattern = Some ".*/admin/.*"; mcp_type = Tool }
  in
  let map_users =
    { methods = [ "GET" ]; pattern = Some ".*/users/.*"; mcp_type = Tool }
  in
  printf "matches admin pattern: %b\n" (route_matches_map route map_admin);
  printf "matches users pattern: %b\n" (route_matches_map route map_users);
  [%expect
    {|
    matches admin pattern: true
    matches users pattern: false
    |}]

let%expect_test "apply route maps - first match wins" =
  let route =
    {
      path = "/admin/settings";
      method_ = "GET";
      operation_id = "getSettings";
      summary = None;
      tags = [];
    }
  in
  let maps =
    [
      {
        methods = [ "GET"; "POST" ];
        pattern = Some ".*/admin/.*";
        mcp_type = Exclude;
      };
      { methods = [ "GET" ]; pattern = None; mcp_type = Resource };
    ]
  in
  let result = apply_route_maps route maps in
  printf "type: %s\n" (mcp_type_to_string result);
  [%expect {| type: exclude |}]

(* =============================================================================
   Tests: Custom Route Map Function
   ============================================================================= *)

let%expect_test "route_map_fn can convert route types" =
  let route =
    {
      path = "/admin/settings";
      method_ = "GET";
      operation_id = "getAdminSettings";
      summary = None;
      tags = [];
    }
  in
  (* Custom function that converts admin routes to tools *)
  let admin_routes_to_tools r _ =
    if String.is_substring r.path ~substring:"/admin/" then Some Tool else None
  in
  let initial_type = default_mcp_type route in
  let final_type =
    apply_route_map_fn ~route_map_fn:(Some admin_routes_to_tools) route
      initial_type
  in
  printf "initial: %s\n" (mcp_type_to_string initial_type);
  printf "final: %s\n" (mcp_type_to_string final_type);
  [%expect {|
    initial: resource
    final: tool
    |}]

let%expect_test "route_map_fn returning None uses default" =
  let route =
    {
      path = "/users";
      method_ = "GET";
      operation_id = "listUsers";
      summary = None;
      tags = [];
    }
  in
  let always_none _ _ = None in
  let initial_type = default_mcp_type route in
  let final_type =
    apply_route_map_fn ~route_map_fn:(Some always_none) route initial_type
  in
  printf "initial: %s\n" (mcp_type_to_string initial_type);
  printf "final: %s\n" (mcp_type_to_string final_type);
  printf "unchanged: %b\n" (equal_mcp_type initial_type final_type);
  [%expect
    {|
    initial: resource
    final: resource
    unchanged: true
    |}]

let%expect_test "route_map_fn can rescue excluded routes" =
  let routes =
    [
      {
        path = "/admin/settings";
        method_ = "GET";
        operation_id = "getAdminSettings";
        summary = None;
        tags = [];
      };
      {
        path = "/admin/settings";
        method_ = "POST";
        operation_id = "updateAdminSettings";
        summary = None;
        tags = [];
      };
      {
        path = "/users";
        method_ = "GET";
        operation_id = "listUsers";
        summary = None;
        tags = [];
      };
    ]
  in
  (* Exclude all admin routes *)
  let maps =
    [
      {
        methods = [ "GET"; "POST" ];
        pattern = Some ".*/admin/.*";
        mcp_type = Exclude;
      };
    ]
  in
  (* But rescue admin GET routes *)
  let rescue_admin_get r _mcp_type =
    if String.equal r.path "/admin/settings" && String.equal r.method_ "GET"
    then Some Tool
    else None
  in
  printf "Route processing:\n";
  List.iter routes ~f:(fun r ->
      let map_type = apply_route_maps r maps in
      let final_type =
        apply_route_map_fn ~route_map_fn:(Some rescue_admin_get) r map_type
      in
      printf "  %s %s: %s -> %s\n" r.method_ r.path
        (mcp_type_to_string map_type)
        (mcp_type_to_string final_type));
  [%expect
    {|
    Route processing:
      GET /admin/settings: exclude -> tool
      POST /admin/settings: exclude -> exclude
      GET /users: resource -> resource
    |}]

(* =============================================================================
   Tests: Component Customization
   ============================================================================= *)

type component = {
  name : string;
  description : string option;
  tags : (string, String.comparator_witness) Set.t;
  component_type : mcp_type;
}

type component_fn = http_route -> component -> component
(** Component function type *)

(** Apply component_fn to customize a component *)
let apply_component_fn ~(component_fn : component_fn option)
    (route : http_route) (component : component) : component =
  match component_fn with
  | None -> component
  | Some fn -> fn route component

let%expect_test "component_fn can add tags" =
  let route =
    {
      path = "/admin/settings";
      method_ = "GET";
      operation_id = "getAdminSettings";
      summary = None;
      tags = [];
    }
  in
  let component =
    {
      name = "getAdminSettings";
      description = Some "Get admin settings";
      tags = Set.empty (module String);
      component_type = Tool;
    }
  in
  let add_admin_tag r c =
    if String.is_substring r.path ~substring:"/admin/" then
      { c with tags = Set.add c.tags "admin" }
    else c
  in
  let updated =
    apply_component_fn ~component_fn:(Some add_admin_tag) route component
  in
  printf "has admin tag: %b\n" (Set.mem updated.tags "admin");
  printf "tag count: %d\n" (Set.length updated.tags);
  [%expect {|
    has admin tag: true
    tag count: 1
    |}]

let%expect_test "component_fn can modify description" =
  let route =
    {
      path = "/users";
      method_ = "POST";
      operation_id = "createUser";
      summary = None;
      tags = [];
    }
  in
  let component =
    {
      name = "createUser";
      description = Some "Create user";
      tags = Set.empty (module String);
      component_type = Tool;
    }
  in
  let add_suffix _ c =
    {
      c with
      description =
        Some (Option.value c.description ~default:"" ^ " [CUSTOMIZED]");
    }
  in
  let updated =
    apply_component_fn ~component_fn:(Some add_suffix) route component
  in
  printf "description: %s\n" (Option.value updated.description ~default:"None");
  printf "has suffix: %b\n"
    (String.is_substring
       (Option.value updated.description ~default:"")
       ~substring:"[CUSTOMIZED]");
  [%expect
    {|
    description: Create user [CUSTOMIZED]
    has suffix: true
    |}]

(* =============================================================================
   Tests: Combined Route Mapping and Component Customization
   ============================================================================= *)

let%expect_test "combined route_map_fn and component_fn" =
  let routes =
    [
      {
        path = "/admin/settings";
        method_ = "GET";
        operation_id = "getAdminSettings";
        summary = None;
        tags = [];
      };
      {
        path = "/users";
        method_ = "GET";
        operation_id = "listUsers";
        summary = None;
        tags = [];
      };
    ]
  in
  (* Convert admin GET to Tool *)
  let route_mapper r _ =
    if String.is_substring r.path ~substring:"/admin/" then Some Tool else None
  in
  (* Add tags based on path *)
  let component_customizer r c =
    if String.is_substring r.path ~substring:"/admin/" then
      { c with tags = Set.add c.tags "admin" }
    else { c with tags = Set.add c.tags "public" }
  in
  printf "Processing routes:\n";
  List.iter routes ~f:(fun r ->
      let initial_type = default_mcp_type r in
      let final_type =
        apply_route_map_fn ~route_map_fn:(Some route_mapper) r initial_type
      in
      let component =
        {
          name = r.operation_id;
          description = None;
          tags = Set.empty (module String);
          component_type = final_type;
        }
      in
      let updated =
        apply_component_fn ~component_fn:(Some component_customizer) r component
      in
      printf "  %s: type=%s, tags=[%s]\n" r.operation_id
        (mcp_type_to_string updated.component_type)
        (Set.to_list updated.tags |> String.concat ~sep:", "));
  [%expect
    {|
    Processing routes:
      getAdminSettings: type=tool, tags=[admin]
      listUsers: type=resource, tags=[public]
    |}]

(* =============================================================================
   Tests: Component Name Modification
   ============================================================================= *)

let%expect_test "component_fn can modify name" =
  let route =
    {
      path = "/users";
      method_ = "GET";
      operation_id = "getUserById";
      summary = None;
      tags = [];
    }
  in
  let component =
    {
      name = "getUserById";
      description = None;
      tags = Set.empty (module String);
      component_type = Resource;
    }
  in
  let add_prefix _ c =
    if String.is_prefix c.name ~prefix:"get" then
      { c with name = "v1_removed_" ^ c.name }
    else c
  in
  let updated =
    apply_component_fn ~component_fn:(Some add_prefix) route component
  in
  printf "original name: %s\n" component.name;
  printf "updated name: %s\n" updated.name;
  printf "has prefix: %b\n"
    (String.is_prefix updated.name ~prefix:"v1_removed_");
  [%expect
    {|
    original name: getUserById
    updated name: v1_removed_getUserById
    has prefix: true
    |}]

(* =============================================================================
   Tests: Error Handling in Functions
   ============================================================================= *)

(** Safe wrapper for route_map_fn that catches exceptions *)
let safe_route_map_fn ~(route_map_fn : route_map_fn) (route : http_route)
    (initial_type : mcp_type) : mcp_type =
  try
    match route_map_fn route initial_type with
    | Some t -> t
    | None -> initial_type
  with _ -> initial_type

let%expect_test "error handling - route_map_fn exception caught" =
  let route =
    {
      path = "/users";
      method_ = "GET";
      operation_id = "listUsers";
      summary = None;
      tags = [];
    }
  in
  let error_fn r _ =
    if String.equal r.path "/users" then failwith "Test error" else None
  in
  let initial_type = default_mcp_type route in
  let final_type =
    safe_route_map_fn ~route_map_fn:error_fn route initial_type
  in
  printf "error caught: %b\n" (equal_mcp_type final_type initial_type);
  printf "type: %s\n" (mcp_type_to_string final_type);
  [%expect {|
    error caught: true
    type: resource
    |}]
