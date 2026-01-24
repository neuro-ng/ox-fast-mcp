(** Test fixtures for OpenAPI tests.

    Translated from Python conftest.py to OCaml. Provides reusable test data and
    mock objects for OpenAPI testing.

    OCaml Note: pytest fixtures are translated to module-level functions and
    values that can be reused across tests. *)

open! Core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* =============================================================================
   MCP Types (from openapi module - defined locally since not exported)
   ============================================================================= *)

(** MCP component type *)
module MCPType = struct
  type t = Tool | Resource | ResourceTemplate | Exclude
  [@@deriving sexp, compare, equal]

  let to_string = function
    | Tool -> "tool"
    | Resource -> "resource"
    | ResourceTemplate -> "resource_template"
    | Exclude -> "exclude"

  let of_string = function
    | "tool" -> Some Tool
    | "resource" -> Some Resource
    | "resource_template" -> Some ResourceTemplate
    | "exclude" -> Some Exclude
    | _ -> None
end

type route_map = {
  methods : string list;
  pattern : string;
  mcp_type : MCPType.t;
  tags : string list;
  mcp_tags : string list;
}
[@@deriving sexp, compare]
(** Route mapping configuration *)

(* =============================================================================
   User Types (translated from Python Pydantic models)
   ============================================================================= *)

type user = { id : int; name : string; active : bool }
[@@deriving sexp, compare, yojson]
(** User record type *)

type user_create = { name : string; active : bool }
[@@deriving sexp, compare, yojson]
(** User creation record type *)

(* =============================================================================
   Test Data
   ============================================================================= *)

(** Initial users database for testing *)
let users_db () : user Int.Map.t =
  Int.Map.of_alist_exn
    [
      (1, { id = 1; name = "Alice"; active = true });
      (2, { id = 2; name = "Bob"; active = true });
      (3, { id = 3; name = "Charlie"; active = false });
    ]

(** Get users as list sorted by id *)
let get_users (db : user Int.Map.t) =
  Map.data db |> List.sort ~compare:(fun u1 u2 -> Int.compare u1.id u2.id)

(** Search users with optional filters *)
let search_users ~(db : user Int.Map.t) ?name ?active ?min_id () =
  let results = get_users db in
  let results =
    match name with
    | Some n ->
      List.filter results ~f:(fun u ->
          String.is_substring ~substring:(String.lowercase n)
            (String.lowercase u.name))
    | None -> results
  in
  let results =
    match active with
    | Some a -> List.filter results ~f:(fun u -> Bool.equal u.active a)
    | None -> results
  in
  let results =
    match min_id with
    | Some min -> List.filter results ~f:(fun u -> u.id >= min)
    | None -> results
  in
  List.sort results ~compare:(fun u1 u2 -> Int.compare u1.id u2.id)

(** Get a user by ID *)
let get_user ~(db : user Int.Map.t) user_id = Map.find db user_id

(** Get user by ID filtered by active state *)
let get_user_active_state ~(db : user Int.Map.t) ~user_id ~is_active =
  match Map.find db user_id with
  | Some user when Bool.equal user.active is_active -> Some user
  | _ -> None

(** Create a new user *)
let create_user ~(db : user Int.Map.t) (user : user_create) :
    user Int.Map.t * user =
  let max_id =
    Map.fold db ~init:0 ~f:(fun ~key ~data:_ acc -> Int.max key acc)
  in
  let user_id = max_id + 1 in
  let new_user = { id = user_id; name = user.name; active = user.active } in
  let new_db = Map.set db ~key:user_id ~data:new_user in
  (new_db, new_user)

(** Update a user's name *)
let update_user_name ~(db : user Int.Map.t) ~user_id ~name =
  match Map.find db user_id with
  | Some user ->
    let updated_user = { user with name } in
    let new_db = Map.set db ~key:user_id ~data:updated_user in
    Some (new_db, updated_user)
  | None -> None

(* =============================================================================
   Route Maps for GET Requests
   ============================================================================= *)

(** Route maps for GET requests. Use these to create components of all types
    instead of just tools. *)
let get_route_maps : route_map list =
  [
    (* GET requests with path parameters go to ResourceTemplate *)
    {
      methods = [ "GET" ];
      pattern = ".*\\{.*\\}.*";
      mcp_type = MCPType.ResourceTemplate;
      tags = [];
      mcp_tags = [];
    };
    (* GET requests without path parameters go to Resource *)
    {
      methods = [ "GET" ];
      pattern = ".*";
      mcp_type = MCPType.Resource;
      tags = [];
      mcp_tags = [];
    };
  ]

(* =============================================================================
   Mock OpenAPI Spec
   ============================================================================= *)

(** Create a mock OpenAPI spec for testing *)
let mock_openapi_spec () : Yojson.Safe.t =
  `Assoc
    [
      ("openapi", `String "3.0.0");
      ( "info",
        `Assoc
          [
            ("title", `String "Test FastAPI App"); ("version", `String "1.0.0");
          ] );
      ( "paths",
        `Assoc
          [
            ( "/users",
              `Assoc
                [
                  ( "get",
                    `Assoc
                      [
                        ("summary", `String "Get all users");
                        ("operationId", `String "get_users");
                        ("tags", `List [ `String "users"; `String "list" ]);
                      ] );
                  ( "post",
                    `Assoc
                      [
                        ("summary", `String "Create a new user");
                        ("operationId", `String "create_user");
                        ("tags", `List [ `String "users"; `String "create" ]);
                      ] );
                ] );
            ( "/users/{user_id}",
              `Assoc
                [
                  ( "get",
                    `Assoc
                      [
                        ("summary", `String "Get a user by ID");
                        ("operationId", `String "get_user");
                        ("tags", `List [ `String "users"; `String "detail" ]);
                        ( "parameters",
                          `List
                            [
                              `Assoc
                                [
                                  ("name", `String "user_id");
                                  ("in", `String "path");
                                  ("required", `Bool true);
                                  ( "schema",
                                    `Assoc [ ("type", `String "integer") ] );
                                ];
                            ] );
                      ] );
                ] );
            ( "/search",
              `Assoc
                [
                  ( "get",
                    `Assoc
                      [
                        ("summary", `String "Search users with filters");
                        ("operationId", `String "search_users");
                        ("tags", `List [ `String "search" ]);
                        ( "parameters",
                          `List
                            [
                              `Assoc
                                [
                                  ("name", `String "name");
                                  ("in", `String "query");
                                  ("required", `Bool false);
                                  ( "schema",
                                    `Assoc [ ("type", `String "string") ] );
                                ];
                              `Assoc
                                [
                                  ("name", `String "active");
                                  ("in", `String "query");
                                  ("required", `Bool false);
                                  ( "schema",
                                    `Assoc [ ("type", `String "boolean") ] );
                                ];
                              `Assoc
                                [
                                  ("name", `String "min_id");
                                  ("in", `String "query");
                                  ("required", `Bool false);
                                  ( "schema",
                                    `Assoc [ ("type", `String "integer") ] );
                                ];
                            ] );
                      ] );
                ] );
            ( "/ping",
              `Assoc
                [
                  ( "get",
                    `Assoc
                      [
                        ("summary", `String "Ping the server");
                        ("operationId", `String "ping");
                        ("responses", `Assoc [ ("200", `Assoc []) ]);
                      ] );
                ] );
          ] );
    ]

(** Convert a user to JSON *)
let user_to_json (user : user) : Yojson.Safe.t = yojson_of_user user

(** Convert a list of users to JSON *)
let users_to_json (users : user list) : Yojson.Safe.t =
  `List (List.map users ~f:user_to_json)
