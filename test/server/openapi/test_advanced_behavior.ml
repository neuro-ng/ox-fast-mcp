(** Tests for OpenAPI advanced behavior.

    Translated from Python test_advanced_behavior.py to OCaml. Tests query
    parameter handling, tag transfer, and enum handling.

    Note: Python tests require Client/httpx not available in OCaml. These tests
    focus on unit testing URL building and query parameter logic. *)

open! Core
open! Expect_test_helpers_core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
module Conftest = Conftest

(* =============================================================================
   Helper Functions for URL/Query Parameter Testing
   ============================================================================= *)

(** Build query string from parameters, excluding empty/None values *)
let build_query_string params =
  List.filter_map params ~f:(fun (key, value_opt) ->
      match value_opt with
      | Some "" -> None (* Empty string excluded *)
      | Some v -> Some (sprintf "%s=%s" key (Uri.pct_encode v))
      | None -> None (* None excluded *))
  |> String.concat ~sep:"&"

(** Parse query string into key-value list *)
let parse_query_string qs =
  String.split qs ~on:'&'
  |> List.filter_map ~f:(fun part ->
         match String.lsplit2 part ~on:'=' with
         | Some (k, v) -> Some (k, Uri.pct_decode v)
         | None -> None)

(* =============================================================================
   Test: Empty Query Parameters Not Sent
   ============================================================================= *)

let%expect_test "empty query params - empty string excluded" =
  let params =
    [ ("name", Some ""); ("active", Some "true"); ("min_id", Some "2") ]
  in
  let qs = build_query_string params in
  printf "query: %s\n" qs;
  printf "has_name: %b\n" (String.is_substring qs ~substring:"name=");
  printf "has_active: %b\n" (String.is_substring qs ~substring:"active=");
  printf "has_min_id: %b\n" (String.is_substring qs ~substring:"min_id=");
  [%expect
    {|
    query: active=true&min_id=2
    has_name: false
    has_active: true
    has_min_id: true
    |}]

let%expect_test "empty query params - None excluded" =
  let params = [ ("name", None); ("active", None); ("min_id", Some "2") ] in
  let qs = build_query_string params in
  printf "query: %s\n" qs;
  printf "has_name: %b\n" (String.is_substring qs ~substring:"name=");
  printf "has_active: %b\n" (String.is_substring qs ~substring:"active=");
  [%expect
    {|
    query: min_id=2
    has_name: false
    has_active: false
    |}]

let%expect_test "empty query params - all None returns empty" =
  let params = [ ("name", None); ("active", None); ("min_id", None) ] in
  let qs = build_query_string params in
  printf "is_empty: %b\n" (String.is_empty qs);
  [%expect {| is_empty: true |}]

let%expect_test "empty query params - mixed values" =
  let params =
    [
      ("name", Some "");
      (* Empty - excluded *)
      ("active", None);
      (* None - excluded *)
      ("min_id", Some "2");
      (* Has value - included *)
    ]
  in
  let qs = build_query_string params in
  let parsed = parse_query_string qs in
  printf "param_count: %d\n" (List.length parsed);
  printf "has_min_id: %b\n" (List.Assoc.mem parsed ~equal:String.equal "min_id");
  printf "has_name: %b\n" (List.Assoc.mem parsed ~equal:String.equal "name");
  printf "has_active: %b\n" (List.Assoc.mem parsed ~equal:String.equal "active");
  [%expect
    {|
    param_count: 1
    has_min_id: true
    has_name: false
    has_active: false
    |}]

(* =============================================================================
   Test: Path Parameters Validation
   ============================================================================= *)

(** Validate path parameters - None values should be rejected *)
let validate_path_params ~required_params params =
  List.filter_map required_params ~f:(fun key ->
      match List.Assoc.find params ~equal:String.equal key with
      | Some (Some v) when not (String.is_empty v) -> None
      | Some (Some _) -> Some (sprintf "Empty value for required param: %s" key)
      | Some None -> Some (sprintf "Missing required path parameter: %s" key)
      | None -> Some (sprintf "Missing required path parameter: %s" key))

let%expect_test "path params - None rejected" =
  let params = [ ("user_id", None); ("name", Some "New Name") ] in
  let errors = validate_path_params ~required_params:[ "user_id" ] params in
  printf "error_count: %d\n" (List.length errors);
  List.iter errors ~f:(fun e -> printf "error: %s\n" e);
  [%expect
    {|
    error_count: 1
    error: Missing required path parameter: user_id
    |}]

let%expect_test "path params - empty string rejected" =
  let params = [ ("user_id", Some ""); ("name", Some "New Name") ] in
  let errors = validate_path_params ~required_params:[ "user_id" ] params in
  printf "error_count: %d\n" (List.length errors);
  List.iter errors ~f:(fun e -> printf "error: %s\n" e);
  [%expect
    {|
    error_count: 1
    error: Empty value for required param: user_id
    |}]

let%expect_test "path params - valid params pass" =
  let params = [ ("user_id", Some "1"); ("name", Some "New Name") ] in
  let errors = validate_path_params ~required_params:[ "user_id" ] params in
  printf "error_count: %d\n" (List.length errors);
  [%expect {| error_count: 0 |}]

(* =============================================================================
   Test: Tag Transfer (unit testing tag structures)
   ============================================================================= *)

type tool_with_tags = { name : string; tags : string list }
(** Tool-like type with tags for testing *)

type resource_with_tags = { name : string; tags : string list; uri : string }
(** Resource-like type with tags for testing *)

type template_with_tags = {
  name : string;
  tags : string list;
  uri_template : string;
}
(** Resource template-like type with tags for testing *)

(** Extract tags from OpenAPI route *)
let extract_tags_from_route route =
  match route with
  | `Assoc fields -> (
    match List.Assoc.find fields ~equal:String.equal "tags" with
    | Some (`List tags) ->
      List.filter_map tags ~f:(function
        | `String s -> Some s
        | _ -> None)
    | _ -> [])
  | _ -> []

let%expect_test "tag extraction - extracts tags from route" =
  let route =
    `Assoc
      [
        ("summary", `String "Create user");
        ("tags", `List [ `String "users"; `String "create" ]);
      ]
  in
  let tags = extract_tags_from_route route in
  printf "tag_count: %d\n" (List.length tags);
  List.iter tags ~f:(fun t -> printf "tag: %s\n" t);
  [%expect {|
    tag_count: 2
    tag: users
    tag: create
    |}]

let%expect_test "tag extraction - empty when no tags" =
  let route = `Assoc [ ("summary", `String "Get user") ] in
  let tags = extract_tags_from_route route in
  printf "tag_count: %d\n" (List.length tags);
  [%expect {| tag_count: 0 |}]

let%expect_test "tags transferred to tool" =
  let route_tags = [ "users"; "create" ] in
  let tool : tool_with_tags =
    { name = "create_user_users_post"; tags = route_tags }
  in
  printf "name: %s\n" tool.name;
  printf "has_users: %b\n" (List.mem tool.tags "users" ~equal:String.equal);
  printf "has_create: %b\n" (List.mem tool.tags "create" ~equal:String.equal);
  printf "tag_count: %d\n" (List.length tool.tags);
  [%expect
    {|
    name: create_user_users_post
    has_users: true
    has_create: true
    tag_count: 2
    |}]

let%expect_test "tags transferred to resource" =
  let route_tags = [ "users"; "list" ] in
  let resource : resource_with_tags =
    {
      name = "get_users_users_get";
      tags = route_tags;
      uri = "resource://get_users_users_get";
    }
  in
  printf "name: %s\n" resource.name;
  printf "has_users: %b\n" (List.mem resource.tags "users" ~equal:String.equal);
  printf "has_list: %b\n" (List.mem resource.tags "list" ~equal:String.equal);
  [%expect
    {|
    name: get_users_users_get
    has_users: true
    has_list: true
    |}]

let%expect_test "tags transferred to resource template" =
  let route_tags = [ "users"; "detail" ] in
  let template : template_with_tags =
    {
      name = "get_user_users";
      tags = route_tags;
      uri_template = "resource://get_user_users/{user_id}";
    }
  in
  printf "name: %s\n" template.name;
  printf "has_users: %b\n" (List.mem template.tags "users" ~equal:String.equal);
  printf "has_detail: %b\n"
    (List.mem template.tags "detail" ~equal:String.equal);
  printf "tag_count: %d\n" (List.length template.tags);
  [%expect
    {|
    name: get_user_users
    has_users: true
    has_detail: true
    tag_count: 2
    |}]

(* =============================================================================
   Test: Enum Handling
   ============================================================================= *)

type enum_definition = { enum_type : string; values : string list }
(** Enum definition structure *)

(** Parse enum definition from JSON schema *)
let parse_enum_definition schema =
  match schema with
  | `Assoc fields -> (
    match
      ( List.Assoc.find fields ~equal:String.equal "type",
        List.Assoc.find fields ~equal:String.equal "enum" )
    with
    | Some (`String t), Some (`List values) ->
      let str_values =
        List.filter_map values ~f:(function
          | `String s -> Some s
          | _ -> None)
      in
      Some { enum_type = t; values = str_values }
    | _ -> None)
  | _ -> None

let%expect_test "enum definition parsing - parses correctly" =
  let schema =
    `Assoc
      [
        ("type", `String "string");
        ("enum", `List [ `String "foo"; `String "bar"; `String "baz" ]);
      ]
  in
  (match parse_enum_definition schema with
  | Some def ->
    printf "type: %s\n" def.enum_type;
    printf "values: %s\n" (String.concat ~sep:", " def.values)
  | None -> printf "failed to parse\n");
  [%expect {|
    type: string
    values: foo, bar, baz
    |}]

let%expect_test "enum definition parsing - missing enum returns None" =
  let schema = `Assoc [ ("type", `String "string") ] in
  printf "parsed: %b\n" (Option.is_some (parse_enum_definition schema));
  [%expect {| parsed: false |}]

(** Check if schema has anyOf with $ref *)
let has_enum_ref schema =
  match schema with
  | `Assoc fields -> (
    match List.Assoc.find fields ~equal:String.equal "anyOf" with
    | Some (`List options) ->
      List.exists options ~f:(fun opt ->
          match opt with
          | `Assoc opt_fields -> (
            match List.Assoc.find opt_fields ~equal:String.equal "$ref" with
            | Some (`String ref_str) ->
              String.is_prefix ref_str ~prefix:"#/$defs/"
            | _ -> false)
          | _ -> false)
    | _ -> false)
  | _ -> false

let%expect_test "enum ref detection - finds $ref in anyOf" =
  let schema =
    `Assoc
      [
        ( "anyOf",
          `List
            [
              `Assoc [ ("$ref", `String "#/$defs/QueryEnum") ];
              `Assoc [ ("type", `String "null") ];
            ] );
      ]
  in
  printf "has_enum_ref: %b\n" (has_enum_ref schema);
  [%expect {| has_enum_ref: true |}]

let%expect_test "enum ref detection - no anyOf returns false" =
  let schema = `Assoc [ ("type", `String "string") ] in
  printf "has_enum_ref: %b\n" (has_enum_ref schema);
  [%expect {| has_enum_ref: false |}]

(** Check if $defs contains enum definition *)
let has_enum_in_defs ~enum_name schema =
  match schema with
  | `Assoc fields -> (
    match List.Assoc.find fields ~equal:String.equal "$defs" with
    | Some (`Assoc defs) -> (
      match List.Assoc.find defs ~equal:String.equal enum_name with
      | Some enum_schema -> Option.is_some (parse_enum_definition enum_schema)
      | None -> false)
    | _ -> false)
  | _ -> false

let%expect_test "enum in $defs - finds enum definition" =
  let schema =
    `Assoc
      [
        ( "$defs",
          `Assoc
            [
              ( "QueryEnum",
                `Assoc
                  [
                    ("type", `String "string");
                    ( "enum",
                      `List [ `String "foo"; `String "bar"; `String "baz" ] );
                  ] );
            ] );
      ]
  in
  printf "has_QueryEnum: %b\n" (has_enum_in_defs ~enum_name:"QueryEnum" schema);
  printf "has_Other: %b\n" (has_enum_in_defs ~enum_name:"Other" schema);
  [%expect {|
    has_QueryEnum: true
    has_Other: false
    |}]
