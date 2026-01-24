(** Tests for OpenAPI 3.0 and 3.1 compatibility.

    Translated from Python test_openapi_compatibility.py to OCaml. Tests
    compatibility with both OpenAPI 3.0 and 3.1 specifications, including
    handling of version-specific features.

    Note: Python tests use httpx mocking and full server integration. These
    OCaml tests focus on parsing and schema handling differences. *)

open! Core
open! Expect_test_helpers_core

(* =============================================================================
   Helper Functions for Version Detection
   ============================================================================= *)

type openapi_version = V30 | V31 | Unknown

let parse_version (spec : Yojson.Safe.t) : openapi_version =
  match spec with
  | `Assoc fields -> (
    match List.Assoc.find fields ~equal:String.equal "openapi" with
    | Some (`String v) when String.is_prefix v ~prefix:"3.0" -> V30
    | Some (`String v) when String.is_prefix v ~prefix:"3.1" -> V31
    | _ -> Unknown)
  | _ -> Unknown

let version_to_string = function
  | V30 -> "3.0"
  | V31 -> "3.1"
  | Unknown -> "unknown"

(* =============================================================================
   Tests: Version Detection
   ============================================================================= *)

let%expect_test "detect OpenAPI 3.0 version" =
  let spec =
    `Assoc
      [
        ("openapi", `String "3.0.0");
        ( "info",
          `Assoc [ ("title", `String "Test"); ("version", `String "1.0") ] );
      ]
  in
  printf "version: %s\n" (version_to_string (parse_version spec));
  [%expect {| version: 3.0 |}]

let%expect_test "detect OpenAPI 3.1 version" =
  let spec =
    `Assoc
      [
        ("openapi", `String "3.1.0");
        ( "info",
          `Assoc [ ("title", `String "Test"); ("version", `String "1.0") ] );
      ]
  in
  printf "version: %s\n" (version_to_string (parse_version spec));
  [%expect {| version: 3.1 |}]

(* =============================================================================
   Helper: Extract routes from OpenAPI spec
   ============================================================================= *)

type route_info = {
  path : string;
  method_ : string;
  operation_id : string option;
  summary : string option;
}

let extract_routes (spec : Yojson.Safe.t) : route_info list =
  match spec with
  | `Assoc fields -> (
    match List.Assoc.find fields ~equal:String.equal "paths" with
    | Some (`Assoc paths) ->
      List.concat_map paths ~f:(fun (path, methods) ->
          match methods with
          | `Assoc method_list ->
            List.filter_map method_list ~f:(fun (method_, operation) ->
                (* Skip non-HTTP method keys like "parameters" *)
                if
                  List.mem
                    [ "get"; "post"; "put"; "delete"; "patch" ]
                    method_ ~equal:String.equal
                then
                  match operation with
                  | `Assoc op_fields ->
                    let operation_id =
                      List.Assoc.find op_fields ~equal:String.equal
                        "operationId"
                      |> Option.bind ~f:(function
                           | `String s -> Some s
                           | _ -> None)
                    in
                    let summary =
                      List.Assoc.find op_fields ~equal:String.equal "summary"
                      |> Option.bind ~f:(function
                           | `String s -> Some s
                           | _ -> None)
                    in
                    Some { path; method_; operation_id; summary }
                  | _ -> None
                else None)
          | _ -> [])
    | _ -> [])
  | _ -> []

let%expect_test "extract routes from OpenAPI 3.0 spec" =
  let spec =
    `Assoc
      [
        ("openapi", `String "3.0.0");
        ( "info",
          `Assoc
            [ ("title", `String "Product API"); ("version", `String "1.0") ] );
        ( "paths",
          `Assoc
            [
              ( "/products",
                `Assoc
                  [
                    ( "get",
                      `Assoc
                        [
                          ("operationId", `String "listProducts");
                          ("summary", `String "List all products");
                        ] );
                    ( "post",
                      `Assoc
                        [
                          ("operationId", `String "createProduct");
                          ("summary", `String "Create a new product");
                        ] );
                  ] );
              ( "/products/{product_id}",
                `Assoc
                  [
                    ( "get",
                      `Assoc
                        [
                          ("operationId", `String "getProduct");
                          ("summary", `String "Get product by ID");
                        ] );
                  ] );
            ] );
      ]
  in
  let routes = extract_routes spec in
  printf "route_count: %d\n" (List.length routes);
  List.iter routes ~f:(fun r ->
      printf "%s %s: %s\n"
        (String.uppercase r.method_)
        r.path
        (Option.value r.operation_id ~default:"<none>"));
  [%expect
    {|
    route_count: 3
    GET /products: listProducts
    POST /products: createProduct
    GET /products/{product_id}: getProduct
    |}]

(* =============================================================================
   Tests: OpenAPI 3.0 vs 3.1 Schema Differences
   ============================================================================= *)

(** Check if schema uses OpenAPI 3.0 style exclusiveMaximum (boolean) *)
let has_boolean_exclusive_maximum (schema : Yojson.Safe.t) : bool =
  match schema with
  | `Assoc fields -> (
    match List.Assoc.find fields ~equal:String.equal "exclusiveMaximum" with
    | Some (`Bool _) -> true
    | _ -> false)
  | _ -> false

(** Check if schema uses OpenAPI 3.1 style exclusiveMaximum (number) *)
let has_numeric_exclusive_maximum (schema : Yojson.Safe.t) : bool =
  match schema with
  | `Assoc fields -> (
    match List.Assoc.find fields ~equal:String.equal "exclusiveMaximum" with
    | Some (`Int _) | Some (`Float _) -> true
    | _ -> false)
  | _ -> false

let%expect_test "OpenAPI 3.0 exclusiveMaximum boolean format" =
  let schema =
    `Assoc
      [
        ("type", `String "number");
        ("minimum", `Int 0);
        ("maximum", `Int 100);
        ("exclusiveMaximum", `Bool true);
      ]
  in
  printf "has boolean exclusiveMaximum: %b\n"
    (has_boolean_exclusive_maximum schema);
  printf "has numeric exclusiveMaximum: %b\n"
    (has_numeric_exclusive_maximum schema);
  [%expect
    {|
    has boolean exclusiveMaximum: true
    has numeric exclusiveMaximum: false
    |}]

let%expect_test "OpenAPI 3.1 exclusiveMaximum numeric format" =
  let schema =
    `Assoc
      [
        ("type", `String "number");
        ("minimum", `Int 0);
        ("exclusiveMaximum", `Int 100);
      ]
  in
  printf "has boolean exclusiveMaximum: %b\n"
    (has_boolean_exclusive_maximum schema);
  printf "has numeric exclusiveMaximum: %b\n"
    (has_numeric_exclusive_maximum schema);
  [%expect
    {|
    has boolean exclusiveMaximum: false
    has numeric exclusiveMaximum: true
    |}]

(* =============================================================================
   Tests: Nullable Handling Differences
   ============================================================================= *)

(** Check if schema uses OpenAPI 3.0 nullable format *)
let has_nullable_keyword (schema : Yojson.Safe.t) : bool =
  match schema with
  | `Assoc fields -> (
    match List.Assoc.find fields ~equal:String.equal "nullable" with
    | Some (`Bool true) -> true
    | _ -> false)
  | _ -> false

(** Check if schema uses OpenAPI 3.1 type array format for nullable *)
let has_type_array_with_null (schema : Yojson.Safe.t) : bool =
  match schema with
  | `Assoc fields -> (
    match List.Assoc.find fields ~equal:String.equal "type" with
    | Some (`List types) ->
      List.exists types ~f:(function
        | `String "null" -> true
        | _ -> false)
    | _ -> false)
  | _ -> false

let%expect_test "OpenAPI 3.0 nullable format" =
  let schema =
    `Assoc [ ("type", `String "string"); ("nullable", `Bool true) ]
  in
  printf "has nullable keyword: %b\n" (has_nullable_keyword schema);
  printf "has type array with null: %b\n" (has_type_array_with_null schema);
  [%expect
    {|
    has nullable keyword: true
    has type array with null: false
    |}]

let%expect_test "OpenAPI 3.1 type array format" =
  let schema =
    `Assoc [ ("type", `List [ `String "string"; `String "null" ]) ]
  in
  printf "has nullable keyword: %b\n" (has_nullable_keyword schema);
  printf "has type array with null: %b\n" (has_type_array_with_null schema);
  [%expect
    {|
    has nullable keyword: false
    has type array with null: true
    |}]

(* =============================================================================
   Helper: Normalize Schema for Compatibility
   ============================================================================= *)

(** Normalize exclusive constraints from 3.0 to 3.1 style *)
let normalize_exclusive_constraints (schema : Yojson.Safe.t) : Yojson.Safe.t =
  match schema with
  | `Assoc fields ->
    let maximum =
      List.Assoc.find fields ~equal:String.equal "maximum"
      |> Option.bind ~f:(function
           | `Int i -> Some i
           | `Float f -> Some (Float.to_int f)
           | _ -> None)
    in
    let minimum =
      List.Assoc.find fields ~equal:String.equal "minimum"
      |> Option.bind ~f:(function
           | `Int i -> Some i
           | `Float f -> Some (Float.to_int f)
           | _ -> None)
    in
    let exclusive_max_bool =
      List.Assoc.find fields ~equal:String.equal "exclusiveMaximum"
      |> Option.bind ~f:(function
           | `Bool b -> Some b
           | _ -> None)
    in
    let exclusive_min_bool =
      List.Assoc.find fields ~equal:String.equal "exclusiveMinimum"
      |> Option.bind ~f:(function
           | `Bool b -> Some b
           | _ -> None)
    in
    let new_fields =
      List.filter_map fields ~f:(fun (k, v) ->
          match k with
          | "exclusiveMaximum" when Option.is_some exclusive_max_bool ->
            Option.bind maximum ~f:(fun m ->
                if Option.value exclusive_max_bool ~default:false then
                  Some (k, `Int m)
                else None)
          | "exclusiveMinimum" when Option.is_some exclusive_min_bool ->
            Option.bind minimum ~f:(fun m ->
                if Option.value exclusive_min_bool ~default:false then
                  Some (k, `Int m)
                else None)
          | "maximum" when Option.is_some exclusive_max_bool -> None
          | "minimum" when Option.is_some exclusive_min_bool -> None
          | _ -> Some (k, v))
    in
    `Assoc new_fields
  | _ -> schema

let%expect_test "normalize 3.0 exclusive constraints to 3.1" =
  let schema_30 =
    `Assoc
      [
        ("type", `String "number");
        ("minimum", `Int 0);
        ("maximum", `Int 100);
        ("exclusiveMaximum", `Bool true);
      ]
  in
  let normalized = normalize_exclusive_constraints schema_30 in
  (match normalized with
  | `Assoc fields -> (
    printf "has exclusiveMaximum: %b\n"
      (List.Assoc.mem fields ~equal:String.equal "exclusiveMaximum");
    match List.Assoc.find fields ~equal:String.equal "exclusiveMaximum" with
    | Some (`Int i) -> printf "exclusiveMaximum value: %d\n" i
    | _ -> ())
  | _ -> ());
  [%expect
    {|
    has exclusiveMaximum: true
    exclusiveMaximum value: 100
    |}]

(* =============================================================================
   Tests: $ref Resolution
   ============================================================================= *)

(** Extract component schemas from spec *)
let extract_component_schemas (spec : Yojson.Safe.t) :
    (string * Yojson.Safe.t) list =
  match spec with
  | `Assoc fields -> (
    match List.Assoc.find fields ~equal:String.equal "components" with
    | Some (`Assoc comp_fields) -> (
      match List.Assoc.find comp_fields ~equal:String.equal "schemas" with
      | Some (`Assoc schemas) -> schemas
      | _ -> [])
    | _ -> [])
  | _ -> []

let%expect_test "extract component schemas" =
  let spec =
    `Assoc
      [
        ("openapi", `String "3.0.0");
        ( "info",
          `Assoc [ ("title", `String "Test"); ("version", `String "1.0") ] );
        ( "components",
          `Assoc
            [
              ( "schemas",
                `Assoc
                  [
                    ( "LoanDetails",
                      `Assoc
                        [
                          ("type", `String "object");
                          ( "properties",
                            `Assoc
                              [
                                ("amount", `Assoc [ ("type", `String "number") ]);
                                ( "interest_rate",
                                  `Assoc
                                    [
                                      ("type", `String "number");
                                      ("minimum", `Int 0);
                                      ("maximum", `Int 100);
                                      ("exclusiveMaximum", `Bool true);
                                    ] );
                              ] );
                        ] );
                  ] );
            ] );
      ]
  in
  let schemas = extract_component_schemas spec in
  printf "schema_count: %d\n" (List.length schemas);
  List.iter schemas ~f:(fun (name, _) -> printf "- %s\n" name);
  [%expect {|
    schema_count: 1
    - LoanDetails
    |}]

(* =============================================================================
   Tests: Full Spec Compatibility
   ============================================================================= *)

let%expect_test "OpenAPI 3.0 spec with exclusiveMaximum is parseable" =
  let spec =
    `Assoc
      [
        ("openapi", `String "3.0.0");
        ( "info",
          `Assoc [ ("title", `String "Loan API"); ("version", `String "1.0") ]
        );
        ( "paths",
          `Assoc
            [
              ( "/loans",
                `Assoc
                  [
                    ( "post",
                      `Assoc
                        [
                          ("operationId", `String "createLoan");
                          ("summary", `String "Create a loan");
                        ] );
                  ] );
            ] );
        ( "components",
          `Assoc
            [
              ( "schemas",
                `Assoc
                  [
                    ( "LoanDetails",
                      `Assoc
                        [
                          ("type", `String "object");
                          ( "properties",
                            `Assoc
                              [
                                ( "interest_rate",
                                  `Assoc
                                    [
                                      ("type", `String "number");
                                      ("maximum", `Int 100);
                                      ("exclusiveMaximum", `Bool true);
                                    ] );
                              ] );
                        ] );
                  ] );
            ] );
      ]
  in
  let version = parse_version spec in
  let routes = extract_routes spec in
  printf "version: %s\n" (version_to_string version);
  printf "route_count: %d\n" (List.length routes);
  printf "operation_id: %s\n"
    (Option.value
       (List.hd routes |> Option.bind ~f:(fun r -> r.operation_id))
       ~default:"<none>");
  [%expect
    {|
    version: 3.0
    route_count: 1
    operation_id: createLoan
    |}]

let%expect_test "OpenAPI 3.1 spec with numeric exclusiveMaximum is parseable" =
  let spec =
    `Assoc
      [
        ("openapi", `String "3.1.0");
        ( "info",
          `Assoc [ ("title", `String "Loan API"); ("version", `String "1.0") ]
        );
        ( "paths",
          `Assoc
            [
              ( "/loans",
                `Assoc
                  [
                    ( "post",
                      `Assoc
                        [
                          ("operationId", `String "createLoan");
                          ("summary", `String "Create a loan");
                        ] );
                  ] );
            ] );
        ( "components",
          `Assoc
            [
              ( "schemas",
                `Assoc
                  [
                    ( "LoanDetails",
                      `Assoc
                        [
                          ("type", `String "object");
                          ( "properties",
                            `Assoc
                              [
                                ( "interest_rate",
                                  `Assoc
                                    [
                                      ("type", `String "number");
                                      ("exclusiveMaximum", `Int 100);
                                    ] );
                              ] );
                        ] );
                  ] );
            ] );
      ]
  in
  let version = parse_version spec in
  let routes = extract_routes spec in
  printf "version: %s\n" (version_to_string version);
  printf "route_count: %d\n" (List.length routes);
  printf "operation_id: %s\n"
    (Option.value
       (List.hd routes |> Option.bind ~f:(fun r -> r.operation_id))
       ~default:"<none>");
  [%expect
    {|
    version: 3.1
    route_count: 1
    operation_id: createLoan
    |}]
