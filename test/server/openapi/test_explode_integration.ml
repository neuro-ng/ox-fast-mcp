(** Tests for OpenAPI explode property handling.

    Translated from Python test_explode_integration.py to OCaml. Tests that the
    explode property is correctly parsed from OpenAPI specifications and
    properly applied during parameter serialization.

    Note: Python tests use httpx.AsyncClient mocking. These OCaml tests focus on
    unit testing parsing and serialization logic. *)

open! Core
open! Expect_test_helpers_core

(* =============================================================================
   Types and Helpers
   ============================================================================= *)

type explode_setting = ExplodeTrue | ExplodeFalse | ExplodeDefault

let explode_to_string = function
  | ExplodeTrue -> "true"
  | ExplodeFalse -> "false"
  | ExplodeDefault -> "default"

type parameter_info = {
  name : string;
  location : string;
  explode : explode_setting;
  style : string option;
  schema : Yojson.Safe.t option;
}

(** Parse explode from OpenAPI parameter JSON *)
let parse_explode (param_json : Yojson.Safe.t) : explode_setting =
  match param_json with
  | `Assoc fields -> (
    match List.Assoc.find fields ~equal:String.equal "explode" with
    | Some (`Bool true) -> ExplodeTrue
    | Some (`Bool false) -> ExplodeFalse
    | _ -> ExplodeDefault)
  | _ -> ExplodeDefault

(** Parse parameter from OpenAPI spec *)
let parse_parameter (param_json : Yojson.Safe.t) : parameter_info option =
  match param_json with
  | `Assoc fields -> (
    let name =
      List.Assoc.find fields ~equal:String.equal "name"
      |> Option.bind ~f:(function
           | `String s -> Some s
           | _ -> None)
    in
    let location =
      List.Assoc.find fields ~equal:String.equal "in"
      |> Option.bind ~f:(function
           | `String s -> Some s
           | _ -> None)
    in
    let explode = parse_explode param_json in
    let style =
      List.Assoc.find fields ~equal:String.equal "style"
      |> Option.bind ~f:(function
           | `String s -> Some s
           | _ -> None)
    in
    let schema = List.Assoc.find fields ~equal:String.equal "schema" in
    match (name, location) with
    | Some n, Some loc ->
      Some { name = n; location = loc; explode; style; schema }
    | _ -> None)
  | _ -> None

(** Serialize array value based on explode setting *)
let serialize_array_param ~(explode : explode_setting) ~(param_name : string)
    (values : string list) : (string * string) list =
  match explode with
  | ExplodeFalse ->
    (* Comma-separated single parameter *)
    [ (param_name, String.concat values ~sep:",") ]
  | ExplodeTrue | ExplodeDefault ->
    (* Separate parameters - represented as multiple key-value pairs *)
    List.map values ~f:(fun v -> (param_name, v))

(* =============================================================================
   Tests: Explode Parsing from OpenAPI Spec
   ============================================================================= *)

let%expect_test "explode=false parsing from OpenAPI spec" =
  let param_json =
    `Assoc
      [
        ("name", `String "tags");
        ("in", `String "query");
        ("required", `Bool false);
        ("style", `String "form");
        ("explode", `Bool false);
        ( "schema",
          `Assoc
            [
              ("type", `String "array");
              ("items", `Assoc [ ("type", `String "string") ]);
            ] );
      ]
  in
  let param = parse_parameter param_json |> Option.value_exn in
  printf "name: %s\n" param.name;
  printf "location: %s\n" param.location;
  printf "explode: %s\n" (explode_to_string param.explode);
  [%expect {|
    name: tags
    location: query
    explode: false
    |}]

let%expect_test "explode=true parsing from OpenAPI spec" =
  let param_json =
    `Assoc
      [
        ("name", `String "tags");
        ("in", `String "query");
        ("explode", `Bool true);
        ( "schema",
          `Assoc
            [
              ("type", `String "array");
              ("items", `Assoc [ ("type", `String "string") ]);
            ] );
      ]
  in
  let param = parse_parameter param_json |> Option.value_exn in
  printf "explode: %s\n" (explode_to_string param.explode);
  [%expect {| explode: true |}]

let%expect_test "explode default parsing from OpenAPI spec" =
  let param_json =
    `Assoc
      [
        ("name", `String "tags");
        ("in", `String "query");
        (* No explode specified *)
        ( "schema",
          `Assoc
            [
              ("type", `String "array");
              ("items", `Assoc [ ("type", `String "string") ]);
            ] );
      ]
  in
  let param = parse_parameter param_json |> Option.value_exn in
  printf "explode: %s\n" (explode_to_string param.explode);
  [%expect {| explode: default |}]

(* =============================================================================
   Tests: Array Serialization with Explode Setting
   ============================================================================= *)

let%expect_test "explode=false serialization - comma separated" =
  let values = [ "red"; "blue"; "green" ] in
  let serialized =
    serialize_array_param ~explode:ExplodeFalse ~param_name:"tags" values
  in
  printf "param_count: %d\n" (List.length serialized);
  List.iter serialized ~f:(fun (k, v) -> printf "%s=%s\n" k v);
  [%expect {|
    param_count: 1
    tags=red,blue,green
    |}]

let%expect_test "explode=true serialization - separate params" =
  let values = [ "red"; "blue"; "green" ] in
  let serialized =
    serialize_array_param ~explode:ExplodeTrue ~param_name:"tags" values
  in
  printf "param_count: %d\n" (List.length serialized);
  List.iter serialized ~f:(fun (k, v) -> printf "%s=%s\n" k v);
  [%expect
    {|
    param_count: 3
    tags=red
    tags=blue
    tags=green
    |}]

let%expect_test "explode default serialization - separate params" =
  let values = [ "red"; "blue"; "green" ] in
  let serialized =
    serialize_array_param ~explode:ExplodeDefault ~param_name:"tags" values
  in
  printf "param_count: %d\n" (List.length serialized);
  List.iter serialized ~f:(fun (k, v) -> printf "%s=%s\n" k v);
  [%expect
    {|
    param_count: 3
    tags=red
    tags=blue
    tags=green
    |}]

let%expect_test "explode=false with single value" =
  let values = [ "red" ] in
  let serialized =
    serialize_array_param ~explode:ExplodeFalse ~param_name:"tags" values
  in
  printf "param_count: %d\n" (List.length serialized);
  List.iter serialized ~f:(fun (k, v) -> printf "%s=%s\n" k v);
  [%expect {|
    param_count: 1
    tags=red
    |}]

let%expect_test "explode=false with empty array" =
  let values = [] in
  let serialized =
    serialize_array_param ~explode:ExplodeFalse ~param_name:"tags" values
  in
  printf "param_count: %d\n" (List.length serialized);
  List.iter serialized ~f:(fun (k, v) -> printf "%s=%s\n" k v);
  [%expect {|
    param_count: 1
    tags=
    |}]

(* =============================================================================
   Tests: Query String Building
   ============================================================================= *)

(** Build query string from parameters *)
let build_query_string (params : (string * string) list) : string =
  List.map params ~f:(fun (k, v) -> sprintf "%s=%s" k (Uri.pct_encode v))
  |> String.concat ~sep:"&"

let%expect_test "build query string - explode=false" =
  let values = [ "red"; "blue"; "green" ] in
  let serialized =
    serialize_array_param ~explode:ExplodeFalse ~param_name:"tags" values
  in
  let query = build_query_string serialized in
  printf "query: %s\n" query;
  [%expect {| query: tags=red,blue,green |}]

let%expect_test "build query string - explode=true" =
  let values = [ "red"; "blue" ] in
  let serialized =
    serialize_array_param ~explode:ExplodeTrue ~param_name:"tags" values
  in
  let query = build_query_string serialized in
  printf "query: %s\n" query;
  [%expect {| query: tags=red&tags=blue |}]

(* =============================================================================
   Tests: Parse Parameters from OpenAPI Route
   ============================================================================= *)

let parse_route_parameters (operation : Yojson.Safe.t) : parameter_info list =
  match operation with
  | `Assoc fields -> (
    match List.Assoc.find fields ~equal:String.equal "parameters" with
    | Some (`List params) -> List.filter_map params ~f:parse_parameter
    | _ -> [])
  | _ -> []

let%expect_test "parse route parameters with explode" =
  let operation =
    `Assoc
      [
        ("operationId", `String "search_items");
        ( "parameters",
          `List
            [
              `Assoc
                [
                  ("name", `String "tags");
                  ("in", `String "query");
                  ("explode", `Bool false);
                  ( "schema",
                    `Assoc
                      [
                        ("type", `String "array");
                        ("items", `Assoc [ ("type", `String "string") ]);
                      ] );
                ];
              `Assoc
                [
                  ("name", `String "categories");
                  ("in", `String "query");
                  ("explode", `Bool true);
                  ( "schema",
                    `Assoc
                      [
                        ("type", `String "array");
                        ("items", `Assoc [ ("type", `String "string") ]);
                      ] );
                ];
            ] );
      ]
  in
  let params = parse_route_parameters operation in
  printf "param_count: %d\n" (List.length params);
  List.iter params ~f:(fun p ->
      printf "%s: explode=%s\n" p.name (explode_to_string p.explode));
  [%expect
    {|
    param_count: 2
    tags: explode=false
    categories: explode=true
    |}]
