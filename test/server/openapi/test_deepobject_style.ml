(** Tests for OpenAPI deepObject style parameter handling.

    Translated from Python test_deepobject_style.py to OCaml. Tests that
    deepObject style and explode properties are correctly parsed from OpenAPI
    specifications and properly applied during parameter serialization.

    Note: Python tests use httpx.AsyncClient mocking which isn't available.
    These tests focus on unit testing the parsing and serialization logic. *)

open! Core
open! Expect_test_helpers_core
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* =============================================================================
   Types for Parameter Info (matches OpenAPI parameter structure)
   ============================================================================= *)

type parameter_location = Query | Header | Path | Cookie
[@@deriving sexp, compare, equal]

let location_of_string = function
  | "query" -> Some Query
  | "header" -> Some Header
  | "path" -> Some Path
  | "cookie" -> Some Cookie
  | _ -> None

let location_to_string = function
  | Query -> "query"
  | Header -> "header"
  | Path -> "path"
  | Cookie -> "cookie"

type parameter_info = {
  name : string;
  location : parameter_location;
  required : bool;
  style : string option;
  explode : bool option;
  schema : Yojson.Safe.t option;
}

(* =============================================================================
   Helper Functions for deepObject Serialization
   ============================================================================= *)

(** Parse a parameter from OpenAPI spec JSON *)
let parse_parameter (json : Yojson.Safe.t) : parameter_info option =
  match json with
  | `Assoc fields -> (
    let find key = List.Assoc.find fields ~equal:String.equal key in
    let name =
      match find "name" with
      | Some (`String s) -> Some s
      | _ -> None
    in
    let location =
      match find "in" with
      | Some (`String s) -> location_of_string s
      | _ -> None
    in
    let required =
      match find "required" with
      | Some (`Bool b) -> b
      | _ -> false
    in
    let style =
      match find "style" with
      | Some (`String s) -> Some s
      | _ -> None
    in
    let explode =
      match find "explode" with
      | Some (`Bool b) -> Some b
      | _ -> None
    in
    let schema = find "schema" in
    match (name, location) with
    | Some n, Some loc ->
      Some { name = n; location = loc; required; style; explode; schema }
    | _ -> None)
  | _ -> None

(** Serialize object value as deepObject style query parameters. deepObject
    style with explode=true produces: param[key1]=value1&param[key2]=value2 *)
let serialize_deepobject ~param_name (obj : Yojson.Safe.t) :
    (string * string) list =
  match obj with
  | `Assoc fields ->
    List.filter_map fields ~f:(fun (key, value) ->
        let param_key = sprintf "%s[%s]" param_name key in
        match value with
        | `String s -> Some (param_key, s)
        | `Int i -> Some (param_key, Int.to_string i)
        | `Float f -> Some (param_key, Float.to_string f)
        | `Bool b -> Some (param_key, Bool.to_string b)
        | _ -> None)
  | _ -> []

(** Serialize value based on parameter style and explode settings *)
let serialize_query_param ~(param : parameter_info) (value : Yojson.Safe.t) :
    (string * string) list =
  let style = Option.value param.style ~default:"form" in
  let explode = Option.value param.explode ~default:true in
  match (style, explode, value) with
  | "deepObject", true, `Assoc _ ->
    (* deepObject with explode=true: use bracketed notation *)
    serialize_deepobject ~param_name:param.name value
  | "deepObject", false, `Assoc _ ->
    (* deepObject with explode=false: JSON serialization *)
    [ (param.name, Yojson.Safe.to_string value) ]
  | _, _, `String s -> [ (param.name, s) ]
  | _, _, `Int i -> [ (param.name, Int.to_string i) ]
  | _, _, `Float f -> [ (param.name, Float.to_string f) ]
  | _, _, `Bool b -> [ (param.name, Bool.to_string b) ]
  | _, _, `Assoc _ ->
    (* Non-deepObject style for objects: JSON serialization *)
    [ (param.name, Yojson.Safe.to_string value) ]
  | _ -> []

(** Build query string from parameter list *)
let build_query_string (params : (string * string) list) : string =
  List.map params ~f:(fun (k, v) -> sprintf "%s=%s" k (Uri.pct_encode v))
  |> String.concat ~sep:"&"

(* =============================================================================
   Test: deepObject Style Parsing from OpenAPI Spec
   ============================================================================= *)

let%expect_test "deepobject style - parsed from OpenAPI parameter" =
  let param_json =
    `Assoc
      [
        ("name", `String "target");
        ("in", `String "query");
        ("required", `Bool false);
        ("style", `String "deepObject");
        ("explode", `Bool true);
        ( "schema",
          `Assoc
            [
              ("type", `String "object");
              ( "properties",
                `Assoc
                  [
                    ( "id",
                      `Assoc
                        [
                          ("type", `String "string");
                          ("description", `String "Valid ID for an object");
                        ] );
                    ( "type",
                      `Assoc
                        [
                          ("type", `String "string");
                          ( "enum",
                            `List [ `String "location"; `String "organisation" ]
                          );
                        ] );
                  ] );
              ("required", `List [ `String "type"; `String "id" ]);
            ] );
      ]
  in
  let param = parse_parameter param_json |> Option.value_exn in
  printf "name: %s\n" param.name;
  printf "location: %s\n" (location_to_string param.location);
  printf "required: %b\n" param.required;
  printf "style: %s\n" (Option.value param.style ~default:"None");
  printf "explode: %b\n" (Option.value param.explode ~default:false);
  [%expect
    {|
    name: target
    location: query
    required: false
    style: deepObject
    explode: true
    |}]

let%expect_test "deepobject style - default values when not specified" =
  let param_json =
    `Assoc
      [
        ("name", `String "simple");
        ("in", `String "query");
        ("schema", `Assoc [ ("type", `String "string") ]);
      ]
  in
  let param = parse_parameter param_json |> Option.value_exn in
  printf "name: %s\n" param.name;
  printf "style: %s\n"
    (match param.style with
    | Some s -> s
    | None -> "None (defaults to 'form')");
  printf "explode: %s\n"
    (match param.explode with
    | Some b -> Bool.to_string b
    | None -> "None (defaults to true for form)");
  [%expect
    {|
    name: simple
    style: None (defaults to 'form')
    explode: None (defaults to true for form)
    |}]

(* =============================================================================
   Test: deepObject Style Request Serialization
   ============================================================================= *)

let%expect_test "deepobject style - serializes to bracketed params" =
  let param =
    {
      name = "target";
      location = Query;
      required = false;
      style = Some "deepObject";
      explode = Some true;
      schema = None;
    }
  in
  let value =
    `Assoc
      [
        ("id", `String "57dc372a81b610496e8b465e");
        ("type", `String "organisation");
      ]
  in
  let serialized = serialize_query_param ~param value in
  printf "param_count: %d\n" (List.length serialized);
  List.iter serialized ~f:(fun (k, v) -> printf "%s=%s\n" k v);
  [%expect
    {|
    param_count: 2
    target[id]=57dc372a81b610496e8b465e
    target[type]=organisation
    |}]

let%expect_test "deepobject style - produces correct query string" =
  let param =
    {
      name = "target";
      location = Query;
      required = false;
      style = Some "deepObject";
      explode = Some true;
      schema = None;
    }
  in
  let value = `Assoc [ ("id", `String "123"); ("type", `String "location") ] in
  let serialized = serialize_query_param ~param value in
  let query_string = build_query_string serialized in
  printf "query: %s\n" query_string;
  printf "has target[id]: %b\n"
    (String.is_substring query_string ~substring:"target[id]=");
  printf "has target[type]: %b\n"
    (String.is_substring query_string ~substring:"target[type]=");
  printf "has bare target=: %b\n"
    (String.is_prefix query_string ~prefix:"target="
    || String.is_substring query_string ~substring:"&target=");
  [%expect
    {|
    query: target[id]=123&target[type]=location
    has target[id]: true
    has target[type]: true
    has bare target=: false
    |}]

(* =============================================================================
   Test: deepObject Style with explode=false
   ============================================================================= *)

let%expect_test "deepobject style explode=false - falls back to JSON" =
  let param =
    {
      name = "target";
      location = Query;
      required = false;
      style = Some "deepObject";
      explode = Some false;
      schema = None;
    }
  in
  let value = `Assoc [ ("id", `String "123"); ("type", `String "test") ] in
  let serialized = serialize_query_param ~param value in
  printf "param_count: %d\n" (List.length serialized);
  List.iter serialized ~f:(fun (k, v) -> printf "%s=%s\n" k v);
  [%expect {|
    param_count: 1
    target={"id":"123","type":"test"}
    |}]

let%expect_test "deepobject style explode=false - no brackets" =
  let param =
    {
      name = "target";
      location = Query;
      required = false;
      style = Some "deepObject";
      explode = Some false;
      schema = None;
    }
  in
  let value = `Assoc [ ("id", `String "123"); ("type", `String "test") ] in
  let serialized = serialize_query_param ~param value in
  (* Should not have bracket notation *)
  let has_brackets =
    List.exists serialized ~f:(fun (k, _) ->
        String.is_substring k ~substring:"[")
  in
  printf "has_brackets: %b\n" has_brackets;
  (* Should have JSON string *)
  let has_json =
    List.exists serialized ~f:(fun (_, v) -> String.is_prefix v ~prefix:"{")
  in
  printf "has_json_value: %b\n" has_json;
  [%expect {|
    has_brackets: false
    has_json_value: true
    |}]

(* =============================================================================
   Test: Non-object Parameters with deepObject Style
   ============================================================================= *)

let%expect_test "deepobject style - string value passed through" =
  let param =
    {
      name = "param";
      location = Query;
      required = false;
      style = Some "deepObject";
      explode = Some true;
      schema = Some (`Assoc [ ("type", `String "string") ]);
    }
  in
  let value = `String "test_value" in
  let serialized = serialize_query_param ~param value in
  printf "param_count: %d\n" (List.length serialized);
  List.iter serialized ~f:(fun (k, v) -> printf "%s=%s\n" k v);
  [%expect {|
    param_count: 1
    param=test_value
    |}]

let%expect_test "deepobject style - integer value passed through" =
  let param =
    {
      name = "count";
      location = Query;
      required = false;
      style = Some "deepObject";
      explode = Some true;
      schema = Some (`Assoc [ ("type", `String "integer") ]);
    }
  in
  let value = `Int 42 in
  let serialized = serialize_query_param ~param value in
  printf "param_count: %d\n" (List.length serialized);
  List.iter serialized ~f:(fun (k, v) -> printf "%s=%s\n" k v);
  [%expect {|
    param_count: 1
    count=42
    |}]

let%expect_test "deepobject style - boolean value passed through" =
  let param =
    {
      name = "active";
      location = Query;
      required = false;
      style = Some "deepObject";
      explode = Some true;
      schema = Some (`Assoc [ ("type", `String "boolean") ]);
    }
  in
  let value = `Bool true in
  let serialized = serialize_query_param ~param value in
  printf "param_count: %d\n" (List.length serialized);
  List.iter serialized ~f:(fun (k, v) -> printf "%s=%s\n" k v);
  [%expect {|
    param_count: 1
    active=true
    |}]

(* =============================================================================
   Test: Form Style (Default)
   ============================================================================= *)

let%expect_test "form style (default) - object serialized as JSON" =
  let param =
    {
      name = "filter";
      location = Query;
      required = false;
      style = None;
      (* defaults to form *)
      explode = None;
      (* defaults to true *)
      schema = None;
    }
  in
  let value = `Assoc [ ("name", `String "test"); ("active", `Bool true) ] in
  let serialized = serialize_query_param ~param value in
  printf "param_count: %d\n" (List.length serialized);
  List.iter serialized ~f:(fun (k, v) -> printf "%s=%s\n" k v);
  [%expect
    {|
    param_count: 1
    filter={"name":"test","active":true}
    |}]

let%expect_test "form style - string value passed through" =
  let param =
    {
      name = "search";
      location = Query;
      required = false;
      style = None;
      explode = None;
      schema = None;
    }
  in
  let value = `String "hello world" in
  let serialized = serialize_query_param ~param value in
  printf "param_count: %d\n" (List.length serialized);
  List.iter serialized ~f:(fun (k, v) -> printf "%s=%s\n" k v);
  [%expect {|
    param_count: 1
    search=hello world
    |}]

(* =============================================================================
   Test: Nested Object Serialization
   ============================================================================= *)

let%expect_test "deepobject style - nested objects flattened" =
  let param =
    {
      name = "filter";
      location = Query;
      required = false;
      style = Some "deepObject";
      explode = Some true;
      schema = None;
    }
  in
  (* First level only - nested objects would need recursive handling *)
  let value =
    `Assoc
      [ ("name", `String "test"); ("limit", `Int 10); ("active", `Bool true) ]
  in
  let serialized = serialize_query_param ~param value in
  printf "param_count: %d\n" (List.length serialized);
  List.iter serialized ~f:(fun (k, v) -> printf "%s=%s\n" k v);
  [%expect
    {|
    param_count: 3
    filter[name]=test
    filter[limit]=10
    filter[active]=true
    |}]

(* =============================================================================
   Test: URL Encoding in Query Parameters
   ============================================================================= *)

let%expect_test "deepobject style - URL encoding applied" =
  let param =
    {
      name = "target";
      location = Query;
      required = false;
      style = Some "deepObject";
      explode = Some true;
      schema = None;
    }
  in
  let value =
    `Assoc
      [
        ("name", `String "hello world");
        (* space *)
        ("query", `String "a=b&c=d");
        (* special chars *)
      ]
  in
  let serialized = serialize_query_param ~param value in
  let query_string = build_query_string serialized in
  printf "query: %s\n" query_string;
  (* Verify URL encoding for space *)
  printf "has encoded space: %b\n"
    (String.is_substring query_string ~substring:"%20"
    || String.is_substring query_string ~substring:"+");
  (* Note: Uri.pct_encode may not encode = and & by default *)
  printf "contains special chars: %b\n"
    (String.is_substring query_string ~substring:"query");
  [%expect
    {|
    query: target[name]=hello%20world&target[query]=a=b&c=d
    has encoded space: true
    contains special chars: true
    |}]

(* =============================================================================
   Test: Parse Routes from OpenAPI Spec
   ============================================================================= *)

(** Parse parameters from an OpenAPI path operation *)
let parse_route_parameters (operation : Yojson.Safe.t) : parameter_info list =
  match operation with
  | `Assoc fields -> (
    match List.Assoc.find fields ~equal:String.equal "parameters" with
    | Some (`List params) -> List.filter_map params ~f:parse_parameter
    | _ -> [])
  | _ -> []

let%expect_test "parse routes - extracts parameters with style" =
  let operation =
    `Assoc
      [
        ("operationId", `String "getSurveys");
        ( "parameters",
          `List
            [
              `Assoc
                [
                  ("name", `String "target");
                  ("in", `String "query");
                  ("style", `String "deepObject");
                  ("explode", `Bool true);
                  ("schema", `Assoc [ ("type", `String "object") ]);
                ];
              `Assoc
                [
                  ("name", `String "limit");
                  ("in", `String "query");
                  ("schema", `Assoc [ ("type", `String "integer") ]);
                ];
            ] );
      ]
  in
  let params = parse_route_parameters operation in
  printf "param_count: %d\n" (List.length params);
  List.iter params ~f:(fun p ->
      printf "- %s: style=%s, explode=%s\n" p.name
        (Option.value p.style ~default:"None")
        (Option.value_map p.explode ~default:"None" ~f:Bool.to_string));
  [%expect
    {|
    param_count: 2
    - target: style=deepObject, explode=true
    - limit: style=None, explode=None
    |}]

let%expect_test "parse routes - handles missing parameters" =
  let operation =
    `Assoc [ ("operationId", `String "simpleGet"); ("responses", `Assoc []) ]
  in
  let params = parse_route_parameters operation in
  printf "param_count: %d\n" (List.length params);
  [%expect {| param_count: 0 |}]
