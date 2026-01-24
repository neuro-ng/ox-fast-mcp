(** Tests for OpenAPI path parameter handling.

    Translated from Python test_openapi_path_parameters.py to OCaml. Tests array
    path parameters, query parameters with explode settings, and parameter
    location enum handling.

    Note: Python tests use httpx.AsyncClient mocking. These OCaml tests focus on
    parameter serialization logic. *)

open! Core
open! Expect_test_helpers_core

(* =============================================================================
   Types for Path Parameter Handling
   ============================================================================= *)

type parameter_location = Path | Query | Header | Cookie
[@@deriving sexp, equal]

let location_of_string = function
  | "path" -> Some Path
  | "query" -> Some Query
  | "header" -> Some Header
  | "cookie" -> Some Cookie
  | _ -> None

let location_to_string = function
  | Path -> "path"
  | Query -> "query"
  | Header -> "header"
  | Cookie -> "cookie"

type parameter_style = Simple | Form | DeepObject | Label | Matrix

let style_of_string = function
  | "simple" -> Some Simple
  | "form" -> Some Form
  | "deepObject" -> Some DeepObject
  | "label" -> Some Label
  | "matrix" -> Some Matrix
  | _ -> None

type parameter_info = {
  name : string;
  location : parameter_location;
  required : bool;
  style : parameter_style option;
  explode : bool option;
  schema : Yojson.Safe.t option;
}

(* =============================================================================
   Path Parameter Serialization
   ============================================================================= *)

(** Serialize array value for path parameter (style=simple, explode=false) *)
let serialize_path_array (values : string list) : string =
  String.concat values ~sep:","

(** Serialize complex object for path parameter *)
let serialize_path_object (obj : (string * string) list) : string =
  List.map obj ~f:(fun (k, v) -> sprintf "%s,%s" k v) |> String.concat ~sep:","

(** Replace path parameter in URL template *)
let replace_path_param ~(url : string) ~(param_name : string) ~(value : string)
    : string =
  let pattern = sprintf "{%s}" param_name in
  String.substr_replace_all url ~pattern ~with_:value

(* =============================================================================
   Tests: Array Path Parameter Handling
   ============================================================================= *)

let%expect_test "serialize array path parameter - single value" =
  let values = [ "monday" ] in
  let serialized = serialize_path_array values in
  printf "serialized: %s\n" serialized;
  [%expect {| serialized: monday |}]

let%expect_test "serialize array path parameter - multiple values" =
  let values = [ "monday"; "tuesday"; "wednesday" ] in
  let serialized = serialize_path_array values in
  printf "serialized: %s\n" serialized;
  [%expect {| serialized: monday,tuesday,wednesday |}]

let%expect_test "replace path parameter in URL" =
  let url = "/select/{days}" in
  let result =
    replace_path_param ~url ~param_name:"days" ~value:"monday,tuesday"
  in
  printf "url: %s\n" result;
  [%expect {| url: /select/monday,tuesday |}]

let%expect_test "path parameter - full workflow" =
  let url = "/select/{days}" in
  let values = [ "monday"; "tuesday" ] in
  let serialized = serialize_path_array values in
  let result = replace_path_param ~url ~param_name:"days" ~value:serialized in
  printf "url: %s\n" result;
  printf "expected: /select/monday,tuesday\n";
  printf "matches: %b\n" (String.equal result "/select/monday,tuesday");
  [%expect
    {|
    url: /select/monday,tuesday
    expected: /select/monday,tuesday
    matches: true
    |}]

(* =============================================================================
   Tests: Complex Nested Array Path Parameters
   ============================================================================= *)

let%expect_test "serialize complex object for path" =
  let obj = [ ("status", "active"); ("type", "user") ] in
  let serialized = serialize_path_object obj in
  printf "serialized: %s\n" serialized;
  [%expect {| serialized: status,active,type,user |}]

let%expect_test "complex filters path parameter" =
  let url = "/report/{filters}" in
  let filters = [ ("field", "status"); ("value", "active") ] in
  let serialized = serialize_path_object filters in
  let result =
    replace_path_param ~url ~param_name:"filters" ~value:serialized
  in
  printf "url: %s\n" result;
  printf "contains status: %b\n"
    (String.is_substring result ~substring:"status");
  printf "contains active: %b\n"
    (String.is_substring result ~substring:"active");
  printf "no braces: %b\n"
    ((not (String.is_substring result ~substring:"{"))
    && not (String.is_substring result ~substring:"}"));
  [%expect
    {|
    url: /report/field,status,value,active
    contains status: true
    contains active: true
    no braces: true
    |}]

(* =============================================================================
   Tests: Query Parameter with Explode Settings
   ============================================================================= *)

(** Serialize array query parameter based on explode setting *)
let serialize_query_array ~(explode : bool) ~(param_name : string)
    (values : string list) : (string * string) list =
  if explode then List.map values ~f:(fun v -> (param_name, v))
  else [ (param_name, String.concat values ~sep:",") ]

let%expect_test "query parameter explode=false - comma separated" =
  let values = [ "monday"; "tuesday" ] in
  let params = serialize_query_array ~explode:false ~param_name:"days" values in
  printf "param_count: %d\n" (List.length params);
  List.iter params ~f:(fun (k, v) -> printf "%s=%s\n" k v);
  [%expect {|
    param_count: 1
    days=monday,tuesday
    |}]

let%expect_test "query parameter explode=true - separate params" =
  let values = [ "monday"; "tuesday" ] in
  let params = serialize_query_array ~explode:true ~param_name:"days" values in
  printf "param_count: %d\n" (List.length params);
  List.iter params ~f:(fun (k, v) -> printf "%s=%s\n" k v);
  [%expect {|
    param_count: 2
    days=monday
    days=tuesday
    |}]

(* =============================================================================
   Tests: Empty Array Parameter Exclusion
   ============================================================================= *)

(** Filter out empty array parameters *)
let filter_empty_arrays (params : (string * string list) list) :
    (string * string list) list =
  List.filter params ~f:(fun (_, values) -> not (List.is_empty values))

let%expect_test "empty array parameter exclusion" =
  let params =
    [
      ("tags", []);
      (* Empty - should be excluded *)
      ("categories", [ "tech"; "news" ]);
      (* Non-empty - should be included *)
    ]
  in
  let filtered = filter_empty_arrays params in
  printf "param_count: %d\n" (List.length filtered);
  List.iter filtered ~f:(fun (k, v) ->
      printf "%s: [%s]\n" k (String.concat v ~sep:", "));
  [%expect {|
    param_count: 1
    categories: [tech, news]
    |}]

(* =============================================================================
   Tests: Parameter Location Enum Handling
   ============================================================================= *)

let%expect_test "parameter location enum handling" =
  let test_cases =
    [ ("path", Path); ("query", Query); ("header", Header); ("cookie", Cookie) ]
  in
  List.iter test_cases ~f:(fun (input, expected) ->
      match location_of_string input with
      | Some loc ->
        printf "%s -> %s (matches: %b)\n" input (location_to_string loc)
          (equal_parameter_location loc expected)
      | None -> printf "%s -> None\n" input);
  [%expect
    {|
    path -> path (matches: true)
    query -> query (matches: true)
    header -> header (matches: true)
    cookie -> cookie (matches: true)
    |}]

let%expect_test "parameter location string conversion round-trip" =
  let locations = [ Path; Query; Header; Cookie ] in
  List.iter locations ~f:(fun loc ->
      let str = location_to_string loc in
      let roundtrip = location_of_string str in
      printf "%s -> %s (round-trip: %b)\n" str
        (Option.value_map roundtrip ~default:"None" ~f:location_to_string)
        (Option.equal equal_parameter_location roundtrip (Some loc)));
  [%expect
    {|
    path -> path (round-trip: true)
    query -> query (round-trip: true)
    header -> header (round-trip: true)
    cookie -> cookie (round-trip: true)
    |}]

(* =============================================================================
   Tests: Parse Parameter from OpenAPI Spec
   ============================================================================= *)

let parse_parameter (json : Yojson.Safe.t) : parameter_info option =
  match json with
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
           | `String s -> location_of_string s
           | _ -> None)
    in
    let required =
      List.Assoc.find fields ~equal:String.equal "required"
      |> Option.value_map ~default:false ~f:(function
           | `Bool b -> b
           | _ -> false)
    in
    let style =
      List.Assoc.find fields ~equal:String.equal "style"
      |> Option.bind ~f:(function
           | `String s -> style_of_string s
           | _ -> None)
    in
    let explode =
      List.Assoc.find fields ~equal:String.equal "explode"
      |> Option.bind ~f:(function
           | `Bool b -> Some b
           | _ -> None)
    in
    let schema = List.Assoc.find fields ~equal:String.equal "schema" in
    match (name, location) with
    | Some n, Some loc ->
      Some { name = n; location = loc; required; style; explode; schema }
    | _ -> None)
  | _ -> None

let%expect_test "parse array path parameter from spec" =
  let param_json =
    `Assoc
      [
        ("name", `String "days");
        ("in", `String "path");
        ("required", `Bool true);
        ("style", `String "simple");
        ("explode", `Bool false);
        ( "schema",
          `Assoc
            [
              ("type", `String "array");
              ( "items",
                `Assoc
                  [
                    ("type", `String "string");
                    ( "enum",
                      `List
                        [
                          `String "monday";
                          `String "tuesday";
                          `String "wednesday";
                        ] );
                  ] );
            ] );
      ]
  in
  let param = parse_parameter param_json |> Option.value_exn in
  printf "name: %s\n" param.name;
  printf "location: %s\n" (location_to_string param.location);
  printf "required: %b\n" param.required;
  printf "explode: %b\n" (Option.value param.explode ~default:true);
  [%expect
    {|
    name: days
    location: path
    required: true
    explode: false
    |}]

(* =============================================================================
   Tests: DeepObject Empty Dict Exclusion
   ============================================================================= *)

(** Check if object is empty *)
let is_empty_object (obj : (string * 'a) list) : bool = List.is_empty obj

let%expect_test "empty deep object exclusion" =
  let params =
    [
      ("filters", []);
      (* Empty - should be excluded *)
      ("options", [ ("sort", "name"); ("order", "asc") ]);
      (* Non-empty *)
    ]
  in
  let non_empty =
    List.filter params ~f:(fun (_, obj) -> not (is_empty_object obj))
  in
  printf "non_empty_count: %d\n" (List.length non_empty);
  List.iter non_empty ~f:(fun (k, _) -> printf "- %s\n" k);
  [%expect {|
    non_empty_count: 1
    - options
    |}]
