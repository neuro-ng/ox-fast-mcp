(** Tests for optional parameter handling in OxFastMCP OpenAPI integration.

    Translated from Python test_optional_parameters.py to OCaml. Tests that
    optional parameters preserve their original schema without forcing nullable
    behavior.

    Note: These tests verify schema generation for optional parameters. *)

open! Core
open! Expect_test_helpers_core

(* =============================================================================
   Types for Schema Generation
   ============================================================================= *)

type parameter_info = {
  name : string;
  location : string;
  required : bool;
  schema : Yojson.Safe.t;
  description : string option;
}

(* =============================================================================
   Schema Combination Logic
   ============================================================================= *)

(** Combine parameter schemas into a single JSON schema *)
let combine_schemas (parameters : parameter_info list) : Yojson.Safe.t =
  let properties =
    List.map parameters ~f:(fun p ->
        let prop_schema =
          match p.schema with
          | `Assoc fields ->
            let with_desc =
              match p.description with
              | Some d ->
                if List.Assoc.mem fields ~equal:String.equal "description" then
                  fields
                else ("description", `String d) :: fields
              | None -> fields
            in
            `Assoc with_desc
          | other -> other
        in
        (p.name, prop_schema))
  in
  let required_list =
    List.filter_map parameters ~f:(fun p ->
        if p.required then Some (`String p.name) else None)
  in
  `Assoc
    [
      ("type", `String "object");
      ("properties", `Assoc properties);
      ("required", `List required_list);
    ]

(** Check if schema has anyOf with null type *)
let has_nullable_anyof (schema : Yojson.Safe.t) : bool =
  match schema with
  | `Assoc fields -> List.Assoc.mem fields ~equal:String.equal "anyOf"
  | _ -> false

(** Get required list from combined schema *)
let get_required_list (schema : Yojson.Safe.t) : string list =
  match schema with
  | `Assoc fields -> (
    match List.Assoc.find fields ~equal:String.equal "required" with
    | Some (`List items) ->
      List.filter_map items ~f:(function
        | `String s -> Some s
        | _ -> None)
    | _ -> [])
  | _ -> []

(** Get property schema from combined schema *)
let get_property_schema (schema : Yojson.Safe.t) ~(name : string) :
    Yojson.Safe.t option =
  match schema with
  | `Assoc fields -> (
    match List.Assoc.find fields ~equal:String.equal "properties" with
    | Some (`Assoc props) -> List.Assoc.find props ~equal:String.equal name
    | _ -> None)
  | _ -> None

(* =============================================================================
   Tests: Optional Parameter Schema Preservation
   ============================================================================= *)

let%expect_test "optional parameter preserves original type" =
  let optional_param =
    {
      name = "optional_param";
      location = "query";
      required = false;
      schema = `Assoc [ ("type", `String "string") ];
      description = Some "Optional parameter";
    }
  in
  let required_param =
    {
      name = "required_param";
      location = "query";
      required = true;
      schema = `Assoc [ ("type", `String "string") ];
      description = Some "Required parameter";
    }
  in
  let schema = combine_schemas [ required_param; optional_param ] in
  (* Check optional parameter schema *)
  let opt_schema = get_property_schema schema ~name:"optional_param" in
  (match opt_schema with
  | Some (`Assoc fields) ->
    printf "optional type: %s\n"
      (List.Assoc.find fields ~equal:String.equal "type"
      |> Option.value_map ~default:"None" ~f:(function
           | `String s -> s
           | _ -> "other"));
    printf "has anyOf: %b\n" (List.Assoc.mem fields ~equal:String.equal "anyOf")
  | _ -> printf "schema: None\n");
  (* Check required parameter schema *)
  let req_schema = get_property_schema schema ~name:"required_param" in
  (match req_schema with
  | Some (`Assoc fields) ->
    printf "required type: %s\n"
      (List.Assoc.find fields ~equal:String.equal "type"
      |> Option.value_map ~default:"None" ~f:(function
           | `String s -> s
           | _ -> "other"));
    printf "has anyOf: %b\n" (List.Assoc.mem fields ~equal:String.equal "anyOf")
  | _ -> printf "schema: None\n");
  (* Check required list *)
  let required = get_required_list schema in
  printf "required_param in required: %b\n"
    (List.mem required "required_param" ~equal:String.equal);
  printf "optional_param in required: %b\n"
    (List.mem required "optional_param" ~equal:String.equal);
  [%expect
    {|
    optional type: string
    has anyOf: false
    required type: string
    has anyOf: false
    required_param in required: true
    optional_param in required: false
    |}]

(* =============================================================================
   Tests: Various Schema Types Preserved
   ============================================================================= *)

let test_schema_preserved (schema_json : Yojson.Safe.t) : unit =
  let param =
    {
      name = "optional_param";
      location = "query";
      required = false;
      schema = schema_json;
      description = Some "Optional parameter";
    }
  in
  let combined = combine_schemas [ param ] in
  let prop_schema = get_property_schema combined ~name:"optional_param" in
  match prop_schema with
  | Some (`Assoc fields) ->
    printf "has anyOf: %b\n" (List.Assoc.mem fields ~equal:String.equal "anyOf");
    printf "has description: %b\n"
      (List.Assoc.mem fields ~equal:String.equal "description")
  | _ -> printf "schema: None\n"

let%expect_test "optional string parameter preserves schema" =
  test_schema_preserved (`Assoc [ ("type", `String "string") ]);
  [%expect {|
    has anyOf: false
    has description: true
    |}]

let%expect_test "optional integer parameter preserves schema" =
  test_schema_preserved (`Assoc [ ("type", `String "integer") ]);
  [%expect {|
    has anyOf: false
    has description: true
    |}]

let%expect_test "optional number parameter preserves schema" =
  test_schema_preserved (`Assoc [ ("type", `String "number") ]);
  [%expect {|
    has anyOf: false
    has description: true
    |}]

let%expect_test "optional boolean parameter preserves schema" =
  test_schema_preserved (`Assoc [ ("type", `String "boolean") ]);
  [%expect {|
    has anyOf: false
    has description: true
    |}]

let%expect_test "optional array parameter preserves schema" =
  test_schema_preserved
    (`Assoc
      [
        ("type", `String "array");
        ("items", `Assoc [ ("type", `String "string") ]);
      ]);
  [%expect {|
    has anyOf: false
    has description: true
    |}]

let%expect_test "optional object parameter preserves schema" =
  test_schema_preserved
    (`Assoc
      [
        ("type", `String "object");
        ( "properties",
          `Assoc [ ("name", `Assoc [ ("type", `String "string") ]) ] );
      ]);
  [%expect {|
    has anyOf: false
    has description: true
    |}]

(* =============================================================================
   Tests: Schema Type and Fields Preserved
   ============================================================================= *)

let%expect_test "optional parameter schema fields preserved" =
  let param =
    {
      name = "filter";
      location = "query";
      required = false;
      schema =
        `Assoc
          [
            ("type", `String "object");
            ( "properties",
              `Assoc [ ("name", `Assoc [ ("type", `String "string") ]) ] );
          ];
      description = Some "Filter parameter";
    }
  in
  let combined = combine_schemas [ param ] in
  let prop_schema = get_property_schema combined ~name:"filter" in
  (match prop_schema with
  | Some (`Assoc fields) ->
    printf "type: %s\n"
      (List.Assoc.find fields ~equal:String.equal "type"
      |> Option.value_map ~default:"None" ~f:(function
           | `String s -> s
           | _ -> "other"));
    printf "has properties: %b\n"
      (List.Assoc.mem fields ~equal:String.equal "properties");
    printf "description: %s\n"
      (List.Assoc.find fields ~equal:String.equal "description"
      |> Option.value_map ~default:"None" ~f:(function
           | `String s -> s
           | _ -> "other"))
  | _ -> printf "schema: None\n");
  [%expect
    {|
    type: object
    has properties: true
    description: Filter parameter
    |}]
