(** Tests for OpenAPI description propagation to OxFastMCP components.

    Translated from Python test_description_propagation.py to OCaml. Tests that
    descriptions from OpenAPI specs are correctly propagated to resources,
    resource templates, and tools.

    Note: Python tests use httpx.AsyncClient mocking and FastMCP client. These
    OCaml tests focus on unit testing the description extraction logic. *)

open! Core
open! Expect_test_helpers_core

(* =============================================================================
   Helper Types for Description Testing
   ============================================================================= *)

type route_info = {
  path : string;
  method_ : string;
  operation_id : string;
  summary : string option;
  description : string option;
  parameters : parameter_desc list;
}

and parameter_desc = {
  param_name : string;
  param_location : string;
  param_description : string option;
  param_schema : Yojson.Safe.t option;
}

(* =============================================================================
   Description Extraction Functions
   ============================================================================= *)

(** Extract description from OpenAPI operation *)
let extract_operation_description (operation : Yojson.Safe.t) : string option =
  match operation with
  | `Assoc fields ->
    List.Assoc.find fields ~equal:String.equal "description"
    |> Option.bind ~f:(function
         | `String s -> Some s
         | _ -> None)
  | _ -> None

(** Extract summary from OpenAPI operation *)
let extract_operation_summary (operation : Yojson.Safe.t) : string option =
  match operation with
  | `Assoc fields ->
    List.Assoc.find fields ~equal:String.equal "summary"
    |> Option.bind ~f:(function
         | `String s -> Some s
         | _ -> None)
  | _ -> None

(** Extract parameter descriptions from OpenAPI operation *)
let extract_parameter_descriptions (operation : Yojson.Safe.t) :
    (string * string option) list =
  match operation with
  | `Assoc fields -> (
    match List.Assoc.find fields ~equal:String.equal "parameters" with
    | Some (`List params) ->
      List.filter_map params ~f:(fun param ->
          match param with
          | `Assoc p_fields ->
            let name =
              List.Assoc.find p_fields ~equal:String.equal "name"
              |> Option.bind ~f:(function
                   | `String s -> Some s
                   | _ -> None)
            in
            let desc =
              List.Assoc.find p_fields ~equal:String.equal "description"
              |> Option.bind ~f:(function
                   | `String s -> Some s
                   | _ -> None)
            in
            Option.map name ~f:(fun n -> (n, desc))
          | _ -> None)
    | _ -> [])
  | _ -> []

(** Extract response description from OpenAPI operation *)
let extract_response_description (operation : Yojson.Safe.t)
    ~(status_code : string) : string option =
  match operation with
  | `Assoc fields -> (
    match List.Assoc.find fields ~equal:String.equal "responses" with
    | Some (`Assoc responses) -> (
      match List.Assoc.find responses ~equal:String.equal status_code with
      | Some (`Assoc resp_fields) ->
        List.Assoc.find resp_fields ~equal:String.equal "description"
        |> Option.bind ~f:(function
             | `String s -> Some s
             | _ -> None)
      | _ -> None)
    | _ -> None)
  | _ -> None

(** Build component description from route info *)
let build_component_description ~(summary : string option)
    ~(description : string option) ~(params : (string * string option) list)
    ~(response_desc : string option) : string =
  let parts = ref [] in
  (* Add summary if present *)
  Option.iter summary ~f:(fun s -> parts := s :: !parts);
  (* Add description if present *)
  Option.iter description ~f:(fun d -> parts := d :: !parts);
  (* Add response description if present *)
  Option.iter response_desc ~f:(fun r -> parts := ("Response: " ^ r) :: !parts);
  (* Add parameter descriptions *)
  List.iter params ~f:(fun (name, desc) ->
      match desc with
      | Some d -> parts := sprintf "  - %s: %s" name d :: !parts
      | None -> ());
  if
    (not (List.is_empty params))
    && List.exists params ~f:(fun (_, d) -> Option.is_some d)
  then parts := "Parameters:" :: !parts;
  String.concat (List.rev !parts) ~sep:"\n"

(* =============================================================================
   Tests: Description Extraction
   ============================================================================= *)

let%expect_test "extract operation description" =
  let operation =
    `Assoc
      [
        ("operationId", `String "getItem");
        ("summary", `String "Get item summary");
        ("description", `String "GET_DESCRIPTION\n\nFUNCTION_GET_DESCRIPTION");
      ]
  in
  printf "description: %s\n"
    (Option.value (extract_operation_description operation) ~default:"None");
  printf "summary: %s\n"
    (Option.value (extract_operation_summary operation) ~default:"None");
  [%expect
    {|
    description: GET_DESCRIPTION

    FUNCTION_GET_DESCRIPTION
    summary: Get item summary
    |}]

let%expect_test "extract parameter descriptions" =
  let operation =
    `Assoc
      [
        ("operationId", `String "getItem");
        ( "parameters",
          `List
            [
              `Assoc
                [
                  ("name", `String "item_id");
                  ("in", `String "path");
                  ("description", `String "PATH_PARAM_DESCRIPTION");
                ];
              `Assoc
                [
                  ("name", `String "fields");
                  ("in", `String "query");
                  ("description", `String "QUERY_PARAM_DESCRIPTION");
                ];
            ] );
      ]
  in
  let params = extract_parameter_descriptions operation in
  printf "param_count: %d\n" (List.length params);
  List.iter params ~f:(fun (name, desc) ->
      printf "%s: %s\n" name (Option.value desc ~default:"None"));
  [%expect
    {|
    param_count: 2
    item_id: PATH_PARAM_DESCRIPTION
    fields: QUERY_PARAM_DESCRIPTION
    |}]

let%expect_test "extract response description" =
  let operation =
    `Assoc
      [
        ("operationId", `String "listItems");
        ( "responses",
          `Assoc
            [
              ( "200",
                `Assoc
                  [
                    ("description", `String "LIST_RESPONSE_DESCRIPTION");
                    ( "content",
                      `Assoc
                        [
                          ("application/json", `Assoc [ ("schema", `Assoc []) ]);
                        ] );
                  ] );
            ] );
      ]
  in
  printf "200 response: %s\n"
    (Option.value
       (extract_response_description operation ~status_code:"200")
       ~default:"None");
  printf "404 response: %s\n"
    (Option.value
       (extract_response_description operation ~status_code:"404")
       ~default:"None");
  [%expect
    {|
    200 response: LIST_RESPONSE_DESCRIPTION
    404 response: None
    |}]

let%expect_test "build component description" =
  let desc =
    build_component_description ~summary:(Some "Get item summary")
      ~description:(Some "GET_DESCRIPTION\n\nFUNCTION_GET_DESCRIPTION")
      ~params:
        [
          ("item_id", Some "PATH_PARAM_DESCRIPTION");
          ("fields", Some "QUERY_PARAM_DESCRIPTION");
        ]
      ~response_desc:(Some "GET_RESPONSE_DESCRIPTION")
  in
  printf "%s\n" desc;
  [%expect
    {|
    Get item summary
    GET_DESCRIPTION

    FUNCTION_GET_DESCRIPTION
    Response: GET_RESPONSE_DESCRIPTION
      - item_id: PATH_PARAM_DESCRIPTION
      - fields: QUERY_PARAM_DESCRIPTION
    Parameters:
    |}]

(* =============================================================================
   Tests: Description Propagation Verification
   ============================================================================= *)

let%expect_test "description propagation - route description included" =
  let operation =
    `Assoc
      [
        ("operationId", `String "listItems");
        ("summary", `String "List items summary");
        ("description", `String "LIST_DESCRIPTION\n\nFUNCTION_LIST_DESCRIPTION");
        ( "responses",
          `Assoc
            [
              ( "200",
                `Assoc [ ("description", `String "LIST_RESPONSE_DESCRIPTION") ]
              );
            ] );
      ]
  in
  let desc =
    build_component_description
      ~summary:(extract_operation_summary operation)
      ~description:(extract_operation_description operation)
      ~params:[] ~response_desc:None
  in
  printf "has LIST_DESCRIPTION: %b\n"
    (String.is_substring desc ~substring:"LIST_DESCRIPTION");
  printf "has FUNCTION_LIST_DESCRIPTION: %b\n"
    (String.is_substring desc ~substring:"FUNCTION_LIST_DESCRIPTION");
  [%expect
    {|
    has LIST_DESCRIPTION: true
    has FUNCTION_LIST_DESCRIPTION: true
    |}]

let%expect_test "description propagation - path parameter included" =
  let operation =
    `Assoc
      [
        ("operationId", `String "getItem");
        ("description", `String "Get item by ID");
        ( "parameters",
          `List
            [
              `Assoc
                [
                  ("name", `String "item_id");
                  ("in", `String "path");
                  ("description", `String "PATH_PARAM_DESCRIPTION");
                ];
            ] );
      ]
  in
  let params = extract_parameter_descriptions operation in
  let desc =
    build_component_description ~summary:None
      ~description:(extract_operation_description operation)
      ~params ~response_desc:None
  in
  printf "has PATH_PARAM_DESCRIPTION: %b\n"
    (String.is_substring desc ~substring:"PATH_PARAM_DESCRIPTION");
  printf "has item_id: %b\n" (String.is_substring desc ~substring:"item_id");
  [%expect {|
    has PATH_PARAM_DESCRIPTION: true
    has item_id: true
    |}]

let%expect_test "description propagation - query parameter included" =
  let operation =
    `Assoc
      [
        ("operationId", `String "getItem");
        ( "parameters",
          `List
            [
              `Assoc
                [
                  ("name", `String "fields");
                  ("in", `String "query");
                  ("description", `String "QUERY_PARAM_DESCRIPTION");
                ];
            ] );
      ]
  in
  let params = extract_parameter_descriptions operation in
  let desc =
    build_component_description ~summary:None ~description:None ~params
      ~response_desc:None
  in
  printf "has QUERY_PARAM_DESCRIPTION: %b\n"
    (String.is_substring desc ~substring:"QUERY_PARAM_DESCRIPTION");
  [%expect {| has QUERY_PARAM_DESCRIPTION: true |}]

(* =============================================================================
   Tests: Request Body Property Descriptions
   ============================================================================= *)

(** Extract property descriptions from request body schema *)
let extract_request_body_property_descriptions (operation : Yojson.Safe.t) :
    (string * string) list =
  match operation with
  | `Assoc fields -> (
    match List.Assoc.find fields ~equal:String.equal "requestBody" with
    | Some (`Assoc rb_fields) -> (
      match List.Assoc.find rb_fields ~equal:String.equal "content" with
      | Some (`Assoc content) -> (
        match
          List.Assoc.find content ~equal:String.equal "application/json"
        with
        | Some (`Assoc json_content) -> (
          match List.Assoc.find json_content ~equal:String.equal "schema" with
          | Some (`Assoc schema) -> (
            match List.Assoc.find schema ~equal:String.equal "properties" with
            | Some (`Assoc props) ->
              List.filter_map props ~f:(fun (name, prop) ->
                  match prop with
                  | `Assoc prop_fields ->
                    List.Assoc.find prop_fields ~equal:String.equal
                      "description"
                    |> Option.bind ~f:(function
                         | `String s -> Some (name, s)
                         | _ -> None)
                  | _ -> None)
            | _ -> [])
          | _ -> [])
        | _ -> [])
      | _ -> [])
    | _ -> [])
  | _ -> []

let%expect_test "extract request body property descriptions" =
  let operation =
    `Assoc
      [
        ("operationId", `String "createItem");
        ( "requestBody",
          `Assoc
            [
              ("required", `Bool true);
              ("description", `String "BODY_DESCRIPTION");
              ( "content",
                `Assoc
                  [
                    ( "application/json",
                      `Assoc
                        [
                          ( "schema",
                            `Assoc
                              [
                                ("type", `String "object");
                                ( "properties",
                                  `Assoc
                                    [
                                      ( "name",
                                        `Assoc
                                          [
                                            ("type", `String "string");
                                            ( "description",
                                              `String "PROP_DESCRIPTION" );
                                          ] );
                                    ] );
                              ] );
                        ] );
                  ] );
            ] );
      ]
  in
  let props = extract_request_body_property_descriptions operation in
  printf "prop_count: %d\n" (List.length props);
  List.iter props ~f:(fun (name, desc) -> printf "%s: %s\n" name desc);
  [%expect {|
    prop_count: 1
    name: PROP_DESCRIPTION
    |}]

(* =============================================================================
   Tests: Response Model Field Descriptions
   ============================================================================= *)

(** Extract response model field descriptions *)
let extract_response_field_descriptions (operation : Yojson.Safe.t)
    ~(status_code : string) : (string * string) list =
  match operation with
  | `Assoc fields -> (
    match List.Assoc.find fields ~equal:String.equal "responses" with
    | Some (`Assoc responses) -> (
      match List.Assoc.find responses ~equal:String.equal status_code with
      | Some (`Assoc resp_fields) -> (
        match List.Assoc.find resp_fields ~equal:String.equal "content" with
        | Some (`Assoc content) -> (
          match
            List.Assoc.find content ~equal:String.equal "application/json"
          with
          | Some (`Assoc json_content) -> (
            match List.Assoc.find json_content ~equal:String.equal "schema" with
            | Some (`Assoc schema) ->
              (* Handle array of items or direct object *)
              let prop_fields =
                match
                  List.Assoc.find schema ~equal:String.equal "properties"
                with
                | Some (`Assoc props) -> props
                | _ -> (
                  (* Check for array items *)
                  match List.Assoc.find schema ~equal:String.equal "items" with
                  | Some (`Assoc items) -> (
                    match
                      List.Assoc.find items ~equal:String.equal "properties"
                    with
                    | Some (`Assoc props) -> props
                    | _ -> [])
                  | _ -> [])
              in
              List.filter_map prop_fields ~f:(fun (name, prop) ->
                  match prop with
                  | `Assoc pf ->
                    List.Assoc.find pf ~equal:String.equal "description"
                    |> Option.bind ~f:(function
                         | `String s -> Some (name, s)
                         | _ -> None)
                  | _ -> None)
            | _ -> [])
          | _ -> [])
        | _ -> [])
      | _ -> [])
    | _ -> [])
  | _ -> []

let%expect_test "extract response model field descriptions" =
  let operation =
    `Assoc
      [
        ("operationId", `String "listItems");
        ( "responses",
          `Assoc
            [
              ( "200",
                `Assoc
                  [
                    ("description", `String "Success");
                    ( "content",
                      `Assoc
                        [
                          ( "application/json",
                            `Assoc
                              [
                                ( "schema",
                                  `Assoc
                                    [
                                      ("type", `String "array");
                                      ( "items",
                                        `Assoc
                                          [
                                            ("type", `String "object");
                                            ( "properties",
                                              `Assoc
                                                [
                                                  ( "id",
                                                    `Assoc
                                                      [
                                                        ( "type",
                                                          `String "string" );
                                                        ( "description",
                                                          `String
                                                            "ITEM_RESPONSE_ID_DESCRIPTION"
                                                        );
                                                      ] );
                                                  ( "name",
                                                    `Assoc
                                                      [
                                                        ( "type",
                                                          `String "string" );
                                                        ( "description",
                                                          `String
                                                            "ITEM_RESPONSE_NAME_DESCRIPTION"
                                                        );
                                                      ] );
                                                ] );
                                          ] );
                                    ] );
                              ] );
                        ] );
                  ] );
            ] );
      ]
  in
  let fields =
    extract_response_field_descriptions operation ~status_code:"200"
  in
  printf "field_count: %d\n" (List.length fields);
  List.iter fields ~f:(fun (name, desc) -> printf "%s: %s\n" name desc);
  [%expect
    {|
    field_count: 2
    id: ITEM_RESPONSE_ID_DESCRIPTION
    name: ITEM_RESPONSE_NAME_DESCRIPTION
    |}]
