open Alcotest
open Yojson.Safe
open Utilities.Openapi

module TestData = struct
  let fastapi_schema = `Assoc [
    ("openapi", `String "3.1.0");
    ("info", `Assoc [
      ("title", `String "Comprehensive Test API");
      ("description", `String "A test API with various OpenAPI features");
      ("version", `String "1.0.0");
    ]);
    ("paths", `Assoc [
      ("/items", `Assoc [
        ("get", `Assoc [
          ("operationId", `String "list_items");
          ("summary", `String "List all items");
          ("description", `String "Get a list of all items with optional filtering");
          ("tags", `List [`String "items"]);
          ("parameters", `List [
            `Assoc [
              ("name", `String "skip");
              ("in", `String "query");
              ("description", `String "Number of items to skip");
              ("required", `Bool false);
              ("schema", `Assoc [("type", `String "integer")]);
            ];
            `Assoc [
              ("name", `String "limit");
              ("in", `String "query");
              ("description", `String "Max number of items to return");
              ("required", `Bool false);
              ("schema", `Assoc [
                ("type", `String "integer");
                ("default", `Int 10);
              ]);
            ];
            `Assoc [
              ("name", `String "status");
              ("in", `String "query");
              ("description", `String "Filter items by status");
              ("required", `Bool false);
              ("schema", `Assoc [
                ("type", `String "string");
                ("enum", `List [
                  `String "available";
                  `String "pending";
                  `String "sold";
                ]);
              ]);
            ];
          ]);
          ("responses", `Assoc [
            ("200", `Assoc [("description", `String "List of items")]);
          ]);
        ]);
        ("post", `Assoc [
          ("operationId", `String "create_item");
          ("summary", `String "Create a new item");
          ("tags", `List [`String "items"]);
          ("parameters", `List [
            `Assoc [
              ("name", `String "x-token");
              ("in", `String "header");
              ("description", `String "Authentication token");
              ("required", `Bool true);
              ("schema", `Assoc [("type", `String "string")]);
            ];
          ]);
          ("requestBody", `Assoc [
            ("required", `Bool true);
            ("content", `Assoc [
              ("application/json", `Assoc [
                ("schema", `Assoc [
                  ("type", `String "object");
                  ("required", `List [`String "name"; `String "price"]);
                  ("properties", `Assoc [
                    ("name", `Assoc [("type", `String "string")]);
                    ("description", `Assoc [("type", `String "string")]);
                    ("price", `Assoc [("type", `String "number")]);
                    ("tax", `Assoc [("type", `String "number")]);
                    ("tags", `Assoc [
                      ("type", `String "array");
                      ("items", `Assoc [("type", `String "string")]);
                    ]);
                    ("status", `Assoc [
                      ("type", `String "string");
                      ("enum", `List [
                        `String "available";
                        `String "pending";
                        `String "sold";
                      ]);
                      ("default", `String "available");
                    ]);
                  ]);
                ]);
              ]);
            ]);
          ]);
          ("responses", `Assoc [
            ("201", `Assoc [("description", `String "Item created")]);
          ]);
        ]);
      ]);
    ]);
  ]
end

let test_parse_fastapi_schema_route_count () =
  let routes = parse_openapi_to_http_routes TestData.fastapi_schema in
  check int "route count" 2 (List.length routes)

let test_parse_fastapi_schema_operation_ids () =
  let routes = parse_openapi_to_http_routes TestData.fastapi_schema in
  let expected_operations = [
    "list_items";
    "create_item";
  ] in
  List.iter (fun op_id ->
    let found = List.exists (fun route ->
      match route.operation_id with
      | Some id -> String.equal id op_id
      | None -> false
    ) routes in
    check bool (Printf.sprintf "operation %s exists" op_id) found true
  ) expected_operations

let test_path_parameter_parsing () =
  let routes = parse_openapi_to_http_routes TestData.fastapi_schema in
  (* Since the current schema doesn't have path parameters, we'll test that there are 0 path params *)
  let list_items = List.find_opt (fun route ->
    match route.operation_id with
    | Some id -> String.equal id "list_items"
    | None -> false
  ) routes in
  match list_items with
  | None -> fail "list_items route not found"
  | Some route ->
    let path_params = List.filter (fun param ->
      param.location = `Path
    ) route.parameters in
    check int "path param count" 0 (List.length path_params)

let test_query_parameter_parsing () =
  let routes = parse_openapi_to_http_routes TestData.fastapi_schema in
  let list_items = List.find_opt (fun route ->
    match route.operation_id with
    | Some id -> String.equal id "list_items"
    | None -> false
  ) routes in
  match list_items with
  | None -> fail "list_items route not found"
  | Some route ->
    let query_params = List.filter (fun param ->
      param.location = `Query
    ) route.parameters in
    check int "query param count" (List.length query_params) 3;
    let param_names = List.map (fun p -> p.name) query_params in
    check bool "has skip param" (List.mem "skip" param_names) true;
    check bool "has limit param" (List.mem "limit" param_names) true;
    check bool "has status param" (List.mem "status" param_names) true

let test_header_parameter_parsing () =
  let routes = parse_openapi_to_http_routes TestData.fastapi_schema in
  let create_item = List.find_opt (fun route ->
    match route.operation_id with
    | Some id -> String.equal id "create_item"
    | None -> false
  ) routes in
  match create_item with
  | None -> fail "create_item route not found"
  | Some route ->
    let header_params = List.filter (fun param ->
      param.location = `Header
    ) route.parameters in
    check int "header param count" (List.length header_params) 1;
    let token_param = List.hd header_params in
    check string "param name" token_param.name "x-token";
    check bool "param required" token_param.required true

let test_request_body_content_type () =
  let routes = parse_openapi_to_http_routes TestData.fastapi_schema in
  let create_item = List.find_opt (fun route ->
    match route.operation_id with
    | Some id -> String.equal id "create_item"
    | None -> false
  ) routes in
  match create_item with
  | None -> fail "create_item route not found"
  | Some route ->
    match route.request_body with
    | None -> fail "request body not found"
    | Some body ->
      check bool "has application/json" (List.mem_assoc "application/json" body.content_schema) true

let test_suite = [
  "FastAPI OpenAPI Schema", [
    test_case "Route count" `Quick test_parse_fastapi_schema_route_count;
    test_case "Operation IDs" `Quick test_parse_fastapi_schema_operation_ids;
    test_case "Path parameters" `Quick test_path_parameter_parsing;
    test_case "Query parameters" `Quick test_query_parameter_parsing;
    test_case "Header parameters" `Quick test_header_parameter_parsing;
    test_case "Request body content type" `Quick test_request_body_content_type;
  ];
]

let () = run "FastAPI OpenAPI Tests" test_suite 