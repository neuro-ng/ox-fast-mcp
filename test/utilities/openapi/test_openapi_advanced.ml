open Alcotest
open Yojson.Safe

module TestData = struct
  let complex_schema = `Assoc [
    ("openapi", `String "3.1.0");
    ("info", `Assoc [
      ("title", `String "Complex API");
      ("version", `String "1.0.0");
    ]);
    ("paths", `Assoc [
      ("/users", `Assoc [
        ("get", `Assoc [
          ("summary", `String "List all users");
          ("operationId", `String "listUsers");
          ("parameters", `List [
            `Assoc [("$ref", `String "#/components/parameters/PageLimit")];
            `Assoc [("$ref", `String "#/components/parameters/PageOffset")];
          ]);
          ("responses", `Assoc [
            ("200", `Assoc [("description", `String "A list of users")]);
          ]);
        ]);
      ]);
      ("/users/{userId}", `Assoc [
        ("get", `Assoc [
          ("summary", `String "Get user by ID");
          ("operationId", `String "getUser");
          ("parameters", `List [
            `Assoc [("$ref", `String "#/components/parameters/UserId")];
            `Assoc [("$ref", `String "#/components/parameters/IncludeInactive")];
          ]);
          ("responses", `Assoc [
            ("200", `Assoc [("description", `String "User details")]);
          ]);
        ]);
      ]);
      ("/users/{userId}/orders", `Assoc [
        ("post", `Assoc [
          ("summary", `String "Create order for user");
          ("operationId", `String "createOrder");
          ("parameters", `List [
            `Assoc [("$ref", `String "#/components/parameters/UserId")];
          ]);
          ("requestBody", `Assoc [("$ref", `String "#/components/requestBodies/OrderRequest")]);
          ("responses", `Assoc [
            ("201", `Assoc [("description", `String "Order created")]);
          ]);
        ]);
      ]);
    ]);
    ("components", `Assoc [
      ("parameters", `Assoc [
        ("UserId", `Assoc [
          ("name", `String "userId");
          ("in", `String "path");
          ("required", `Bool true);
          ("schema", `Assoc [
            ("type", `String "string");
            ("format", `String "uuid");
          ]);
        ]);
        ("PageLimit", `Assoc [
          ("name", `String "limit");
          ("in", `String "query");
          ("schema", `Assoc [
            ("type", `String "integer");
            ("default", `Int 20);
            ("maximum", `Int 100);
          ]);
        ]);
        ("PageOffset", `Assoc [
          ("name", `String "offset");
          ("in", `String "query");
          ("schema", `Assoc [
            ("type", `String "integer");
            ("default", `Int 0);
          ]);
        ]);
        ("IncludeInactive", `Assoc [
          ("name", `String "include_inactive");
          ("in", `String "query");
          ("schema", `Assoc [
            ("type", `String "boolean");
            ("default", `Bool false);
          ]);
        ]);
      ]);
      ("schemas", `Assoc [
        ("User", `Assoc [
          ("type", `String "object");
          ("properties", `Assoc [
            ("id", `Assoc [
              ("type", `String "string");
              ("format", `String "uuid");
            ]);
            ("name", `Assoc [("type", `String "string")]);
            ("email", `Assoc [
              ("type", `String "string");
              ("format", `String "email");
            ]);
            ("role", `Assoc [("$ref", `String "#/components/schemas/Role")]);
            ("address", `Assoc [("$ref", `String "#/components/schemas/Address")]);
          ]);
        ]);
        ("Role", `Assoc [
          ("type", `String "string");
          ("enum", `List [`String "admin"; `String "user"; `String "guest"]);
        ]);
        ("Address", `Assoc [
          ("type", `String "object");
          ("properties", `Assoc [
            ("street", `Assoc [("type", `String "string")]);
            ("city", `Assoc [("type", `String "string")]);
            ("zip", `Assoc [("type", `String "string")]);
            ("country", `Assoc [("type", `String "string")]);
          ]);
        ]);
        ("Order", `Assoc [
          ("type", `String "object");
          ("properties", `Assoc [
            ("id", `Assoc [
              ("type", `String "string");
              ("format", `String "uuid");
            ]);
            ("items", `Assoc [
              ("type", `String "array");
              ("items", `Assoc [("$ref", `String "#/components/schemas/OrderItem")]);
            ]);
            ("total", `Assoc [("type", `String "number")]);
            ("status", `Assoc [("$ref", `String "#/components/schemas/OrderStatus")]);
          ]);
        ]);
        ("OrderItem", `Assoc [
          ("type", `String "object");
          ("properties", `Assoc [
            ("product_id", `Assoc [
              ("type", `String "string");
              ("format", `String "uuid");
            ]);
            ("quantity", `Assoc [("type", `String "integer")]);
            ("price", `Assoc [("type", `String "number")]);
          ]);
        ]);
        ("OrderStatus", `Assoc [
          ("type", `String "string");
          ("enum", `List [
            `String "pending";
            `String "processing";
            `String "shipped";
            `String "delivered";
            `String "cancelled";
          ]);
        ]);
      ]);
      ("requestBodies", `Assoc [
        ("OrderRequest", `Assoc [
          ("description", `String "Order to create");
          ("required", `Bool true);
          ("content", `Assoc [
            ("application/json", `Assoc [
              ("schema", `Assoc [
                ("type", `String "object");
                ("required", `List [`String "items"]);
                ("properties", `Assoc [
                  ("items", `Assoc [
                    ("type", `String "array");
                    ("items", `Assoc [("$ref", `String "#/components/schemas/OrderItem")]);
                  ]);
                  ("notes", `Assoc [("type", `String "string")]);
                ]);
              ]);
            ]);
          ]);
        ]);
      ]);
    ]);
  ]

  let schema_with_invalid_reference = `Assoc [
    ("openapi", `String "3.1.0");
    ("info", `Assoc [
      ("title", `String "Invalid Reference API");
      ("version", `String "1.0.0");
    ]);
    ("paths", `Assoc [
      ("/broken-ref", `Assoc [
        ("get", `Assoc [
          ("summary", `String "Endpoint with broken reference");
          ("operationId", `String "brokenRef");
          ("parameters", `List [
            `Assoc [("$ref", `String "#/components/parameters/NonExistentParam")];
          ]);
          ("responses", `Assoc [
            ("200", `Assoc [("description", `String "Something")]);
          ]);
        ]);
      ]);
    ]);
    ("components", `Assoc [
      ("parameters", `Assoc []);
    ]);
  ]

  let schema_with_content_params = `Assoc [
    ("openapi", `String "3.1.0");
    ("info", `Assoc [
      ("title", `String "Content Params API");
      ("version", `String "1.0.0");
    ]);
    ("paths", `Assoc [
      ("/complex-params", `Assoc [
        ("post", `Assoc [
          ("summary", `String "Endpoint with complex parameter");
          ("operationId", `String "complexParams");
          ("parameters", `List [
            `Assoc [
              ("name", `String "filter");
              ("in", `String "query");
              ("content", `Assoc [
                ("application/json", `Assoc [
                  ("schema", `Assoc [
                    ("type", `String "object");
                    ("properties", `Assoc [
                      ("field", `Assoc [("type", `String "string")]);
                      ("operator", `Assoc [
                        ("type", `String "string");
                        ("enum", `List [`String "eq"; `String "gt"; `String "lt"]);
                      ]);
                      ("value", `Assoc [("type", `String "string")]);
                    ]);
                  ]);
                ]);
              ]);
            ];
          ]);
          ("responses", `Assoc [
            ("200", `Assoc [("description", `String "Results")]);
          ]);
        ]);
      ]);
    ]);
  ]

  let schema_all_http_methods = `Assoc [
    ("openapi", `String "3.1.0");
    ("info", `Assoc [
      ("title", `String "All Methods API");
      ("version", `String "1.0.0");
    ]);
    ("paths", `Assoc [
      ("/resource", `Assoc [
        ("get", `Assoc [
          ("operationId", `String "getResource");
          ("responses", `Assoc [
            ("200", `Assoc [("description", `String "Success")]);
          ]);
        ]);
        ("post", `Assoc [
          ("operationId", `String "createResource");
          ("responses", `Assoc [
            ("201", `Assoc [("description", `String "Created")]);
          ]);
        ]);
        ("put", `Assoc [
          ("operationId", `String "updateResource");
          ("responses", `Assoc [
            ("200", `Assoc [("description", `String "Updated")]);
          ]);
        ]);
        ("delete", `Assoc [
          ("operationId", `String "deleteResource");
          ("responses", `Assoc [
            ("204", `Assoc [("description", `String "Deleted")]);
          ]);
        ]);
        ("patch", `Assoc [
          ("operationId", `String "patchResource");
          ("responses", `Assoc [
            ("200", `Assoc [("description", `String "Patched")]);
          ]);
        ]);
        ("head", `Assoc [
          ("operationId", `String "headResource");
          ("responses", `Assoc [
            ("200", `Assoc [("description", `String "Headers only")]);
          ]);
        ]);
        ("options", `Assoc [
          ("operationId", `String "optionsResource");
          ("responses", `Assoc [
            ("200", `Assoc [("description", `String "Options")]);
          ]);
        ]);
        ("trace", `Assoc [
          ("operationId", `String "traceResource");
          ("responses", `Assoc [
            ("200", `Assoc [("description", `String "Trace")]);
          ]);
        ]);
      ]);
    ]);
  ]

  let schema_with_external_reference = `Assoc [
    ("openapi", `String "3.0.0");
    ("info", `Assoc [
      ("title", `String "External Reference API");
      ("version", `String "1.0.0");
    ]);
    ("paths", `Assoc [
      ("/products", `Assoc [
        ("post", `Assoc [
          ("summary", `String "Create a product");
          ("operationId", `String "createProduct");
          ("requestBody", `Assoc [
            ("required", `Bool true);
            ("content", `Assoc [
              ("application/json", `Assoc [
                ("schema", `Assoc [
                  ("type", `String "object");
                  ("properties", `Assoc [
                    ("obj", `Assoc [
                      ("$ref", `String "http://cyaninc.com/json-schemas/market-v1/product-constraints");
                    ]);
                  ]);
                ]);
              ]);
            ]);
          ]);
          ("responses", `Assoc [
            ("201", `Assoc [("description", `String "Product created")]);
          ]);
        ]);
      ]);
    ]);
  ]
end

module TestComplexSchemas = struct
  let test_complex_schema_route_count () =
    let routes = FastMCP.Utilities.OpenAPI.parse_openapi_to_http_routes TestData.complex_schema in
    Alcotest.(check int) "route count" 3 (List.length routes)

  let rec check_no_components_refs json =
    match json with
    | `Assoc fields ->
        List.iter (fun (key, value) ->
          if key = "$ref" then
            match value with
            | `String ref_str ->
                if String.starts_with ~prefix:"#/components/" ref_str then
                  Alcotest.fail (Printf.sprintf "reference '%s' was not rewritten" ref_str)
            | _ -> ()
          else
            check_no_components_refs value
        ) fields
    | `List items ->
        List.iter check_no_components_refs items
    | _ -> ()

  let test_complex_schema_ref_rewriting () =
    let routes = FastMCP.Utilities.OpenAPI.parse_openapi_to_http_routes TestData.complex_schema in
    List.iter (fun route ->
      let schema_defs = FastMCP.Utilities.OpenAPI.HttpRoute.get_schema_definitions route in
      check_no_components_refs schema_defs;
      let params = FastMCP.Utilities.OpenAPI.HttpRoute.get_parameters route in
      List.iter (fun param ->
        let schema = FastMCP.Utilities.OpenAPI.Parameter.get_schema param in
        check_no_components_refs schema
      ) params
    ) routes

  let test_complex_schema_list_users_query_param_limit () =
    let routes = FastMCP.Utilities.OpenAPI.parse_openapi_to_http_routes TestData.complex_schema in
    let route_map = List.fold_left (fun acc route ->
      match FastMCP.Utilities.OpenAPI.HttpRoute.get_operation_id route with
      | Some op_id -> (op_id, route) :: acc
      | None -> acc
    ) [] routes in
    let list_users = List.assoc "listUsers" route_map in
    let params = FastMCP.Utilities.OpenAPI.HttpRoute.get_parameters list_users in
    let limit_param = List.find_opt (fun p -> 
      FastMCP.Utilities.OpenAPI.Parameter.get_name p = "limit"
    ) params in
    match limit_param with
    | Some param ->
        Alcotest.(check string) "param location" "query" (FastMCP.Utilities.OpenAPI.Parameter.get_location param);
        let schema = FastMCP.Utilities.OpenAPI.Parameter.get_schema param in
        let default_val = Yojson.Safe.Util.member "default" schema |> Yojson.Safe.Util.to_int in
        Alcotest.(check int) "param default" 20 default_val
    | None -> Alcotest.fail "limit parameter not found"

  let test_complex_schema_list_users_query_param_limit_maximum () =
    let routes = FastMCP.Utilities.OpenAPI.parse_openapi_to_http_routes TestData.complex_schema in
    let route_map = List.fold_left (fun acc route ->
      match FastMCP.Utilities.OpenAPI.HttpRoute.get_operation_id route with
      | Some op_id -> (op_id, route) :: acc
      | None -> acc
    ) [] routes in
    let list_users = List.assoc "listUsers" route_map in
    let params = FastMCP.Utilities.OpenAPI.HttpRoute.get_parameters list_users in
    let limit_param = List.find_opt (fun p -> 
      FastMCP.Utilities.OpenAPI.Parameter.get_name p = "limit"
    ) params in
    match limit_param with
    | Some param ->
        let schema = FastMCP.Utilities.OpenAPI.Parameter.get_schema param in
        let maximum_val = Yojson.Safe.Util.member "maximum" schema |> Yojson.Safe.Util.to_int in
        Alcotest.(check int) "param maximum" 100 maximum_val
    | None -> Alcotest.fail "limit parameter not found"

  let test_complex_schema_get_user_path_param_existence () =
    let routes = FastMCP.Utilities.OpenAPI.parse_openapi_to_http_routes TestData.complex_schema in
    let route_map = List.fold_left (fun acc route ->
      match FastMCP.Utilities.OpenAPI.HttpRoute.get_operation_id route with
      | Some op_id -> (op_id, route) :: acc
      | None -> acc
    ) [] routes in
    let get_user = List.assoc "getUser" route_map in
    let params = FastMCP.Utilities.OpenAPI.HttpRoute.get_parameters get_user in
    let user_id_param = List.find_opt (fun p -> 
      FastMCP.Utilities.OpenAPI.Parameter.get_name p = "userId"
    ) params in
    match user_id_param with
    | Some param ->
        Alcotest.(check string) "param location" "path" (FastMCP.Utilities.OpenAPI.Parameter.get_location param)
    | None -> Alcotest.fail "userId parameter not found"

  let test_complex_schema_get_user_path_param_required () =
    let routes = FastMCP.Utilities.OpenAPI.parse_openapi_to_http_routes TestData.complex_schema in
    let route_map = List.fold_left (fun acc route ->
      match FastMCP.Utilities.OpenAPI.HttpRoute.get_operation_id route with
      | Some op_id -> (op_id, route) :: acc
      | None -> acc
    ) [] routes in
    let get_user = List.assoc "getUser" route_map in
    let params = FastMCP.Utilities.OpenAPI.HttpRoute.get_parameters get_user in
    let user_id_param = List.find_opt (fun p -> 
      FastMCP.Utilities.OpenAPI.Parameter.get_name p = "userId"
    ) params in
    match user_id_param with
    | Some param ->
        Alcotest.(check bool) "param required" true (FastMCP.Utilities.OpenAPI.Parameter.get_required param)
    | None -> Alcotest.fail "userId parameter not found"

  let test_complex_schema_get_user_path_param_format () =
    let routes = FastMCP.Utilities.OpenAPI.parse_openapi_to_http_routes TestData.complex_schema in
    let route_map = List.fold_left (fun acc route ->
      match FastMCP.Utilities.OpenAPI.HttpRoute.get_operation_id route with
      | Some op_id -> (op_id, route) :: acc
      | None -> acc
    ) [] routes in
    let get_user = List.assoc "getUser" route_map in
    let params = FastMCP.Utilities.OpenAPI.HttpRoute.get_parameters get_user in
    let user_id_param = List.find_opt (fun p -> 
      FastMCP.Utilities.OpenAPI.Parameter.get_name p = "userId"
    ) params in
    match user_id_param with
    | Some param ->
        let schema = FastMCP.Utilities.OpenAPI.Parameter.get_schema param in
        let format_val = Yojson.Safe.Util.member "format" schema |> Yojson.Safe.Util.to_string in
        Alcotest.(check string) "param format" "uuid" format_val
    | None -> Alcotest.fail "userId parameter not found"

  let test_complex_schema_create_order_request_body_presence () =
    let routes = FastMCP.Utilities.OpenAPI.parse_openapi_to_http_routes TestData.complex_schema in
    let route_map = List.fold_left (fun acc route ->
      match FastMCP.Utilities.OpenAPI.HttpRoute.get_operation_id route with
      | Some op_id -> (op_id, route) :: acc
      | None -> acc
    ) [] routes in
    let create_order = List.assoc "createOrder" route_map in
    let request_body = FastMCP.Utilities.OpenAPI.HttpRoute.get_request_body create_order in
    match request_body with
    | Some body ->
        Alcotest.(check bool) "request body required" true (FastMCP.Utilities.OpenAPI.RequestBody.get_required body)
    | None -> Alcotest.fail "Request body not found"

  let test_complex_schema_create_order_request_body_content_type () =
    let routes = FastMCP.Utilities.OpenAPI.parse_openapi_to_http_routes TestData.complex_schema in
    let route_map = List.fold_left (fun acc route ->
      match FastMCP.Utilities.OpenAPI.HttpRoute.get_operation_id route with
      | Some op_id -> (op_id, route) :: acc
      | None -> acc
    ) [] routes in
    let create_order = List.assoc "createOrder" route_map in
    let request_body = FastMCP.Utilities.OpenAPI.HttpRoute.get_request_body create_order in
    match request_body with
    | Some body ->
        let content_schema = FastMCP.Utilities.OpenAPI.RequestBody.get_content_schema body in
        Alcotest.(check bool) "has application/json" true (List.mem_assoc "application/json" content_schema)
    | None -> Alcotest.fail "Request body not found"

  let test_complex_schema_create_order_request_body_properties () =
    let routes = FastMCP.Utilities.OpenAPI.parse_openapi_to_http_routes TestData.complex_schema in
    let route_map = List.fold_left (fun acc route ->
      match FastMCP.Utilities.OpenAPI.HttpRoute.get_operation_id route with
      | Some op_id -> (op_id, route) :: acc
      | None -> acc
    ) [] routes in
    let create_order = List.assoc "createOrder" route_map in
    let request_body = FastMCP.Utilities.OpenAPI.HttpRoute.get_request_body create_order in
    match request_body with
    | Some body ->
        let content_schema = FastMCP.Utilities.OpenAPI.RequestBody.get_content_schema body in
        let json_schema = List.assoc "application/json" content_schema in
        let properties = Yojson.Safe.Util.member "properties" json_schema in
        let items_prop = Yojson.Safe.Util.member "items" properties in
        Alcotest.(check bool) "has items property" true (items_prop <> `Null)
    | None -> Alcotest.fail "Request body not found"

  let test_complex_schema_create_order_request_body_required_fields () =
    let routes = FastMCP.Utilities.OpenAPI.parse_openapi_to_http_routes TestData.complex_schema in
    let route_map = List.fold_left (fun acc route ->
      match FastMCP.Utilities.OpenAPI.HttpRoute.get_operation_id route with
      | Some op_id -> (op_id, route) :: acc
      | None -> acc
    ) [] routes in
    let create_order = List.assoc "createOrder" route_map in
    let request_body = FastMCP.Utilities.OpenAPI.HttpRoute.get_request_body create_order in
    match request_body with
    | Some body ->
        let content_schema = FastMCP.Utilities.OpenAPI.RequestBody.get_content_schema body in
        let json_schema = List.assoc "application/json" content_schema in
        let required = Yojson.Safe.Util.member "required" json_schema |> Yojson.Safe.Util.to_list |> List.map Yojson.Safe.Util.to_string in
        Alcotest.(check (list string)) "required fields" ["items"] required
    | None -> Alcotest.fail "Request body not found"
end

module TestBrokenReferences = struct
  let test_parser_handles_broken_references () =
    let routes = FastMCP.Utilities.OpenAPI.parse_openapi_to_http_routes TestData.schema_with_invalid_reference in
    Alcotest.(check bool) "routes is list" true (List.length routes >= 0);
    let broken_route = List.find_opt (fun r -> 
      FastMCP.Utilities.OpenAPI.HttpRoute.get_path r = "/broken-ref" && 
      FastMCP.Utilities.OpenAPI.HttpRoute.get_method r = "GET"
    ) routes in
    match broken_route with
    | Some route ->
        let op_id = FastMCP.Utilities.OpenAPI.HttpRoute.get_operation_id route in
        Alcotest.(check (option string)) "operation_id" (Some "brokenRef") op_id
    | None -> Alcotest.fail "broken route not found"
end

module TestContentParameters = struct
  let test_content_param_parameter_name () =
    let routes = FastMCP.Utilities.OpenAPI.parse_openapi_to_http_routes TestData.schema_with_content_params in
    let complex_params = List.hd routes in
    let params = FastMCP.Utilities.OpenAPI.HttpRoute.get_parameters complex_params in
    Alcotest.(check int) "parameter count" 1 (List.length params);
    let param = List.hd params in
    Alcotest.(check string) "param name" "filter" (FastMCP.Utilities.OpenAPI.Parameter.get_name param)

  let test_content_param_parameter_location () =
    let routes = FastMCP.Utilities.OpenAPI.parse_openapi_to_http_routes TestData.schema_with_content_params in
    let complex_params = List.hd routes in
    let params = FastMCP.Utilities.OpenAPI.HttpRoute.get_parameters complex_params in
    let param = List.hd params in
    Alcotest.(check string) "param location" "query" (FastMCP.Utilities.OpenAPI.Parameter.get_location param)

  let test_content_param_schema_properties_presence () =
    let routes = FastMCP.Utilities.OpenAPI.parse_openapi_to_http_routes TestData.schema_with_content_params in
    let complex_params = List.hd routes in
    let params = FastMCP.Utilities.OpenAPI.HttpRoute.get_parameters complex_params in
    let param = List.hd params in
    let schema = FastMCP.Utilities.OpenAPI.Parameter.get_schema param in
    let properties = Yojson.Safe.Util.member "properties" schema in
    let field_prop = Yojson.Safe.Util.member "field" properties in
    let operator_prop = Yojson.Safe.Util.member "operator" properties in
    let value_prop = Yojson.Safe.Util.member "value" properties in
    Alcotest.(check bool) "has field property" true (field_prop <> `Null);
    Alcotest.(check bool) "has operator property" true (operator_prop <> `Null);
    Alcotest.(check bool) "has value property" true (value_prop <> `Null)

  let test_content_param_schema_enum_presence () =
    let routes = FastMCP.Utilities.OpenAPI.parse_openapi_to_http_routes TestData.schema_with_content_params in
    let complex_params = List.hd routes in
    let params = FastMCP.Utilities.OpenAPI.HttpRoute.get_parameters complex_params in
    let param = List.hd params in
    let schema = FastMCP.Utilities.OpenAPI.Parameter.get_schema param in
    let properties = Yojson.Safe.Util.member "properties" schema in
    let operator_prop = Yojson.Safe.Util.member "operator" properties in
    let enum_val = Yojson.Safe.Util.member "enum" operator_prop in
    Alcotest.(check bool) "has enum property" true (enum_val <> `Null)
end

module TestHttpMethods = struct
  let test_http_method_presence method_name operation_id =
    let routes = FastMCP.Utilities.OpenAPI.parse_openapi_to_http_routes TestData.schema_all_http_methods in
    let method_route = List.find_opt (fun r -> 
      FastMCP.Utilities.OpenAPI.HttpRoute.get_method r = method_name
    ) routes in
    match method_route with
    | Some route ->
        let op_id = FastMCP.Utilities.OpenAPI.HttpRoute.get_operation_id route in
        Alcotest.(check (option string)) "operation_id" (Some operation_id) op_id;
        Alcotest.(check string) "path" "/resource" (FastMCP.Utilities.OpenAPI.HttpRoute.get_path route)
    | None -> Alcotest.fail (Printf.sprintf "%s route not found" method_name)

  let test_http_get_method_presence () =
    test_http_method_presence "GET" "getResource"

  let test_http_post_method_presence () =
    test_http_method_presence "POST" "createResource"

  let test_http_put_method_presence () =
    test_http_method_presence "PUT" "updateResource"

  let test_http_delete_method_presence () =
    test_http_method_presence "DELETE" "deleteResource"

  let test_http_patch_method_presence () =
    test_http_method_presence "PATCH" "patchResource"

  let test_http_head_method_presence () =
    test_http_method_presence "HEAD" "headResource"

  let test_http_options_method_presence () =
    test_http_method_presence "OPTIONS" "optionsResource"

  let test_http_trace_method_presence () =
    test_http_method_presence "TRACE" "traceResource"
end

module TestExternalReferences = struct
  let test_external_reference_raises_clear_error () =
    try
      let _ = FastMCP.Utilities.OpenAPI.parse_openapi_to_http_routes TestData.schema_with_external_reference in
      Alcotest.fail "Expected ValueError for external reference"
    with
    | Invalid_argument error_message ->
        Alcotest.(check bool) "contains error message" true (String.contains error_message "External or non-local reference not supported");
        Alcotest.(check bool) "contains reference URL" true (String.contains error_message "http://cyaninc.com/json-schemas/market-v1/product-constraints");
        Alcotest.(check bool) "contains FastMCP message" true (String.contains error_message "FastMCP only supports local schema references")
    | _ ->
        Alcotest.fail "Expected Invalid_argument exception"
end

let () =
  run "OpenAPI Advanced Tests" [
    "complex_schemas", [
      test_case "complex schema route count" `Quick TestComplexSchemas.test_complex_schema_route_count;
      test_case "complex schema ref rewriting" `Quick TestComplexSchemas.test_complex_schema_ref_rewriting;
      test_case "complex schema list users query param limit" `Quick TestComplexSchemas.test_complex_schema_list_users_query_param_limit;
      test_case "complex schema list users query param limit maximum" `Quick TestComplexSchemas.test_complex_schema_list_users_query_param_limit_maximum;
      test_case "complex schema get user path param existence" `Quick TestComplexSchemas.test_complex_schema_get_user_path_param_existence;
      test_case "complex schema get user path param required" `Quick TestComplexSchemas.test_complex_schema_get_user_path_param_required;
      test_case "complex schema get user path param format" `Quick TestComplexSchemas.test_complex_schema_get_user_path_param_format;
      test_case "complex schema create order request body presence" `Quick TestComplexSchemas.test_complex_schema_create_order_request_body_presence;
      test_case "complex schema create order request body content type" `Quick TestComplexSchemas.test_complex_schema_create_order_request_body_content_type;
      test_case "complex schema create order request body properties" `Quick TestComplexSchemas.test_complex_schema_create_order_request_body_properties;
      test_case "complex schema create order request body required fields" `Quick TestComplexSchemas.test_complex_schema_create_order_request_body_required_fields;
    ];
    "broken_references", [
      test_case "parser handles broken references" `Quick TestBrokenReferences.test_parser_handles_broken_references;
    ];
    "content_parameters", [
      test_case "content param parameter name" `Quick TestContentParameters.test_content_param_parameter_name;
      test_case "content param parameter location" `Quick TestContentParameters.test_content_param_parameter_location;
      test_case "content param schema properties presence" `Quick TestContentParameters.test_content_param_schema_properties_presence;
      test_case "content param schema enum presence" `Quick TestContentParameters.test_content_param_schema_enum_presence;
    ];
    "http_methods", [
      test_case "http get method presence" `Quick TestHttpMethods.test_http_get_method_presence;
      test_case "http post method presence" `Quick TestHttpMethods.test_http_post_method_presence;
      test_case "http put method presence" `Quick TestHttpMethods.test_http_put_method_presence;
      test_case "http delete method presence" `Quick TestHttpMethods.test_http_delete_method_presence;
      test_case "http patch method presence" `Quick TestHttpMethods.test_http_patch_method_presence;
      test_case "http head method presence" `Quick TestHttpMethods.test_http_head_method_presence;
      test_case "http options method presence" `Quick TestHttpMethods.test_http_options_method_presence;
      test_case "http trace method presence" `Quick TestHttpMethods.test_http_trace_method_presence;
    ];
    "external_references", [
      test_case "external reference raises clear error" `Quick TestExternalReferences.test_external_reference_raises_clear_error;
    ];
  ] 