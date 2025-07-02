open Alcotest
open Utilities.Openapi

(* Custom testable for comparing Yojson values *)
let yojson_testable =
  testable
    (fun fmt json -> Format.fprintf fmt "%s" (Yojson.Safe.to_string json))
    Yojson.Safe.equal

let test_http_method_conversion () =
  let test_method m =
    let str = string_of_http_method m in
    match http_method_of_string str with
    | Some m' -> check bool (Printf.sprintf "method %s roundtrip" str) true (m = m')
    | None -> Alcotest.fail (Printf.sprintf "Failed to convert %s back to method" str)
  in
  test_method `GET;
  test_method `POST;
  test_method `PUT;
  test_method `DELETE;
  test_method `PATCH;
  test_method `OPTIONS;
  test_method `HEAD;
  test_method `TRACE

let test_parameter_location_conversion () =
  let test_location l =
    let str = string_of_parameter_location l in
    match parameter_location_of_string str with
    | Some l' -> check bool (Printf.sprintf "location %s roundtrip" str) true (l = l')
    | None -> Alcotest.fail (Printf.sprintf "Failed to convert %s back to location" str)
  in
  test_location `Path;
  test_location `Query;
  test_location `Header;
  test_location `Cookie

let test_parameter_info_json_roundtrip () =
  let param = {
    name = "test_param";
    location = `Path;
    required = true;
    schema = `Assoc [("type", `String "string")];
    description = Some "Test parameter";
  } in
  let json = json_of_parameter_info param in
  match parameter_info_of_json json with
  | Some param' ->
      check string "name" param.name param'.name;
      check bool "location" true (param.location = param'.location);
      check bool "required" param.required param'.required;
      check yojson_testable "schema" param.schema param'.schema;
      check (option string) "description" param.description param'.description
  | None -> Alcotest.fail "Failed to parse parameter info from JSON"

let test_request_body_info_json_roundtrip () =
  let body = {
    required = true;
    content_schema = [("application/json", `Assoc [("type", `String "object")])];
    description = Some "Test request body";
  } in
  let json = json_of_request_body_info body in
  match request_body_info_of_json json with
  | Some body' ->
      check bool "required" body.required body'.required;
      check (list (pair string yojson_testable)) "content_schema" body.content_schema body'.content_schema;
      check (option string) "description" body.description body'.description
  | None -> Alcotest.fail "Failed to parse request body info from JSON"

let test_response_info_json_roundtrip () =
  let response = {
    description = Some "Test response";
    content_schema = [("application/json", `Assoc [("type", `String "object")])];
  } in
  let json = json_of_response_info response in
  match response_info_of_json json with
  | Some response' ->
      check (option string) "description" response.description response'.description;
      check (list (pair string yojson_testable)) "content_schema" response.content_schema response'.content_schema
  | None -> Alcotest.fail "Failed to parse response info from JSON"

let test_http_route_json_roundtrip () =
  let route = {
    path = "/test";
    http_method = `GET;
    operation_id = Some "test_op";
    summary = Some "Test operation";
    description = Some "Test description";
    tags = ["test"];
    parameters = [{
      name = "test_param";
      location = `Path;
      required = true;
      schema = `Assoc [("type", `String "string")];
      description = Some "Test parameter";
    }];
    request_body = Some {
      required = true;
      content_schema = [("application/json", `Assoc [("type", `String "object")])];
      description = Some "Test request body";
    };
    responses = [("200", {
      description = Some "Test response";
      content_schema = [("application/json", `Assoc [("type", `String "object")])];
    })];
    schema_definitions = [("Test", `Assoc [("type", `String "object")])];
  } in
  let json = json_of_http_route route in
  match http_route_of_json json with
  | Some route' ->
      check string "path" route.path route'.path;
      check bool "http_method" true (route.http_method = route'.http_method);
      check (option string) "operation_id" route.operation_id route'.operation_id;
      check (option string) "summary" route.summary route'.summary;
      check (option string) "description" route.description route'.description;
      check (list string) "tags" route.tags route'.tags;
      check int "parameters length" (List.length route.parameters) (List.length route'.parameters);
      check bool "request_body presence" (Option.is_some route.request_body) (Option.is_some route'.request_body);
      check int "responses length" (List.length route.responses) (List.length route'.responses);
      check (list (pair string yojson_testable)) "schema_definitions" route.schema_definitions route'.schema_definitions
  | None -> Alcotest.fail "Failed to parse HTTP route from JSON"

let () =
  run "OpenAPI Tests" [
    "conversions", [
      test_case "HTTP method conversion" `Quick test_http_method_conversion;
      test_case "Parameter location conversion" `Quick test_parameter_location_conversion;
    ];
    "json_roundtrip", [
      test_case "Parameter info JSON roundtrip" `Quick test_parameter_info_json_roundtrip;
      test_case "Request body info JSON roundtrip" `Quick test_request_body_info_json_roundtrip;
      test_case "Response info JSON roundtrip" `Quick test_response_info_json_roundtrip;
      test_case "HTTP route JSON roundtrip" `Quick test_http_route_json_roundtrip;
    ];
  ] 