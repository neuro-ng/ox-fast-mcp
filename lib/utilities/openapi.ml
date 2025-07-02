type http_method = [ `GET | `POST | `PUT | `DELETE | `PATCH | `OPTIONS | `HEAD | `TRACE ]
type parameter_location = [ `Path | `Query | `Header | `Cookie ]
type json_schema = Yojson.Safe.t

type parameter_info = {
  name: string;
  location: parameter_location;
  required: bool;
  schema: json_schema;
  description: string option;
}

type request_body_info = {
  required: bool;
  content_schema: (string * json_schema) list;  (* media_type * schema *)
  description: string option;
}

type response_info = {
  description: string option;
  content_schema: (string * json_schema) list;  (* media_type * schema *)
}

type http_route = {
  path: string;
  http_method: http_method;
  operation_id: string option;
  summary: string option;
  description: string option;
  tags: string list;
  parameters: parameter_info list;
  request_body: request_body_info option;
  responses: (string * response_info) list;  (* status_code * response_info *)
  schema_definitions: (string * json_schema) list;
}

(* Helper functions *)
let string_of_http_method = function
  | `GET -> "GET"
  | `POST -> "POST"
  | `PUT -> "PUT"
  | `DELETE -> "DELETE"
  | `PATCH -> "PATCH"
  | `OPTIONS -> "OPTIONS"
  | `HEAD -> "HEAD"
  | `TRACE -> "TRACE"

let http_method_of_string = function
  | "GET" -> Some `GET
  | "POST" -> Some `POST
  | "PUT" -> Some `PUT
  | "DELETE" -> Some `DELETE
  | "PATCH" -> Some `PATCH
  | "OPTIONS" -> Some `OPTIONS
  | "HEAD" -> Some `HEAD
  | "TRACE" -> Some `TRACE
  | _ -> None

let string_of_parameter_location = function
  | `Path -> "path"
  | `Query -> "query"
  | `Header -> "header"
  | `Cookie -> "cookie"

let parameter_location_of_string = function
  | "path" -> Some `Path
  | "query" -> Some `Query
  | "header" -> Some `Header
  | "cookie" -> Some `Cookie
  | _ -> None

(* Helper modules *)
module HttpRoute = struct
  let get_method route = string_of_http_method route.http_method
  let get_path route = route.path
  let get_operation_id route = route.operation_id
  let get_parameters route = route.parameters
  let get_request_body route = route.request_body
  let get_tags route = route.tags
end

module Parameter = struct
  let get_name param = param.name
  let get_location param = string_of_parameter_location param.location
  let get_required param = param.required
  let get_schema param = param.schema
end

module RequestBody = struct
  let get_required body = body.required
  let get_content_schema body = body.content_schema
end

(* JSON conversion functions *)
let parameter_info_of_json json =
  let name = Yojson.Safe.Util.member "name" json |> Yojson.Safe.Util.to_string in
  let location = Yojson.Safe.Util.member "in" json |> Yojson.Safe.Util.to_string |> parameter_location_of_string in
  let required = Yojson.Safe.Util.member "required" json |> Yojson.Safe.Util.to_bool_option |> Option.value ~default:false in
  let schema = match Yojson.Safe.Util.member "schema" json with
    | `Null -> (
      match Yojson.Safe.Util.member "content" json with
      | `Assoc content -> (
        let media_type_schema = List.find_map (fun (_, media_type) ->
          match Yojson.Safe.Util.member "schema" media_type with
          | `Null -> None
          | schema -> Some schema
        ) content in
        match media_type_schema with
        | Some schema -> schema
        | None -> `Null
      )
      | _ -> `Null
    )
    | schema -> schema
  in
  let description = Yojson.Safe.Util.member "description" json |> Yojson.Safe.Util.to_string_option in
    match location with
    | Some location -> Some {
        name;
        location;
        required;
        schema;
        description;
      }
    | None -> None

let rec deep_copy_json = function
  | `Assoc fields -> `Assoc (List.map (fun (k, v) -> (k, deep_copy_json v)) fields)
  | `List items -> `List (List.map deep_copy_json items)
  | `String s -> `String s
  | `Int i -> `Int i
  | `Float f -> `Float f
  | `Bool b -> `Bool b
  | `Null -> `Null

let request_body_info_of_json json =
  let required = Yojson.Safe.Util.member "required" json |> Yojson.Safe.Util.to_bool_option |> Option.value ~default:false in
  let content = Yojson.Safe.Util.member "content" json |> Yojson.Safe.Util.to_assoc in
  let content_schema = List.map (fun (media_type, schema) ->
    (media_type, deep_copy_json (Yojson.Safe.Util.member "schema" schema))
  ) content in
  let description = Yojson.Safe.Util.member "description" json |> Yojson.Safe.Util.to_string_option in
    Some {
      required;
      content_schema;
      description;
    }

let response_info_of_json json =
  let description = Yojson.Safe.Util.member "description" json |> Yojson.Safe.Util.to_string_option in
  let content = match Yojson.Safe.Util.member "content" json with
    | `Null -> []
    | content -> Yojson.Safe.Util.to_assoc content
  in
  let content_schema = List.map (fun (media_type, schema) ->
    (media_type, deep_copy_json (Yojson.Safe.Util.member "schema" schema))
  ) content in
    Some {
      description;
      content_schema;
    }

let json_of_parameter_info info =
  `Assoc [
    ("name", `String info.name);
    ("in", `String (string_of_parameter_location info.location));
    ("required", `Bool info.required);
    ("schema", info.schema);
    ("description", match info.description with Some d -> `String d | None -> `Null);
  ]

let json_of_request_body_info (info : request_body_info) =
  `Assoc [
    ("required", `Bool info.required);
    ("content", `Assoc (List.map (fun (media_type, schema) ->
      (media_type, `Assoc [("schema", schema)])
    ) info.content_schema));
    ("description", match info.description with Some d -> `String d | None -> `Null);
  ]

let json_of_response_info (info : response_info) =
  `Assoc [
    ("description", match info.description with Some d -> `String d | None -> `Null);
    ("content", `Assoc (List.map (fun (media_type, schema) ->
      (media_type, `Assoc [("schema", schema)])
    ) info.content_schema));
  ]

let json_of_http_route route =
  `Assoc [
    ("path", `String route.path);
    ("method", `String (string_of_http_method route.http_method));
    ("operation_id", match route.operation_id with Some id -> `String id | None -> `Null);
    ("summary", match route.summary with Some s -> `String s | None -> `Null);
    ("description", match route.description with Some d -> `String d | None -> `Null);
    ("tags", `List (List.map (fun t -> `String t) route.tags));
    ("parameters", `List (List.map json_of_parameter_info route.parameters));
    ("request_body", match route.request_body with Some rb -> json_of_request_body_info rb | None -> `Null);
    ("responses", `Assoc (List.map (fun (code, info) -> (code, json_of_response_info info)) route.responses));
    ("schema_definitions", `Assoc route.schema_definitions);
  ]

let http_route_of_json json =
  let open Yojson.Safe.Util in
  try
    let path = member "path" json |> to_string in
    let method_str = member "method" json |> to_string in
    let http_method = http_method_of_string method_str in
    let operation_id = member "operation_id" json |> to_string_option in
    let summary = member "summary" json |> to_string_option in
    let description = member "description" json |> to_string_option in
    let tags = member "tags" json |> to_list |> List.map to_string in
    let parameters = member "parameters" json |> to_list |> List.filter_map parameter_info_of_json in
    let request_body = member "request_body" json |> request_body_info_of_json in
    let responses = member "responses" json |> to_assoc |> List.filter_map (fun (code, resp) ->
      match response_info_of_json resp with
      | Some info -> Some (code, info)
      | None -> None
    ) in
    let schema_definitions = member "schema_definitions" json |> to_assoc in
    match http_method with
    | Some http_method -> Some {
        path;
        http_method;
        operation_id;
        summary;
        description;
        tags;
        parameters;
        request_body;
        responses;
        schema_definitions;
      }
    | None -> None
  with _ -> None

(* Schema manipulation functions *)
let rec resolve_ref schema ref_path =
  let parts = String.split_on_char '/' (String.sub ref_path 2 (String.length ref_path - 2)) in
  let rec resolve_parts target = function
    | [] -> target
    | part :: rest ->
        match target with
        | `Assoc fields -> (
          match List.assoc_opt part fields with
          | Some next_target -> resolve_parts next_target rest
          | None -> raise (Invalid_argument (Printf.sprintf "Reference part '%s' not found" part))
        )
        | _ -> raise (Invalid_argument (Printf.sprintf "Cannot traverse part '%s'" part))
  in
  try
    let resolved = resolve_parts schema parts in
    match resolved with
    | `Assoc fields when List.mem_assoc "$ref" fields ->
        let nested_ref = List.assoc "$ref" fields in
        resolve_ref schema (match nested_ref with `String s -> s | _ -> ref_path)
    | _ -> resolved
  with Invalid_argument msg ->
    log_warning (fun m -> m "Failed to resolve reference '%s': %s" ref_path msg);
    `Null

let rec replace_ref_with_defs schema json =
  match json with
  | `Assoc fields ->
      let process_field (key, value) =
        match key, value with
        | "$ref", `String ref ->
            if String.starts_with ~prefix:"#/components/schemas/" ref then
              let schema_name = String.sub ref 21 (String.length ref - 21) in
              (key, `String ("#/$defs/" ^ schema_name))
            else if String.starts_with ~prefix:"#/" ref then
              let resolved = resolve_ref schema ref in
              if resolved = `Null then
                (key, value)
              else
                (key, replace_ref_with_defs schema resolved)
            else (
              log_warning (fun m -> m "External reference not supported: %s" ref);
              (key, `String "#/$defs/external_ref")
            )
        | _, value -> (key, replace_ref_with_defs schema value)
      in
      `Assoc (List.map process_field fields)
  | `List items ->
      `List (List.map (replace_ref_with_defs schema) items)
  | other -> other

let combine_schemas route =
  let combined = `Assoc [
    ("type", `String "object");
    ("properties", `Assoc []);
    ("required", `List []);
  ] in
  let add_schema schema combined =
    match schema, combined with
    | `Assoc _, `Assoc combined_fields ->
        let properties = Yojson.Safe.Util.member "properties" schema |> Yojson.Safe.Util.to_assoc in
        let required = Yojson.Safe.Util.member "required" schema |> Yojson.Safe.Util.to_list in
        let new_properties = match List.assoc_opt "properties" combined_fields with
          | Some (`Assoc existing) -> `Assoc (existing @ properties)
          | _ -> `Assoc properties
        in
        let new_required = match List.assoc_opt "required" combined_fields with
          | Some (`List existing) -> `List (existing @ required)
          | _ -> `List required
        in
        `Assoc (
          ("type", `String "object") ::
          ("properties", new_properties) ::
          ("required", new_required) ::
          List.filter (fun (k, _) -> k <> "properties" && k <> "required") combined_fields
        )
    | _ -> combined
  in
  let combined_with_params = List.fold_left (fun acc param ->
    add_schema param.schema acc
  ) combined route.parameters in
  let combined_with_body = match route.request_body with
    | Some body ->
        List.fold_left (fun acc (_, schema) ->
          add_schema schema acc
        ) combined_with_params body.content_schema
    | None -> combined_with_params
  in
  let combined_with_responses = List.fold_left (fun acc (_, resp) ->
    List.fold_left (fun acc (_, schema) ->
      add_schema schema acc
    ) acc resp.content_schema
  ) combined_with_body route.responses in
  let combined_with_defs = `Assoc (
    List.filter (fun (k, _) -> k <> "$defs") (Yojson.Safe.Util.to_assoc combined_with_responses) @
    [("$defs", `Assoc route.schema_definitions)]
  ) in
  let with_refs = replace_ref_with_defs combined_with_defs combined_with_defs in
  Json_schema.compress_schema 
    ~prune_defs:true 
    ~prune_additional_properties:true 
    ~prune_titles:true 
    with_refs

let validate_schema_enum schema =
  match Yojson.Safe.Util.member "enum" schema with
  | `List values -> 
      let validate_enum_value = function
        | `String _ | `Int _ | `Float _ | `Bool _ -> true
        | _ -> false
      in
      List.for_all validate_enum_value values
  | `Null -> true
  | _ -> false

let validate_schema_format schema =
  match Yojson.Safe.Util.member "format" schema with
  | `String format -> (
    match format with
    | "date-time" | "date" | "time" | "duration"
    | "email" | "idn-email"
    | "hostname" | "idn-hostname"
    | "ipv4" | "ipv6"
    | "uri" | "uri-reference" | "iri" | "iri-reference"
    | "uuid"
    | "json-pointer" | "relative-json-pointer"
    | "regex"
    | "byte" | "binary"
    | "password" -> true
    | _ -> false
  )
  | `Null -> true
  | _ -> false

let validate_schema schema =
  try
    let version = Yojson.Safe.Util.member "openapi" schema |> Yojson.Safe.Util.to_string in
    let paths = Yojson.Safe.Util.member "paths" schema in
    match paths with
    | `Assoc _ -> 
        if String.starts_with ~prefix:"3.0" version || String.starts_with ~prefix:"3.1" version then
          let rec validate_schema_node = function
            | `Assoc fields ->
                List.for_all (fun (_, value) -> validate_schema_node value) fields &&
                (match Yojson.Safe.Util.member "enum" schema with
                 | `List _ -> validate_schema_enum schema
                 | _ -> true) &&
                (match Yojson.Safe.Util.member "format" schema with
                 | `String _ -> validate_schema_format schema
                 | _ -> true)
            | `List items -> List.for_all validate_schema_node items
            | _ -> true
          in
          if validate_schema_node schema then
            Ok version
          else
            Error "Invalid schema: invalid enum values or format"
        else
          Error (Printf.sprintf "Unsupported OpenAPI version: %s. Only 3.0.x and 3.1.x are supported." version)
    | _ -> Error "Invalid OpenAPI schema: missing or invalid 'paths' object"
  with e ->
    Error (Printf.sprintf "Invalid OpenAPI schema: %s" (Printexc.to_string e))

let parse_openapi_to_http_routes schema =
  match validate_schema schema with
  | Error msg ->
      log_error (fun m -> m "%s" msg);
      []
  | Ok version ->
      log_info (fun m -> m "Parsing OpenAPI schema version: %s" version);
      let paths = Yojson.Safe.Util.member "paths" schema |> Yojson.Safe.Util.to_assoc in
      let components = Yojson.Safe.Util.member "components" schema in
      let schema_definitions = match components with
        | `Null -> []
        | _ -> match Yojson.Safe.Util.member "schemas" components with
            | `Assoc defs -> defs
            | _ -> []
      in
      let extract_operation path_str (method_str, operation) =
        let http_method = http_method_of_string (String.uppercase_ascii method_str) in
        match http_method with
        | None -> 
            log_warning (fun m -> m "Invalid HTTP method: %s" method_str);
            None
        | Some http_method ->
            let operation_id = Yojson.Safe.Util.member "operationId" operation |> Yojson.Safe.Util.to_string_option in
            let summary = Yojson.Safe.Util.member "summary" operation |> Yojson.Safe.Util.to_string_option in
            let description = Yojson.Safe.Util.member "description" operation |> Yojson.Safe.Util.to_string_option in
            let tags = Yojson.Safe.Util.member "tags" operation |> Yojson.Safe.Util.to_list |> List.map Yojson.Safe.Util.to_string in
            log_debug (fun m -> m "Processing operation: %s %s (ID: %s)" 
              (string_of_http_method http_method) 
              path_str 
              (Option.value operation_id ~default:"<no-id>"));
            let parameters = 
              let path_params = match Yojson.Safe.Util.member "parameters" operation with
                | `List params -> params
                | _ -> []
              in
              let common_params = match Yojson.Safe.Util.member "parameters" (List.assoc path_str paths) with
                | `List params -> params
                | _ -> []
              in
              List.filter_map parameter_info_of_json (path_params @ common_params)
            in
            let request_body = 
              match Yojson.Safe.Util.member "requestBody" operation with
              | `Null -> None
              | body -> request_body_info_of_json body
            in
            let responses = Yojson.Safe.Util.member "responses" operation |> Yojson.Safe.Util.to_assoc |> List.filter_map (fun (code, resp) ->
              match response_info_of_json resp with
              | Some info -> Some (code, info)
              | None -> None
            ) in
            Some {
              path = path_str;
              http_method;
              operation_id;
              summary;
              description;
              tags;
              parameters;
              request_body;
              responses;
              schema_definitions;
            }
      in
      let extract_path_operations (path_str, path_item) =
        let operations = Yojson.Safe.Util.to_assoc path_item |> 
          List.filter (fun (k, _) -> http_method_of_string (String.uppercase_ascii k) <> None) in
        List.filter_map (extract_operation path_str) operations
      in
      try
        let routes = List.concat_map extract_path_operations paths in
        log_info (fun m -> m "Successfully extracted %d routes" (List.length routes));
        routes
      with e -> 
        log_error (fun m -> m "Failed to parse OpenAPI schema: %s" (Printexc.to_string e));
        []

let rec clean_schema_for_display schema =
  match schema with
  | `Assoc fields ->
      let fields_to_remove = [
        "allOf"; "anyOf"; "oneOf"; "not"; "nullable"; "discriminator";
        "readOnly"; "writeOnly"; "deprecated"; "xml"; "externalDocs"
      ] in
      let cleaned_fields = List.filter (fun (k, _) -> 
        not (List.mem k fields_to_remove)
      ) fields in
      let process_field (key, value) = match key with
        | "properties" -> (key, clean_schema_for_display value)
        | "items" -> (key, clean_schema_for_display value)
        | "additionalProperties" when value <> `Bool true -> 
            (key, clean_schema_for_display value)
        | _ -> (key, value)
      in
      `Assoc (List.map process_field cleaned_fields)
  | `List items -> `List (List.map clean_schema_for_display items)
  | other -> other

let rec generate_example_from_schema schema =
  match schema with
  | `Assoc fields -> (
    (* Check for predefined values *)
    match List.assoc_opt "default" fields with
    | Some default -> default
    | None ->
      match List.assoc_opt "enum" fields with
      | Some (`List (x::_)) -> x
      | _ ->
        match List.assoc_opt "examples" fields with
        | Some (`List (x::_)) -> x
        | _ ->
          match List.assoc_opt "example" fields with
          | Some example -> example
          | None ->
            (* Generate based on type *)
            match List.assoc_opt "type" fields with
            | Some (`String "object") -> (
              match List.assoc_opt "properties" fields with
              | Some (`Assoc props) ->
                  let required = match List.assoc_opt "required" fields with
                    | Some (`List reqs) -> List.map (function 
                      | `String s -> s 
                      | _ -> "") reqs
                    | _ -> []
                  in
                  let first_props = List.take (min 3 (List.length props)) props in
                  let example_props = List.map (fun (name, schema) ->
                    (name, generate_example_from_schema schema)
                  ) first_props in
                  `Assoc (
                    List.filter (fun (name, _) -> 
                      List.mem name required
                    ) example_props
                  )
              | _ -> `Assoc [("key", `String "value")]
            )
            | Some (`String "array") -> (
              match List.assoc_opt "items" fields with
              | Some items -> `List [generate_example_from_schema items]
              | _ -> `List [`String "example_item"]
            )
            | Some (`String "string") -> (
              match List.assoc_opt "format" fields with
              | Some (`String "date-time") -> `String "2024-01-01T12:00:00Z"
              | Some (`String "date") -> `String "2024-01-01"
              | Some (`String "email") -> `String "user@example.com"
              | Some (`String "uuid") -> 
                  `String "123e4567-e89b-12d3-a456-426614174000"
              | Some (`String "byte") -> `String "ZXhhbXBsZQ=="
              | _ -> `String "string"
            )
            | Some (`String "integer") -> `Int 1
            | Some (`String "number") -> `Float 1.5
            | Some (`String "boolean") -> `Bool true
            | Some (`String "null") -> `Null
            | _ -> `String "unknown_type"
    )
  | other -> other

let format_json_for_description data =
  try
    let json_str = Yojson.Safe.pretty_to_string ~std:true data in
    "```json\n" ^ json_str ^ "\n```"
  with _ ->
    "```\nCould not serialize to JSON\n```"

let format_description_with_responses ?(parameters=[]) ?request_body ~responses base_description =
  let buf = Buffer.create 256 in
  Buffer.add_string buf base_description;

  (* Add parameter information *)
  let path_params = List.filter (fun p -> p.location = `Path) parameters in
  let query_params = List.filter (fun p -> p.location = `Query) parameters in

  if path_params <> [] then (
    Buffer.add_string buf "\n\n**Path Parameters:**";
    List.iter (fun param ->
      let required_marker = if param.required then " (Required)" else "" in
      let desc = match param.description with 
        | Some d -> d 
        | None -> "No description." 
      in
      Buffer.add_string buf (Printf.sprintf "\n- **%s**%s: %s" 
        param.name required_marker desc)
    ) path_params
  );

  if query_params <> [] then (
    Buffer.add_string buf "\n\n**Query Parameters:**";
    List.iter (fun param ->
      let required_marker = if param.required then " (Required)" else "" in
      let desc = match param.description with 
        | Some d -> d 
        | None -> "No description." 
      in
      Buffer.add_string buf (Printf.sprintf "\n- **%s**%s: %s" 
        param.name required_marker desc)
    ) query_params
  );

  (* Add request body information *)
  (match request_body with
  | Some rb when rb.description <> None ->
      Buffer.add_string buf "\n\n**Request Body:**";
      let required_marker = if rb.required then " (Required)" else "" in
      Buffer.add_string buf (Printf.sprintf "\n%s%s" 
        (Option.value ~default:"" rb.description) required_marker);

      (* Add request body property descriptions *)
      let json_content = List.assoc_opt "application/json" rb.content_schema in
      (match json_content with
      | Some (`Assoc fields) -> (
          match List.assoc_opt "properties" fields with
          | Some (`Assoc props) ->
              Buffer.add_string buf "\n\n**Request Properties:**";
              List.iter (fun (prop_name, prop_schema) ->
                match prop_schema with
                | `Assoc fields -> (
                    match List.assoc_opt "description" fields with
                    | Some (`String desc) ->
                        let required = match List.assoc_opt "required" fields with
                          | Some (`List reqs) -> List.exists (function 
                              | `String s -> s = prop_name 
                              | _ -> false) reqs
                          | _ -> false
                        in
                        let req_mark = if required then " (Required)" else "" in
                        Buffer.add_string buf (Printf.sprintf "\n- **%s**%s: %s" 
                          prop_name req_mark desc)
                    | _ -> ()
                  )
                | _ -> ()
              ) props
          | _ -> ()
        )
      | _ -> ()
      )
  | _ -> ()
  );

  (* Add response information *)
  if responses <> [] then (
    Buffer.add_string buf "\n\n**Responses:**";
    let success_codes = ["200"; "201"; "202"; "204"] in
    let success_status = List.find_opt (fun (code, _) -> 
      List.mem code success_codes
    ) responses in

    List.iter (fun (status_code, resp_info) ->
      let status_marker = match success_status with
        | Some (code, _) when code = status_code -> " (Success)"
        | _ -> ""
      in
      let desc = match resp_info.description with 
        | Some d -> d 
        | None -> "No description." 
      in
      Buffer.add_string buf (Printf.sprintf "\n- **%s**%s: %s" 
        status_code status_marker desc);

      (* Process content schemas *)
      match List.assoc_opt "application/json" resp_info.content_schema with
      | Some schema ->
          Buffer.add_string buf "\n  - Content-Type: `application/json`";
          
          (* Add response property descriptions *)
          (match schema with
          | `Assoc fields -> (
              match List.assoc_opt "type" fields, List.assoc_opt "properties" fields with
              | Some (`String "array"), Some (`Assoc items_props) ->
                  Buffer.add_string buf "\n  - **Response Item Properties:**";
                  List.iter (fun (prop_name, prop_schema) ->
                    match prop_schema with
                    | `Assoc fields ->
                        (match List.assoc_opt "description" fields with
                        | Some (`String desc) ->
                            Buffer.add_string buf (Printf.sprintf "\n    - **%s**: %s" 
                              prop_name desc)
                        | _ -> ())
                    | _ -> ()
                  ) items_props
              | _, Some (`Assoc props) ->
                  Buffer.add_string buf "\n  - **Response Properties:**";
                  List.iter (fun (prop_name, prop_schema) ->
                    match prop_schema with
                    | `Assoc fields ->
                        (match List.assoc_opt "description" fields with
                        | Some (`String desc) ->
                            Buffer.add_string buf (Printf.sprintf "\n    - **%s**: %s" 
                              prop_name desc)
                        | _ -> ())
                    | _ -> ()
                  ) props
              | _ -> ()
            );

            (* Generate Example *)
            let example = generate_example_from_schema schema in
            if example <> `String "unknown_type" && example <> `Null then (
              Buffer.add_string buf "\n  - **Example:**\n";
              Buffer.add_string buf (format_json_for_description example)
            )
          | _ -> ()
      | _ -> ()
    ) responses
  );

  Buffer.contents buf