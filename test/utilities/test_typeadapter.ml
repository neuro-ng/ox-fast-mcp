open Alcotest
open Utilities.Types

(** Test modules and types for complex function testing *)

(** Complex model equivalent to Python BaseModel *)

type complex_model = {
  x : int;
  y : (int * string) list; (* OCaml equivalent of dict[int, str] *)
}

(** Function signature representation *)
type function_parameter = {
  param_name : string;
  param_type : string;
  param_description : string option;
  param_required : bool;
  param_default : json option;
}

type function_signature = {
  func_name : string;
  parameters : function_parameter list;
  return_type : string;
  description : string option;
}

(** Type adapter for OCaml functions *)
type 'a type_adapter = {
  signature : function_signature;
  validate_and_call : (string * json) list -> (json, string) result;
  json_schema : unit -> json;
}

(** Create type adapter for simple functions *)
let create_simple_func_adapter () =
  let signature = {
    name = "simple_func";
    description = Some "A simple test function";
    parameters = [
      create_parameter 
        ~name:"x" 
        ~type_:"integer" 
        ~required:true 
        ();
      create_parameter 
        ~name:"y" 
        ~type_:"string" 
        ~required:false 
        ~default:(`String "default") 
        ();
    ];
    return_type = "string";
    is_async = false;
    is_static = false;
    is_method = false;
    is_class_method = false;
  } in
  
  let validate_and_call args =
    try
      let x_val = match List.assoc_opt "x" args with
        | Some (`Int i) -> i
        | Some (`String s) -> int_of_string s
        | _ -> failwith "x parameter required and must be integer"
      in
      let y_val = match List.assoc_opt "y" args with
        | Some (`String s) -> s
        | None -> "default"
        | _ -> failwith "y parameter must be string"
      in
      let result = string_of_int x_val ^ "-" ^ y_val in
      Ok (`String result)
    with
    | Failure msg -> Error msg
    | _ -> Error "Validation failed"
  in
  
  let json_schema () =
    `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("x", `Assoc [("type", `String "integer")]);
        ("y", `Assoc [
          ("type", `String "string");
          ("default", `String "default")
        ])
      ]);
      ("required", `List [`String "x"]);
      ("description", `String "A simple test function")
    ]
  in
  
  { signature; validate_and_call; json_schema }

(** Create type adapter for functions without annotations *)
let create_unannotated_func_adapter () =
  let signature = {
    name = "unannotated_func";
    description = None;
    parameters = [
      create_parameter 
        ~name:"x" 
        ~type_:"any" 
        ~required:true 
        ();
      create_parameter 
        ~name:"y" 
        ~type_:"any" 
        ~required:true 
        ();
    ];
    return_type = "any";
    is_async = false;
    is_static = false;
    is_method = false;
    is_class_method = false;
  } in
  
  let validate_and_call args =
    try
      let x_val = match List.assoc_opt "x" args with
        | Some json_val -> json_val
        | None -> failwith "x parameter required"
      in
      let y_val = match List.assoc_opt "y" args with
        | Some json_val -> json_val
        | None -> failwith "y parameter required"
      in
      (* Simulate string concatenation behavior *)
      let result = match x_val, y_val with
        | `String s1, `String s2 -> s1 ^ s2
        | `Int i, `String s -> string_of_int i ^ s
        | `String s, `Int i -> s ^ string_of_int i
        | _ -> "unknown"
      in
      Ok (`String result)
    with
    | Failure msg -> Error msg
    | _ -> Error "Validation failed"
  in
  
  let json_schema () =
    `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("x", `Assoc [("type", `String "any")]);
        ("y", `Assoc [("type", `String "any")])
      ]);
      ("required", `List [`String "x"; `String "y"])
    ]
  in
  
  { signature; validate_and_call; json_schema }

(** Create type adapter for complex model functions *)
let create_complex_model_func_adapter () =
  let signature = {
    name = "complex_model_func";
    description = Some "Function that takes and returns a complex model";
    parameters = [
      create_parameter 
        ~name:"model" 
        ~type_:"complex_model" 
        ~description:"A complex model with nested data" 
        ~required:true 
        ();
    ];
    return_type = "complex_model";
    is_async = false;
    is_static = false;
    is_method = false;
    is_class_method = false;
  } in
  
  let validate_and_call args =
    try
      let model_val = match List.assoc_opt "model" args with
        | Some (`Assoc fields) ->
          let x = match List.assoc_opt "x" fields with
            | Some (`Int i) -> i
            | _ -> failwith "x field required and must be integer"
          in
          let y = match List.assoc_opt "y" fields with
            | Some (`Assoc pairs) ->
              List.map (fun (k, v) -> match v with
                | `String s -> (int_of_string k, s)
                | _ -> failwith "y field values must be strings"
              ) pairs
            | _ -> []
          in
          { x; y }
        | _ -> failwith "model parameter required and must be object"
      in
      
      (* Return the validated model as JSON *)
      let result = `Assoc [
        ("x", `Int model_val.x);
        ("y", `Assoc (List.map (fun (k, v) -> (string_of_int k, `String v)) model_val.y))
      ] in
      Ok result
    with
    | Failure msg -> Error msg
    | _ -> Error "Complex model validation failed"
  in
  
  let json_schema () =
    `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("model", `Assoc [
          ("type", `String "object");
          ("properties", `Assoc [
            ("x", `Assoc [("type", `String "integer")]);
            ("y", `Assoc [
              ("type", `String "object");
              ("additionalProperties", `Assoc [("type", `String "string")])
            ])
          ]);
          ("required", `List [`String "x"])
        ])
      ]);
      ("required", `List [`String "model"])
    ]
  in
  
  { signature; validate_and_call; json_schema }

(** Create type adapter for functions with many parameters *)
let create_many_params_func_adapter () =
  let signature = {
    name = "many_params_func";
    description = Some "Function with many parameters for testing pruning";
    parameters = [
      create_parameter 
        ~name:"keep_this" 
        ~type_:"integer" 
        ~required:true 
        ();
      create_parameter 
        ~name:"skip_this" 
        ~type_:"string" 
        ~required:true 
        ();
      create_parameter 
        ~name:"also_keep" 
        ~type_:"number" 
        ~required:true 
        ();
      create_parameter 
        ~name:"also_skip" 
        ~type_:"boolean" 
        ~required:true 
        ();
    ];
    return_type = "array";
    is_async = false;
    is_static = false;
    is_method = false;
    is_class_method = false;
  } in
  
  let validate_and_call args =
    try
      let keep_this = match List.assoc_opt "keep_this" args with
        | Some (`Int i) -> `Int i
        | _ -> failwith "keep_this required"
      in
      let skip_this = match List.assoc_opt "skip_this" args with
        | Some (`String s) -> `String s
        | _ -> `String "default"
      in
      let also_keep = match List.assoc_opt "also_keep" args with
        | Some (`Float f) -> `Float f
        | Some (`Int i) -> `Float (float_of_int i)
        | _ -> failwith "also_keep required"
      in
      let also_skip = match List.assoc_opt "also_skip" args with
        | Some (`Bool b) -> `Bool b
        | _ -> `Bool false
      in
      Ok (`List [keep_this; skip_this; also_keep; also_skip])
    with
    | Failure msg -> Error msg
    | _ -> Error "Many params validation failed"
  in
  
  let json_schema () =
    `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("keep_this", `Assoc [("type", `String "integer")]);
        ("skip_this", `Assoc [("type", `String "string")]);
        ("also_keep", `Assoc [("type", `String "number")]);
        ("also_skip", `Assoc [("type", `String "boolean")])
      ]);
      ("required", `List [`String "keep_this"; `String "skip_this"; `String "also_keep"; `String "also_skip"])
    ]
  in
  
  { signature; validate_and_call; json_schema }

(** Schema compression function (equivalent to Python's compress_schema) *)
let compress_schema schema ~prune_params =
  match schema with
  | `Assoc fields ->
    let filtered_fields = List.map (fun (key, value) ->
      match key, value with
      | "properties", `Assoc props ->
        let filtered_props = List.filter (fun (prop_name, _) ->
          not (List.mem prop_name prune_params)
        ) props in
        (key, `Assoc filtered_props)
      | "required", `List required_list ->
        let filtered_required = List.filter (function
          | `String param_name -> not (List.mem param_name prune_params)
          | _ -> true
        ) required_list in
        (key, `List filtered_required)
      | _ -> (key, value)
    ) fields in
    `Assoc filtered_fields
  | _ -> schema

(** Test complex function runtime argument validation (non-JSON) *)
let test_complex_function_runtime_arg_validation_non_json () =
  let type_adapter = create_simple_func_adapter () in
  
  (* Test with minimum required arguments *)
  let args = [("x", `Int 1)] in
  let result = type_adapter.validate_and_call args in
  (match result with
   | Ok (`String s) -> check string "minimum args result" "1-default" s
   | _ -> fail "Expected successful validation with default");
  
  (* Test with all arguments *)
  let args = [("x", `Int 1); ("y", `String "hello")] in
  let result = type_adapter.validate_and_call args in
  (match result with
   | Ok (`String s) -> check string "all args result" "1-hello" s
   | _ -> fail "Expected successful validation with all args");
  
  (* Test with invalid types *)
  let args = [("x", `String "not an int")] in
  let result = type_adapter.validate_and_call args in
  (match result with
   | Error _ -> check bool "invalid type error" true true
   | Ok _ -> fail "Expected validation error for invalid type")

(** Test missing annotation handling *)
let test_missing_annotation () =
  let type_adapter = create_unannotated_func_adapter () in
  let args = [("x", `String "1"); ("y", `String "2")] in
  let result = type_adapter.validate_and_call args in
  (match result with
   | Ok (`String s) -> check string "unannotated result" "12" s
   | _ -> fail "Expected string concatenation for unannotated function")

(** Test conversion of string to complex type *)
let test_convert_str_to_complex_type () =
  let type_adapter = create_complex_model_func_adapter () in
  let input_data = `Assoc [
    ("x", `Int 1);
    ("y", `Assoc [("1", `String "hello")])
  ] in
  
  let args = [("model", input_data)] in
  let result = type_adapter.validate_and_call args in
  (match result with
   | Ok (`Assoc fields) ->
     let x_val = List.assoc "x" fields in
     let y_val = List.assoc "y" fields in
     check bool "complex model x field" true (x_val = `Int 1);
     check bool "complex model y field" true 
       (match y_val with `Assoc [("1", `String "hello")] -> true | _ -> false)
   | _ -> fail "Expected successful complex model validation")

(** Test parameter skipping/pruning *)
let test_skip_names () =
  let type_adapter = create_many_params_func_adapter () in
  let schema = type_adapter.json_schema () in
  let pruned_schema = compress_schema schema ~prune_params:["skip_this"; "also_skip"] in
  
  (* Check that only desired parameters remain *)
  (match pruned_schema with
   | `Assoc fields ->
     let properties = List.assoc "properties" fields in
     (match properties with
      | `Assoc props ->
        check bool "keep_this present" true (List.mem_assoc "keep_this" props);
        check bool "also_keep present" true (List.mem_assoc "also_keep" props);
        check bool "skip_this absent" false (List.mem_assoc "skip_this" props);
        check bool "also_skip absent" false (List.mem_assoc "also_skip" props)
      | _ -> fail "Expected properties object");
     
     (* Check required array *)
     let required = List.assoc "required" fields in
     (match required with
      | `List req_list ->
        let has_skip_this = List.exists (function `String "skip_this" -> true | _ -> false) req_list in
        let has_also_skip = List.exists (function `String "also_skip" -> true | _ -> false) req_list in
        check bool "skip_this not in required" false has_skip_this;
        check bool "also_skip not in required" false has_also_skip
      | _ -> fail "Expected required array")
   | _ -> fail "Expected schema object")

(** Test lambda function equivalent *)
let test_lambda_function () =
  (* Create a simple lambda-like function adapter *)
  let signature = {
    name = "lambda_func";
    description = None;
    parameters = [
      create_parameter 
        ~name:"x" 
        ~type_:"any" 
        ~required:true 
        ();
      create_parameter 
        ~name:"y" 
        ~type_:"integer" 
        ~required:false 
        ~default:(`Int 5) 
        ();
    ];
    return_type = "string";
    is_async = false;
    is_static = false;
    is_method = false;
    is_class_method = false;
  } in
  
  let validate_and_call args =
    try
      let x_val = match List.assoc_opt "x" args with
        | Some (`String s) -> s
        | Some json_val -> Yojson.Safe.to_string json_val
        | None -> failwith "x parameter required"
      in
      let _y_val = match List.assoc_opt "y" args with
        | Some (`Int i) -> i
        | None -> 5
        | _ -> failwith "y parameter must be integer"
      in
      Ok (`String x_val)
    with
    | Failure msg -> Error msg
    | _ -> Error "Lambda validation failed"
  in
  
  let type_adapter = { signature; validate_and_call; json_schema = fun () -> `Null } in
  
  (* Test basic calls *)
  let result1 = type_adapter.validate_and_call [("x", `String "hello")] in
  (match result1 with
   | Ok (`String s) -> check string "lambda result 1" "hello" s
   | _ -> fail "Expected successful lambda call");
  
  let result2 = type_adapter.validate_and_call [("x", `String "hello"); ("y", `String "world")] in
  (match result2 with
   | Error _ -> check bool "lambda invalid y type" true true
   | Ok _ -> fail "Expected error for invalid y type");
  
  (* Test missing required arg *)
  let result3 = type_adapter.validate_and_call [("y", `Int 10)] in
  (match result3 with
   | Error _ -> check bool "lambda missing x" true true
   | Ok _ -> fail "Expected error for missing required parameter")

(** Test basic JSON schema generation *)
let test_basic_json_schema () =
  let type_adapter = create_simple_func_adapter () in
  let schema = type_adapter.json_schema () in
  
  (match schema with
   | `Assoc fields ->
     (* Check basic properties *)
     let properties = List.assoc "properties" fields in
     (match properties with
      | `Assoc props ->
        check bool "schema has a property" true (List.mem_assoc "a" props || List.mem_assoc "x" props);
        
        (* Check x property type *)
        (match List.assoc_opt "x" props with
         | Some (`Assoc x_props) ->
           let x_type = List.assoc "type" x_props in
           check bool "x is integer" true (x_type = `String "integer")
         | _ -> ());
        
        (* Check y property with default *)
        (match List.assoc_opt "y" props with
         | Some (`Assoc y_props) ->
           let y_type = List.assoc "type" y_props in
           let y_default = List.assoc_opt "default" y_props in
           check bool "y is string" true (y_type = `String "string");
           check bool "y has default" true (y_default = Some (`String "default"))
         | _ -> ())
      | _ -> fail "Expected properties object");
     
     (* Check required array *)
     let required = List.assoc "required" fields in
     (match required with
      | `List req_list ->
        let has_x = List.exists (function `String "x" -> true | _ -> false) req_list in
        let has_y = List.exists (function `String "y" -> true | _ -> false) req_list in
        check bool "x is required" true has_x;
        check bool "y is not required" false has_y
      | _ -> fail "Expected required array")
   | _ -> fail "Expected schema object")

(** Test string vs integer handling *)
let test_str_vs_int () =
  let signature = {
    name = "str_vs_int_func";
    description = None;
    parameters = [
      create_parameter 
        ~name:"a" 
        ~type_:"string" 
        ~required:true 
        ();
      create_parameter 
        ~name:"b" 
        ~type_:"integer" 
        ~required:true 
        ();
    ];
    return_type = "string";
    is_async = false;
    is_static = false;
    is_method = false;
    is_class_method = false;
  } in
  
  let validate_and_call args =
    try
      let a_val = match List.assoc_opt "a" args with
        | Some (`String s) -> s
        | _ -> failwith "a parameter must be string"
      in
      let _b_val = match List.assoc_opt "b" args with
        | Some (`Int i) -> i
        | _ -> failwith "b parameter must be integer"
      in
      Ok (`String a_val)
    with
    | Failure msg -> Error msg
    | _ -> Error "String vs int validation failed"
  in
  
  let type_adapter = { signature; validate_and_call; json_schema = fun () -> `Null } in
  
  let result = type_adapter.validate_and_call [("a", `String "123"); ("b", `Int 123)] in
  (match result with
   | Ok (`String s) -> check string "string preserved as string" "123" s
   | _ -> fail "Expected string value to be preserved")

(** Test schema property validation *)
let test_schema_properties () =
  let type_adapter = create_simple_func_adapter () in
  let schema = type_adapter.json_schema () in
  
  check bool "schema is object" true (match schema with `Assoc _ -> true | _ -> false);
  
  (match schema with
   | `Assoc fields ->
     check bool "has type field" true (List.mem_assoc "type" fields);
     check bool "has properties field" true (List.mem_assoc "properties" fields);
     
     let schema_type = List.assoc "type" fields in
     check bool "type is object" true (schema_type = `String "object")
   | _ -> ())

(** Test parameter requirement validation *)
let test_parameter_requirements () =
  let type_adapter = create_simple_func_adapter () in
  
  (* Test with missing required parameter *)
  let result1 = type_adapter.validate_and_call [("y", `String "hello")] in
  (match result1 with
   | Error _ -> check bool "missing required param error" true true
   | Ok _ -> fail "Expected error for missing required parameter");
  
  (* Test with only required parameter *)
  let result2 = type_adapter.validate_and_call [("x", `Int 42)] in
  (match result2 with
   | Ok _ -> check bool "required param only success" true true
   | Error _ -> fail "Expected success with only required parameter")

(** Test function signature metadata *)
let test_function_signature_metadata () =
  let type_adapter = create_simple_func_adapter () in
  let signature = type_adapter.signature in
  
  check string "function name" "simple_func" signature.name;
  check string "return type" "string" signature.return_type;
  check bool "has description" true (Option.is_some signature.description);
  check int "parameter count" 2 (List.length signature.parameters);
  
  let x_param = List.find (fun p -> p.param_name = "x") signature.parameters in
  check string "x param type" "integer" x_param.param_type;
  check bool "x param required" true x_param.param_required;
  check bool "x param no default" true (Option.is_none x_param.param_default);
  
  let y_param = List.find (fun p -> p.param_name = "y") signature.parameters in
  check string "y param type" "string" y_param.param_type;
  check bool "y param optional" false y_param.param_required;
  check bool "y param has default" true (Option.is_some y_param.param_default)

(** Test edge cases and error conditions *)
let test_edge_cases () =
  let type_adapter = create_simple_func_adapter () in
  
  (* Test with empty arguments *)
  let result1 = type_adapter.validate_and_call [] in
  (match result1 with
   | Error _ -> check bool "empty args error" true true
   | Ok _ -> fail "Expected error for empty arguments");
  
  (* Test with extra arguments (should be ignored or handled gracefully) *)
  let result2 = type_adapter.validate_and_call [
    ("x", `Int 1);
    ("y", `String "hello");
    ("extra", `String "ignored")
  ] in
  (match result2 with
   | Ok _ -> check bool "extra args handled" true true
   | Error _ -> fail "Should handle extra arguments gracefully");
  
  (* Test with null values *)
  let result3 = type_adapter.validate_and_call [("x", `Null)] in
  (match result3 with
   | Error _ -> check bool "null value error" true true
   | Ok _ -> fail "Expected error for null value in required parameter")

(** Main test suite *)
let () =
  run "TypeAdapter Module Tests" [
    ("Function Validation", [
      test_case "Complex function runtime argument validation" `Quick test_complex_function_runtime_arg_validation_non_json;
      test_case "Missing annotation handling" `Quick test_missing_annotation;
      test_case "Parameter requirement validation" `Quick test_parameter_requirements;
    ]);
    ("Type Conversion", [
      test_case "String to complex type conversion" `Quick test_convert_str_to_complex_type;
      test_case "String vs integer handling" `Quick test_str_vs_int;
    ]);
    ("Schema Operations", [
      test_case "Parameter skipping/pruning" `Quick test_skip_names;
      test_case "Basic JSON schema generation" `Quick test_basic_json_schema;
      test_case "Schema property validation" `Quick test_schema_properties;
    ]);
    ("Function Metadata", [
      test_case "Lambda function equivalent" `Quick test_lambda_function;
      test_case "Function signature metadata" `Quick test_function_signature_metadata;
    ]);
    ("Edge Cases", [
      test_case "Edge cases and error conditions" `Quick test_edge_cases;
    ]);
  ] 