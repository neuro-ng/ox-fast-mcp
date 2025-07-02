open Alcotest
open Lwt.Syntax
open Utilities.Types
open Tools.Tool_manager

(** Test adding basic function tools *)
let test_basic_function () =
  Lwt_main.run (
    let manager = create_tool_manager () in
    
    let add_fn ctx args =
      let* () = Lwt.return_unit in
      match args with
      | `Assoc [("a", `Int a); ("b", `Int b)] ->
          Lwt.return [create_text_content (string_of_int (a + b))]
      | _ -> Lwt.fail (Invalid_argument "Expected a and b integers")
    in

    let tool = {
      name = "add";
      description = "Add two numbers";
      parameters = `Assoc [
        ("type", `String "object");
        ("properties", `Assoc [
          ("a", `Assoc [("type", `String "integer")]);
          ("b", `Assoc [("type", `String "integer")])
        ]);
        ("required", `List [`String "a"; `String "b"])
      ];
      handler = add_fn;
      enabled = true;
      tags = [];
    } in

    let* () = add_tool manager tool in
    let* retrieved = get_tool manager "add" in
    
    check string "tool name" "add" retrieved.name;
    check string "tool description" "Add two numbers" retrieved.description;
    check bool "tool enabled" true retrieved.enabled;
    
    (* Test tool execution *)
    let ctx = create_execution_context ~request_id:"test-123" () in
    let args = `Assoc [("a", `Int 1); ("b", `Int 2)] in
    let* result = retrieved.handler ctx args in
    
    check int "result count" 1 (List.length result);
    check bool "result is text" true (match List.hd result with Text _ -> true | _ -> false);
    
    Lwt.return_unit
  )

(** Test adding async function tools *)
let test_async_function () =
  Lwt_main.run (
    let manager = create_tool_manager () in
    
    let fetch_data ctx args =
      let* () = Lwt.return_unit in
      match args with
      | `Assoc [("url", `String url)] ->
          Lwt.return [create_text_content ("Data from " ^ url)]
      | _ -> Lwt.fail (Invalid_argument "Expected url string")
    in

    let tool = {
      name = "fetch_data";
      description = "Fetch data from URL";
      parameters = `Assoc [
        ("type", `String "object");
        ("properties", `Assoc [
          ("url", `Assoc [("type", `String "string")])
        ]);
        ("required", `List [`String "url"])
      ];
      handler = fetch_data;
      enabled = true;
      tags = [];
    } in

    let* () = add_tool manager tool in
    let* retrieved = get_tool manager "fetch_data" in
    
    check string "tool name" "fetch_data" retrieved.name;
    check string "tool description" "Fetch data from URL" retrieved.description;
    
    (* Test async execution *)
    let ctx = create_execution_context ~request_id:"test-123" () in
    let args = `Assoc [("url", `String "http://example.com")] in
    let* result = retrieved.handler ctx args in
    
    check int "result count" 1 (List.length result);
    check bool "result is text" true (match List.hd result with Text _ -> true | _ -> false);
    
    Lwt.return_unit
  )

(** Test tool tags *)
let test_tool_tags () =
  Lwt_main.run (
    let manager = create_tool_manager () in
    
    let math_tool ctx args =
      let* () = Lwt.return_unit in
      Lwt.return [create_text_content "42"]
    in

    let string_tool ctx args =
      let* () = Lwt.return_unit in
      Lwt.return [create_text_content "TEXT"]
    in

    let tool1 = {
      name = "math_tool";
      description = "A math tool";
      parameters = `Null;
      handler = math_tool;
      enabled = true;
      tags = ["math"];
    } in

    let tool2 = {
      name = "string_tool";
      description = "A string tool";
      parameters = `Null;
      handler = string_tool;
      enabled = true;
      tags = ["string"; "utility"];
    } in

    let* () = add_tool manager tool1 in
    let* () = add_tool manager tool2 in
    
    let* tools = get_tools manager in
    let math_tools = List.filter (fun t -> List.mem "math" t.tags) tools in
    let utility_tools = List.filter (fun t -> List.mem "utility" t.tags) tools in
    
    check int "math tools count" 1 (List.length math_tools);
    check int "utility tools count" 1 (List.length utility_tools);
    check string "math tool name" "math_tool" (List.hd math_tools).name;
    check string "utility tool name" "string_tool" (List.hd utility_tools).name;
    
    Lwt.return_unit
  )

(** Test duplicate tool handling *)
let test_duplicate_tool_handling () =
  Lwt_main.run (
    let manager = create_tool_manager ~duplicate_behavior:`Error () in
    
    let tool1 = {
      name = "test_tool";
      description = "Original tool";
      parameters = `Null;
      handler = (fun ctx args -> Lwt.return [create_text_content "original"]);
      enabled = true;
      tags = [];
    } in

    let tool2 = {
      name = "test_tool";
      description = "Duplicate tool";
      parameters = `Null;
      handler = (fun ctx args -> Lwt.return [create_text_content "duplicate"]);
      enabled = true;
      tags = [];
    } in

    let* () = add_tool manager tool1 in
    
    (* Should raise error for duplicate *)
    match%lwt add_tool manager tool2 with
    | exception Tool_error _ -> Lwt.return_unit
    | _ -> Lwt.fail (Failure "Expected Tool_error for duplicate tool")
  )

(** Test tool removal *)
let test_tool_removal () =
  Lwt_main.run (
    let manager = create_tool_manager () in
    
    let tool = {
      name = "removable";
      description = "Tool to remove";
      parameters = `Null;
      handler = (fun ctx args -> Lwt.return [create_text_content "test"]);
      enabled = true;
      tags = [];
    } in

    let* () = add_tool manager tool in
    let* _ = get_tool manager "removable" in (* Should succeed *)
    
    let* () = remove_tool manager "removable" in
    
    (* Should fail to get removed tool *)
    match%lwt get_tool manager "removable" with
    | exception Not_found -> Lwt.return_unit
    | _ -> Lwt.fail (Failure "Expected Not_found for removed tool")
  )

(** Test error handling *)
let test_error_handling () =
  Lwt_main.run (
    let manager = create_tool_manager () in
    
    let error_tool ctx args =
      let* () = Lwt.return_unit in
      Lwt.fail (Tool_error "Specific tool error")
    in

    let tool = {
      name = "error_tool";
      description = "Tool that raises error";
      parameters = `Null;
      handler = error_tool;
      enabled = true;
      tags = [];
    } in

    let* () = add_tool manager tool in
    let* retrieved = get_tool manager "error_tool" in
    
    let ctx = create_execution_context ~request_id:"test-123" () in
    
    (* Should propagate Tool_error *)
    match%lwt retrieved.handler ctx `Null with
    | exception Tool_error msg when msg = "Specific tool error" -> Lwt.return_unit
    | _ -> Lwt.fail (Failure "Expected Tool_error to propagate")
  )

(** Test callable object handling *)
let test_call_tool_callable_object () =
  Lwt_main.run (
    let manager = create_tool_manager () in
    
    let adder ctx args =
      let* () = Lwt.return_unit in
      match args with
      | `Assoc [("x", `Int x); ("y", `Int y)] ->
          Lwt.return [create_text_content (string_of_int (x + y))]
      | _ -> Lwt.fail (Invalid_argument "Expected x and y integers")
    in

    let tool = {
      name = "Adder";
      description = "Adds two numbers";
      parameters = `Assoc [
        ("type", `String "object");
        ("properties", `Assoc [
          ("x", `Assoc [("type", `String "integer")]);
          ("y", `Assoc [("type", `String "integer")])
        ]);
        ("required", `List [`String "x"; `String "y"])
      ];
      handler = adder;
      enabled = true;
      tags = [];
    } in

    let* () = add_tool manager tool in
    let* result = call_tool manager "Adder" (`Assoc [("x", `Int 1); ("y", `Int 2)]) in
    
    match result with
    | [Text t] -> check string "result" "3" t; Lwt.return_unit
    | _ -> Lwt.fail (Failure "Expected text result")
  )

(** Test tool with default arguments *)
let test_call_tool_with_default_args () =
  Lwt_main.run (
    let manager = create_tool_manager () in
    
    let add ctx args =
      let* () = Lwt.return_unit in
      match args with
      | `Assoc [("a", `Int a); ("b", `Int b)] ->
          Lwt.return [create_text_content (string_of_int (a + b))]
      | `Assoc [("a", `Int a)] ->
          Lwt.return [create_text_content (string_of_int (a + 1))]  (* default b=1 *)
      | _ -> Lwt.fail (Invalid_argument "Expected a and optional b integers")
    in

    let tool = {
      name = "add";
      description = "Add two numbers";
      parameters = `Assoc [
        ("type", `String "object");
        ("properties", `Assoc [
          ("a", `Assoc [("type", `String "integer")]);
          ("b", `Assoc [
            ("type", `String "integer");
            ("default", `Int 1)
          ])
        ]);
        ("required", `List [`String "a"])
      ];
      handler = add;
      enabled = true;
      tags = [];
    } in

    let* () = add_tool manager tool in
    let* result = call_tool manager "add" (`Assoc [("a", `Int 1)]) in
    
    match result with
    | [Text t] -> check string "result" "2" t; Lwt.return_unit
    | _ -> Lwt.fail (Failure "Expected text result")
  )

(** Test tool with list arguments *)
let test_call_tool_with_list_int_input () =
  Lwt_main.run (
    let manager = create_tool_manager () in
    
    let sum_vals ctx args =
      let* () = Lwt.return_unit in
      match args with
      | `Assoc [("vals", `List nums)] ->
          let sum = List.fold_left (fun acc n ->
            match n with
            | `Int x -> acc + x
            | _ -> acc
          ) 0 nums in
          Lwt.return [create_text_content (string_of_int sum)]
      | _ -> Lwt.fail (Invalid_argument "Expected list of integers")
    in

    let tool = {
      name = "sum_vals";
      description = "Sum a list of integers";
      parameters = `Assoc [
        ("type", `String "object");
        ("properties", `Assoc [
          ("vals", `Assoc [
            ("type", `String "array");
            ("items", `Assoc [("type", `String "integer")])
          ])
        ]);
        ("required", `List [`String "vals"])
      ];
      handler = sum_vals;
      enabled = true;
      tags = [];
    } in

    let* () = add_tool manager tool in
    let* result = call_tool manager "sum_vals" 
      (`Assoc [("vals", `List [`Int 1; `Int 2; `Int 3])]) in
    
    match result with
    | [Text t] -> check string "result" "6" t; Lwt.return_unit
    | _ -> Lwt.fail (Failure "Expected text result")
  )

(** Test tool with list or string input *)
let test_call_tool_with_list_str_or_str_input () =
  Lwt_main.run (
    let manager = create_tool_manager () in
    
    let concat_strs ctx args =
      let* () = Lwt.return_unit in
      match args with
      | `Assoc [("vals", `List strs)] ->
          let concat = List.fold_left (fun acc s ->
            match s with
            | `String str -> acc ^ str
            | _ -> acc
          ) "" strs in
          Lwt.return [create_text_content concat]
      | `Assoc [("vals", `String str)] ->
          Lwt.return [create_text_content str]
      | _ -> Lwt.fail (Invalid_argument "Expected list of strings or string")
    in

    let tool = {
      name = "concat_strs";
      description = "Concatenate strings";
      parameters = `Assoc [
        ("type", `String "object");
        ("properties", `Assoc [
          ("vals", `Assoc [
            ("oneOf", `List [
              `Assoc [("type", `String "array"); 
                      ("items", `Assoc [("type", `String "string")])];
              `Assoc [("type", `String "string")]
            ])
          ])
        ]);
        ("required", `List [`String "vals"])
      ];
      handler = concat_strs;
      enabled = true;
      tags = [];
    } in

    let* () = add_tool manager tool in
    
    (* Test with list *)
    let* result1 = call_tool manager "concat_strs" 
      (`Assoc [("vals", `List [`String "a"; `String "b"; `String "c"])]) in
    
    match result1 with
    | [Text t1] -> 
        check string "list result" "abc" t1;
        
        (* Test with single string *)
        let* result2 = call_tool manager "concat_strs" 
          (`Assoc [("vals", `String "a")]) in
        
        match result2 with
        | [Text t2] -> check string "string result" "a" t2; Lwt.return_unit
        | _ -> Lwt.fail (Failure "Expected text result for string input")
    | _ -> Lwt.fail (Failure "Expected text result for list input")
  )

(** Test warning on duplicate tools *)
let test_warn_on_duplicate_tools () =
  Lwt_main.run (
    let manager = create_tool_manager ~duplicate_behavior:`Warn () in
    
    let test_fn ctx args =
      let* () = Lwt.return_unit in
      Lwt.return [create_text_content "test"]
    in

    let tool1 = {
      name = "test_tool";
      description = "Test tool";
      parameters = `Null;
      handler = test_fn;
      enabled = true;
      tags = [];
    } in

    let tool2 = {
      name = "test_tool";
      description = "Test tool duplicate";
      parameters = `Null;
      handler = test_fn;
      enabled = true;
      tags = [];
    } in

    let* () = add_tool manager tool1 in
    let* () = add_tool manager tool2 in
    
    (* Tool should still be accessible *)
    let* tool = get_tool manager "test_tool" in
    check string "tool exists" "test_tool" tool.name;
    
    Lwt.return_unit
  )

(** Test replacing duplicate tools *)
let test_replace_duplicate_tools () =
  Lwt_main.run (
    let manager = create_tool_manager ~duplicate_behavior:`Replace () in
    
    let original_fn ctx args =
      let* () = Lwt.return_unit in
      Lwt.return [create_text_content "original"]
    in

    let replacement_fn ctx args =
      let* () = Lwt.return_unit in
      Lwt.return [create_text_content "replacement"]
    in

    let tool1 = {
      name = "test_tool";
      description = "Original tool";
      parameters = `Null;
      handler = original_fn;
      enabled = true;
      tags = [];
    } in

    let tool2 = {
      name = "test_tool";
      description = "Replacement tool";
      parameters = `Null;
      handler = replacement_fn;
      enabled = true;
      tags = [];
    } in

    let* () = add_tool manager tool1 in
    let* () = add_tool manager tool2 in
    
    (* Should get replacement result *)
    let* result = call_tool manager "test_tool" `Null in
    
    match result with
    | [Text t] -> check string "result" "replacement" t; Lwt.return_unit
    | _ -> Lwt.fail (Failure "Expected text result")
  )

(** Test ignoring duplicate tools *)
let test_ignore_duplicate_tools () =
  Lwt_main.run (
    let manager = create_tool_manager ~duplicate_behavior:`Ignore () in
    
    let original_fn ctx args =
      let* () = Lwt.return_unit in
      Lwt.return [create_text_content "original"]
    in

    let replacement_fn ctx args =
      let* () = Lwt.return_unit in
      Lwt.return [create_text_content "replacement"]
    in

    let tool1 = {
      name = "test_tool";
      description = "Original tool";
      parameters = `Null;
      handler = original_fn;
      enabled = true;
      tags = [];
    } in

    let tool2 = {
      name = "test_tool";
      description = "Ignored tool";
      parameters = `Null;
      handler = replacement_fn;
      enabled = true;
      tags = [];
    } in

    let* () = add_tool manager tool1 in
    let* () = add_tool manager tool2 in
    
    (* Should get original result *)
    let* result = call_tool manager "test_tool" `Null in
    
    match result with
    | [Text t] -> check string "result" "original" t; Lwt.return_unit
    | _ -> Lwt.fail (Failure "Expected text result")
  )

(** Test custom tool names *)
let test_add_tool_with_custom_name () =
  Lwt_main.run (
    let manager = create_tool_manager () in
    
    let original_fn ctx args =
      let* () = Lwt.return_unit in
      match args with
      | `Assoc [("x", `Int x)] ->
          Lwt.return [create_text_content (string_of_int (x * 2))]
      | _ -> Lwt.fail (Invalid_argument "Expected x integer")
    in

    let tool = {
      name = "custom_name";
      description = "A tool with custom name";
      parameters = `Assoc [
        ("type", `String "object");
        ("properties", `Assoc [
          ("x", `Assoc [("type", `String "integer")])
        ]);
        ("required", `List [`String "x"])
      ];
      handler = original_fn;
      enabled = true;
      tags = [];
    } in

    let* () = add_tool manager tool in
    let* stored = get_tool manager "custom_name" in
    
    check string "tool name" "custom_name" stored.name;
    
    (* Should not be accessible via original name *)
    match%lwt get_tool manager "original_fn" with
    | exception Not_found -> Lwt.return_unit
    | _ -> Lwt.fail (Failure "Tool should not be accessible via original name")
  )

(** Test tool with image return *)
let test_tool_with_image_return () =
  Lwt_main.run (
    let manager = create_tool_manager () in
    
    let image_tool ctx args =
      let* () = Lwt.return_unit in
      match args with
      | `Assoc [("data", `String data)] ->
          Lwt.return [create_image_content ~data:"test data" ()]
      | _ -> Lwt.fail (Invalid_argument "Expected data string")
    in

    let tool = {
      name = "image_tool";
      description = "Tool that returns an image";
      parameters = `Assoc [
        ("type", `String "object");
        ("properties", `Assoc [
          ("data", `Assoc [("type", `String "string")])
        ]);
        ("required", `List [`String "data"])
      ];
      handler = image_tool;
      enabled = true;
      tags = [];
    } in

    let* () = add_tool manager tool in
    let* result = call_tool manager "image_tool" (`Assoc [("data", `String "test.png")]) in
    
    match result with
    | [Image _] -> Lwt.return_unit
    | _ -> Lwt.fail (Failure "Expected image result")
  )

(** Test context parameter handling *)
let test_context_parameter_handling () =
  Lwt_main.run (
    let manager = create_tool_manager () in
    
    let tool_with_context ctx args =
      let* () = Lwt.return_unit in
      match args with
      | `Assoc [("x", `Int x)] ->
          (* Verify context is valid *)
          match ctx.request_id with
          | Some _ -> Lwt.return [create_text_content (string_of_int x)]
          | None -> Lwt.fail (Invalid_argument "Context missing request_id")
      | _ -> Lwt.fail (Invalid_argument "Expected x integer")
    in

    let tool = {
      name = "context_tool";
      description = "Tool that uses context";
      parameters = `Assoc [
        ("type", `String "object");
        ("properties", `Assoc [
          ("x", `Assoc [("type", `String "integer")])
        ]);
        ("required", `List [`String "x"])
      ];
      handler = tool_with_context;
      enabled = true;
      tags = [];
    } in

    let* () = add_tool manager tool in
    
    (* Create context with request_id *)
    let ctx = create_execution_context ~request_id:"test-123" () in
    let* result = tool.handler ctx (`Assoc [("x", `Int 42)]) in
    
    match result with
    | [Text t] -> check string "result" "42" t; Lwt.return_unit
    | _ -> Lwt.fail (Failure "Expected text result")
  )

(** Test error handling with masked errors *)
let test_masked_error_handling () =
  Lwt_main.run (
    let manager = create_tool_manager ~mask_error_details:true () in
    
    let error_tool ctx args =
      let* () = Lwt.return_unit in
      Lwt.fail (Invalid_argument "Internal error details")
    in

    let tool = {
      name = "error_tool";
      description = "Tool that raises error";
      parameters = `Null;
      handler = error_tool;
      enabled = true;
      tags = [];
    } in

    let* () = add_tool manager tool in
    
    (* Should get generic error without details *)
    match%lwt call_tool manager "error_tool" `Null with
    | exception Tool_error msg ->
        check bool "error message masked" true 
          (String.contains msg "Error calling tool" && 
           not (String.contains msg "Internal error details"));
        Lwt.return_unit
    | _ -> Lwt.fail (Failure "Expected Tool_error")
  )

(** Test tool schema handling *)
let test_tool_schema () =
  Lwt_main.run (
    let manager = create_tool_manager () in
    
    let tool = {
      name = "schema_test";
      description = "Tool with complex schema";
      parameters = `Assoc [
        ("type", `String "object");
        ("properties", `Assoc [
          ("required_param", `Assoc [("type", `String "string")]);
          ("optional_param", `Assoc [
            ("type", `String "integer");
            ("default", `Int 42)
          ]);
          ("array_param", `Assoc [
            ("type", `String "array");
            ("items", `Assoc [("type", `String "string")])
          ])
        ]);
        ("required", `List [`String "required_param"])
      ];
      handler = (fun ctx args -> Lwt.return [create_text_content "test"]);
      enabled = true;
      tags = [];
    } in

    let* () = add_tool manager tool in
    let* retrieved = get_tool manager "schema_test" in
    
    (* Verify schema structure *)
    match retrieved.parameters with
    | `Assoc [("type", `String "object"); ("properties", `Assoc props); ("required", `List req)] ->
        check int "property count" 3 (List.length props);
        check int "required count" 1 (List.length req);
        Lwt.return_unit
    | _ -> Lwt.fail (Failure "Invalid schema structure")
  )

(** Test complex model handling *)
let test_call_tool_with_complex_model () =
  Lwt_main.run (
    let manager = create_tool_manager () in
    
    let name_shrimp ctx args =
      let* () = Lwt.return_unit in
      match args with
      | `Assoc [("tank", `Assoc [("x", `Null); ("shrimp", `List shrimps)])] ->
          let names = List.fold_left (fun acc s ->
            match s with
            | `Assoc [("name", `String name)] -> name :: acc
            | _ -> acc
          ) [] shrimps in
          let names = List.rev names in
          Lwt.return [create_text_content (Yojson.Safe.to_string (`List (List.map (fun n -> `String n) names)))]
      | _ -> Lwt.fail (Invalid_argument "Expected tank with shrimp list")
    in

    let tool = {
      name = "name_shrimp";
      description = "List shrimp names";
      parameters = `Assoc [
        ("type", `String "object");
        ("properties", `Assoc [
          ("tank", `Assoc [
            ("type", `String "object");
            ("properties", `Assoc [
              ("x", `Assoc [("type", `String "null")]);
              ("shrimp", `Assoc [
                ("type", `String "array");
                ("items", `Assoc [
                  ("type", `String "object");
                  ("properties", `Assoc [
                    ("name", `Assoc [("type", `String "string")])
                  ]);
                  ("required", `List [`String "name"])
                ])
              ])
            ]);
            ("required", `List [`String "x"; `String "shrimp"])
          ])
        ]);
        ("required", `List [`String "tank"])
      ];
      handler = name_shrimp;
      enabled = true;
      tags = [];
    } in

    let* () = add_tool manager tool in
    let* result = call_tool manager "name_shrimp" 
      (`Assoc [("tank", `Assoc [
        ("x", `Null);
        ("shrimp", `List [
          `Assoc [("name", `String "rex")];
          `Assoc [("name", `String "gertrude")]
        ])
      ])]) in
    
    match result with
    | [Text t] -> 
        check string "result" "[\n  \"rex\",\n  \"gertrude\"\n]" t;
        Lwt.return_unit
    | _ -> Lwt.fail (Failure "Expected text result")
  )

(** Test custom serializer *)
let test_custom_serializer () =
  Lwt_main.run (
    let manager = create_tool_manager ~serializer:(fun data ->
      match data with
      | `Assoc _ -> "CUSTOM:" ^ Yojson.Safe.to_string data
      | _ -> Yojson.Safe.to_string data
    ) () in
    
    let get_data ctx args =
      let* () = Lwt.return_unit in
      Lwt.return [create_text_content (Yojson.Safe.to_string 
        (`Assoc [
          ("key", `String "value");
          ("number", `Int 123)
        ]))]
    in

    let tool = {
      name = "get_data";
      description = "Get data with custom serialization";
      parameters = `Null;
      handler = get_data;
      enabled = true;
      tags = [];
    } in

    let* () = add_tool manager tool in
    let* result = call_tool manager "get_data" `Null in
    
    match result with
    | [Text t] -> 
        check string "result" "CUSTOM:{\"key\":\"value\",\"number\":123}" t;
        Lwt.return_unit
    | _ -> Lwt.fail (Failure "Expected text result")
  )

(** Test custom serializer with list result *)
let test_custom_serializer_with_list () =
  Lwt_main.run (
    let manager = create_tool_manager ~serializer:(fun data ->
      match data with
      | `List _ -> "CUSTOM:" ^ Yojson.Safe.to_string data
      | _ -> Yojson.Safe.to_string data
    ) () in
    
    let get_data ctx args =
      let* () = Lwt.return_unit in
      Lwt.return [create_text_content (Yojson.Safe.to_string 
        (`List [
          `Assoc [("key", `String "value"); ("number", `Int 123)];
          `Assoc [("key", `String "value2"); ("number", `Int 456)]
        ]))]
    in

    let tool = {
      name = "get_data";
      description = "Get list data with custom serialization";
      parameters = `Null;
      handler = get_data;
      enabled = true;
      tags = [];
    } in

    let* () = add_tool manager tool in
    let* result = call_tool manager "get_data" `Null in
    
    match result with
    | [Text t] -> 
        check string "result" "CUSTOM:[{\"key\":\"value\",\"number\":123},{\"key\":\"value2\",\"number\":456}]" t;
        Lwt.return_unit
    | _ -> Lwt.fail (Failure "Expected text result")
  )

(** Test custom serializer fallback *)
let test_custom_serializer_fallback () =
  Lwt_main.run (
    let manager = create_tool_manager ~serializer:(fun data ->
      (* Simulate broken serializer *)
      raise (Invalid_argument "Serializer error")
    ) () in
    
    let get_data ctx args =
      let* () = Lwt.return_unit in
      Lwt.return [create_text_content (Yojson.Safe.to_string (`String "test"))]
    in

    let tool = {
      name = "get_data";
      description = "Get data with failing serializer";
      parameters = `Null;
      handler = get_data;
      enabled = true;
      tags = [];
    } in

    let* () = add_tool manager tool in
    let* result = call_tool manager "get_data" `Null in
    
    match result with
    | [Text t] -> 
        (* Should fall back to default serialization *)
        check string "result" "\"test\"" t;
        Lwt.return_unit
    | _ -> Lwt.fail (Failure "Expected text result")
  )

(** Test async tool error handling *)
let test_async_tool_error_passthrough () =
  Lwt_main.run (
    let manager = create_tool_manager () in
    
    let async_error_tool ctx args =
      let* () = Lwt.return_unit in
      Lwt.fail (Tool_error "Async tool error")
    in

    let tool = {
      name = "async_error_tool";
      description = "Async tool that raises a ToolError";
      parameters = `Assoc [
        ("type", `String "object");
        ("properties", `Assoc [
          ("x", `Assoc [("type", `String "integer")])
        ]);
        ("required", `List [`String "x"])
      ];
      handler = async_error_tool;
      enabled = true;
      tags = [];
    } in

    let* () = add_tool manager tool in
    
    (* Should propagate Tool_error directly *)
    match%lwt call_tool manager "async_error_tool" (`Assoc [("x", `Int 42)]) with
    | exception Tool_error msg when msg = "Async tool error" -> Lwt.return_unit
    | _ -> Lwt.fail (Failure "Expected Tool_error to propagate")
  )

(** Test async exception converted to tool error with details *)
let test_async_exception_with_details () =
  Lwt_main.run (
    let manager = create_tool_manager () in
    
    let async_buggy_tool ctx args =
      let* () = Lwt.return_unit in
      Lwt.fail (Invalid_argument "Internal async error details")
    in

    let tool = {
      name = "async_buggy_tool";
      description = "Async tool that raises a ValueError";
      parameters = `Assoc [
        ("type", `String "object");
        ("properties", `Assoc [
          ("x", `Assoc [("type", `String "integer")])
        ]);
        ("required", `List [`String "x"])
      ];
      handler = async_buggy_tool;
      enabled = true;
      tags = [];
    } in

    let* () = add_tool manager tool in
    
    (* Should include both tool name and error details *)
    match%lwt call_tool manager "async_buggy_tool" (`Assoc [("x", `Int 42)]) with
    | exception Tool_error msg ->
        check bool "error message contains tool name" true
          (String.contains msg "Error calling tool 'async_buggy_tool'");
        check bool "error message contains details" true
          (String.contains msg "Internal async error details");
        Lwt.return_unit
    | _ -> Lwt.fail (Failure "Expected Tool_error")
  )

(** Test async exception converted to masked tool error *)
let test_async_exception_masked () =
  Lwt_main.run (
    let manager = create_tool_manager ~mask_error_details:true () in
    
    let async_buggy_tool ctx args =
      let* () = Lwt.return_unit in
      Lwt.fail (Invalid_argument "Internal async error details")
    in

    let tool = {
      name = "async_buggy_tool";
      description = "Async tool that raises a ValueError";
      parameters = `Assoc [
        ("type", `String "object");
        ("properties", `Assoc [
          ("x", `Assoc [("type", `String "integer")])
        ]);
        ("required", `List [`String "x"])
      ];
      handler = async_buggy_tool;
      enabled = true;
      tags = [];
    } in

    let* () = add_tool manager tool in
    
    (* Should only include tool name, not error details *)
    match%lwt call_tool manager "async_buggy_tool" (`Assoc [("x", `Int 42)]) with
    | exception Tool_error msg ->
        check bool "error message contains tool name" true
          (String.contains msg "Error calling tool 'async_buggy_tool'");
        check bool "error message masks details" false
          (String.contains msg "Internal async error details");
        Lwt.return_unit
    | _ -> Lwt.fail (Failure "Expected Tool_error")
  )

let add_tools_suite = [
  "Add Tools", [
    test_case "Basic function" `Quick (fun () -> test_basic_function ());
    test_case "Async function" `Quick (fun () -> test_async_function ());
    test_case "Callable object" `Quick (fun () -> test_call_tool_callable_object ());
    test_case "Tool with image return" `Quick (fun () -> test_tool_with_image_return ());
    test_case "Tool removal" `Quick (fun () -> test_tool_removal ());
    test_case "Warn on duplicates" `Quick (fun () -> test_warn_on_duplicate_tools ());
    test_case "Replace duplicates" `Quick (fun () -> test_replace_duplicate_tools ());
    test_case "Ignore duplicates" `Quick (fun () -> test_ignore_duplicate_tools ());
  ];
]

let tool_tags_suite = [
  "Tool Tags", [
    test_case "Tool tags" `Quick (fun () -> test_tool_tags ());
  ];
]

let call_tools_suite = [
  "Call Tools", [
    test_case "Default arguments" `Quick (fun () -> test_call_tool_with_default_args ());
    test_case "List int input" `Quick (fun () -> test_call_tool_with_list_int_input ());
    test_case "List or string input" `Quick (fun () -> test_call_tool_with_list_str_or_str_input ());
    test_case "Complex model" `Quick (fun () -> test_call_tool_with_complex_model ());
    test_case "Custom serializer" `Quick (fun () -> test_custom_serializer ());
    test_case "Custom serializer list" `Quick (fun () -> test_custom_serializer_with_list ());
    test_case "Serializer fallback" `Quick (fun () -> test_custom_serializer_fallback ());
  ];
]

let tool_schema_suite = [
  "Tool Schema", [
    test_case "Tool schema" `Quick (fun () -> test_tool_schema ());
  ];
]

let context_handling_suite = [
  "Context Handling", [
    test_case "Context handling" `Quick (fun () -> test_context_parameter_handling ());
  ];
]

let custom_tool_names_suite = [
  "Custom Tool Names", [
    test_case "Custom tool name" `Quick (fun () -> test_add_tool_with_custom_name ());
  ];
]

let tool_error_handling_suite = [
  "Tool Error Handling", [
    test_case "Error handling" `Quick (fun () -> test_error_handling ());
    test_case "Masked errors" `Quick (fun () -> test_masked_error_handling ());
    test_case "Async error passthrough" `Quick (fun () -> test_async_tool_error_passthrough ());
    test_case "Async error with details" `Quick (fun () -> test_async_exception_with_details ());
    test_case "Async error masked" `Quick (fun () -> test_async_exception_masked ());
  ];
]

let () = 
  run "Tool Manager Tests" (
    add_tools_suite @
    tool_tags_suite @
    call_tools_suite @
    tool_schema_suite @
    context_handling_suite @
    custom_tool_names_suite @
    tool_error_handling_suite
  ) 